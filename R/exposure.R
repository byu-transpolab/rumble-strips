# Helper functions for making headway distributions and Work Exposure analysis.

#Used Libraries
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(svglite)   # for ggsave(..., device = svglite)
library(readr)     # for reading observation_data.csv
library(patchwork) # for combining plots
library(targets)
library(stringr)
library(lubridate)

# Set option to display timestamps with millisecond precision (3 decimal places)
options(digits.secs = 3)

#' Find the critical gap time workers need to adjust TPRS
#
#' @param worker_exposure_data Worker exposure data from targets
#' @return a dbl value representing the critical gap in seconds
find_critical_time <- function(worker_exposure_data) {
  we <- worker_exposure_data %>%
    mutate(
      across(where(is.character), trimws),
      ts = time,
      ts_display = format(time, "%Y-%m-%d %H:%M:%OS3")
    )
  
  # Compute headways (only for Vehicle Passing events)
  we_headways <- compute_vehicle_headways(we)
  
   # Prepare lookups
  wfg_lookup <- we %>%
    filter(event == "Waiting for Gap") %>%
    group_by(site, date) %>%
    summarise(wfg_ts_vec = list(ts), .groups = "drop")

  er_lookup <- we %>%
    filter(event == "Entering Roadway") %>%
    group_by(site, date) %>%
    summarise(er_ts_vec = list(ts), .groups = "drop")

   # Classify headways
  we_classified <- classify_headways(we_headways, wfg_lookup, er_lookup)
 
  # Summary and Raff metrics use classified headways
  raff_results <- compute_all_raff_metrics(we_classified)
  
return(raff_results$overall$t_c_critical_s)
}

## Compute headways between vehicle passing events -- compute_vehicle_headways()
# Calculates time differences between consecutive Vehicle Passing events
# @param events Tibble with parsed timestamps and event types
# @return Tibble with headway calculations for Vehicle Passing events
compute_vehicle_headways <- function(events) {
  # First, ensure we have ts and ts_display columns
  if (!"ts" %in% names(events)) {
    events <- events %>%
      mutate(
        ts = time,
        ts_display = format(time, "%Y-%m-%d %H:%M:%OS3")
      )
  }
  vp <- events %>%
    filter(event == "Vehicle Passing") %>%
    group_by(site, date) %>%
    arrange(ts, .by_group = TRUE) %>%
    mutate(
      prev_vp_ts_numeric = lag(ts),
      prev_vp_ts_display = lag(ts_display),
      current_vp_ts_numeric = ts,
      current_vp_ts_display = ts_display,
      headway_s  = as.numeric(ts - prev_vp_ts_numeric, units = "secs")
    ) %>%
    ungroup()
  
  return(vp)
}

## Classify headways as Accepted, Rejected, or NotTracked -- classify_headways()
# Classifies based on Waiting for Gap and Entering Roadway events
# @param vehicle_headways Tibble with vehicle passing headways
# @param wfg_events Tibble with Waiting for Gap timestamps
# @param er_events Tibble with Entering Roadway timestamps
# @return Tibble with headway_status classification
classify_headways <- function(vehicle_headways, wfg_events, er_events) {
  # Helper: For each current VP, find the most recent Waiting for Gap before it
  vp_wfg <- vehicle_headways %>%
    left_join(wfg_events, by = c("site", "date")) %>%
    group_by(site, date) %>%
    arrange(current_vp_ts_numeric, .by_group = TRUE) %>%
    mutate(
      last_wfg_ts = {
        map2_dbl(current_vp_ts_numeric, wfg_ts_vec, ~ {
          if (is.null(.y) || length(.y) == 0) {
            NA_real_
          } else {
            prior_wfg <- .y[.y < .x]
            if (length(prior_wfg) == 0) NA_real_ else as.numeric(max(prior_wfg))
          }
        }) %>% as.POSIXct(origin = "1970-01-01", tz = "UTC")
      }
    ) %>%
    select(-wfg_ts_vec) %>%
    ungroup()
  
  # Classify based on ER events
  headways_all <- vp_wfg %>%
    left_join(er_events, by = c("site", "date")) %>%
    mutate(
      has_er = map_lgl(er_ts_vec, ~ !is.null(.x) && length(.x) > 0),
      # Define interval start depending on whether a WFG exists
      bound_start = case_when(
        !is.na(last_wfg_ts) & !is.na(prev_vp_ts_numeric) ~ as.POSIXct(pmax(as.numeric(prev_vp_ts_numeric),
                                                                   as.numeric(last_wfg_ts)),
                                                              origin = "1970-01-01", tz = "UTC"),
        !is.na(last_wfg_ts) &  is.na(prev_vp_ts_numeric) ~ last_wfg_ts,  # First VP after WFG
        is.na(last_wfg_ts)                        ~ prev_vp_ts_numeric   # No WFG
      ),
      # Check for ER strictly inside (bound_start, ts)
      er_in_interval = pmap_lgl(list(bound_start, current_vp_ts_numeric, er_ts_vec), ~ {
        if (is.null(..3) || length(..3) == 0) FALSE
        else any(..3 > ..1 & ..3 < ..2)
      }),
      headway_status = case_when(
        # No WFG prior: accept only if ER occurs between the VP pair; else NotTracked
        is.na(last_wfg_ts) & er_in_interval ~ "Accepted",
        is.na(last_wfg_ts) & !er_in_interval ~ "NotTracked",
        
        # WFG exists: accept if ER in (max(prev_vp_ts, last_wfg_ts), ts); else Rejected
        !is.na(last_wfg_ts) & er_in_interval ~ "Accepted",
        !is.na(last_wfg_ts) & !er_in_interval ~ "Rejected",
        
        TRUE ~ "NotTracked"
      )
    ) %>%
    select(
      site, date,
      prev_vp_ts = prev_vp_ts_display,
      current_vp_ts = current_vp_ts_display,
      last_wfg_ts,
      bound_start,
      headway_s,
      headway_status
    )
  
  # Keep only real "Vehicle Passing" couplets (i.e., previous VP exists)
  headways_couplets <- headways_all %>% filter(!is.na(prev_vp_ts))
  
  return(headways_couplets)
}

## Compute overall and grouped Raff metrics -- compute_all_raff_metrics() ##
# Calculates critical headway metrics overall, by site, and by site+date
# @param headways_couplets Tibble with classified headways
# @return List containing raff_overall, raff_by_site, and raff_by_site_date tibbles
compute_all_raff_metrics <- function(headways_couplets) {
  # Use only couplets and only Accepted/Rejected classifications
  gaps <- headways_couplets %>%
    filter(headway_status %in% c("Accepted", "Rejected")) %>%
    select(site, date, headway_s, headway_status)
  
  # Compute metrics overall (all sites/dates combined)
  raff_overall <- compute_raff_metrics(gaps)
  
  # Compute metrics by site
  raff_by_site <- gaps %>%
    group_by(site) %>%
    group_modify(~ compute_raff_metrics(.x)) %>%
    ungroup()
  
  # Compute metrics by site+date
  raff_by_site_date <- gaps %>%
    group_by(site, date) %>%
    group_modify(~ compute_raff_metrics(.x)) %>%
    ungroup()
  
  return(list(
    overall = raff_overall,
    by_site = raff_by_site,
    by_site_date = raff_by_site_date
  ))
}

## Compute Raff critical headway metrics -- compute_raff_metrics() ##
# Calculates t1, t2, acceptance/rejection rates, and critical headway (t_c) using Raff method
# @param df Tibble containing headway_s and headway_status (Accepted/Rejected only)
# @return Tibble with one row containing all Raff metrics
compute_raff_metrics <- function(df) {
  acc <- df %>% filter(headway_status == "Accepted") %>% pull(headway_s)
  rej <- df %>% filter(headway_status == "Rejected") %>% pull(headway_s)

  n_acc <- length(acc)
  n_rej <- length(rej)
  n_tot <- n_acc + n_rej

  # t1 = largest rejected; t2 = smallest accepted
  t1 <- if (n_rej > 0) max(rej, na.rm = TRUE) else NA_real_
  t2 <- if (n_acc > 0) min(acc, na.rm = TRUE) else NA_real_

  # A(t) and R(t) as proportions (0..1) within their own subsets
  A_t1 <- if (n_acc > 0 && !is.na(t1)) mean(acc <= t1) else NA_real_
  R_t1 <- if (n_rej > 0 && !is.na(t1)) mean(rej >= t1) else NA_real_
  A_t2 <- if (n_acc > 0 && !is.na(t2)) mean(acc <= t2) else NA_real_
  R_t2 <- if (n_rej > 0 && !is.na(t2)) mean(rej >= t2) else NA_real_

  # Overall acceptance/rejection rates for context
  A_overall <- if (n_tot > 0) n_acc / n_tot else NA_real_
  R_overall <- if (n_tot > 0) n_rej / n_tot else NA_real_

  # t_c per the provided formula:
  # t_c = ((t2 - t1) * [R(t1) - A(t1)]) / ([A(t2) - R(t2)] + [R(t1) - A(t1)]) + t1
  denom <- (A_t2 - R_t2) + (R_t1 - A_t1)
  tc <- if (is.na(t1) || is.na(t2) || is.na(denom) || denom == 0) NA_real_ else {
    ((t2 - t1) * (R_t1 - A_t1)) / denom + t1
  }

  tibble::tibble(
    n_accepted = n_acc,
    n_rejected = n_rej,
    t1_largest_rejected_s = t1,
    t2_smallest_accepted_s = t2,
    A_t1 = A_t1, R_t1 = R_t1,
    A_t2 = A_t2, R_t2 = R_t2,
    A_overall = A_overall,   # overall % accepted (0..1)
    R_overall = R_overall,   # overall % rejected (0..1)
    t_c_critical_s = tc
  )
}



#' Compute headways and join with spacing data
#'
#' @param camera_back_data Camera back data
#' @param observations Observation data with spacing
#' @return Tibble with headways and spacing joined
compute_headways <- function(camera_back_data, observations) {
  headway_data <- camera_back_data %>%
    arrange(site, date, time) %>%
    group_by(site, date) %>%
    mutate(headway_sec = as.numeric(difftime(time, lag(time), units = "secs"))
    ) %>%
    ungroup() %>%
    filter(!is.na(headway_sec), headway_sec > 0) %>%
    left_join(
      observations %>% select(date, spacing_type),
      by = "date")
}

#' Make a named list of headway tibbles grouped by site or spacing_type
#'
#' @param headway_data tibble. Headway rows (must contain `site` and `spacing_type`).
#' @param group_by_site logical. If TRUE (default) group by `site`; if FALSE
#'   group by `spacing_type` (NA spacing rows are dropped).
#' @return Named list of headway tibbles. List names are the group keys (site or spacing_type).
group_headway <- function(headway_data, group_by_site = TRUE) {
  # Determine grouping column, whether by site or spacing_type
  group_col <- if (group_by_site) "site" else "spacing_type"

  # 
  df <- headway_data
  if (!group_by_site) {
    df <- df %>% filter(!is.na(.data[[group_col]]))
  }

  # make a list of distinct sites or spacing types
  groups <- df %>% distinct(.data[[group_col]]) %>% pull(1)

  # make a list of tibbles.
  # each tibble containing headway data for one site or spacing type.
  grouped_headway_list <- map(groups, function(g) {
    df %>% filter(.data[[group_col]] == g)
  }) %>% set_names(groups)

  grouped_headway_list
}

#' Create a CDF plot for multiple data subsets (combined comparison)
#'
#' @param data_list Named list of tibbles with headway_sec column
#' @param title Plot title
#' @param critical_time Numeric value for critical headway
#' @param colors Named vector of colors for each group
#' @return ggplot object
create_combined_cdf_plot <- function(data_list, title, critical_time, colors = NULL) {
  # Default colors if not provided
  if (is.null(colors)) {
    colors <- setNames(
      c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
      names(data_list)[1:min(4, length(data_list))]
    )
  }
  
  # Set x-axis limit to 100 seconds
  x_limit <- 100
  
  # Manually calculate ECDF for each group using ALL data
  ecdf_data_list <- lapply(names(data_list), function(name) {
    d <- data_list[[name]]
    
    # Sort the headways
    sorted_hdwy <- sort(d$headway_sec)
    n_total <- length(sorted_hdwy)
    
    # Create ECDF points (only up to x_limit for plotting, but calculated from all data)
    # Include all unique values up to x_limit, plus ensure we have x_limit itself
    x_vals <- unique(sorted_hdwy)
    x_vals <- sort(x_vals)
    
    # Calculate cumulative proportion at each x value based on ALL data
    y_vals <- sapply(x_vals, function(x) {
      sum(sorted_hdwy <= x) / n_total
    })
    
    # Create tibble for plotting
    tibble(
      group = name,
      headway_sec = x_vals,
      cdf = y_vals
    )
  })
  
  # Combine all ECDF data
  ecdf_data <- bind_rows(ecdf_data_list)
  
  # Calculate CDF values at t_c for each group using ALL data (unfiltered)
  cdf_at_tc_list <- lapply(names(data_list), function(name) {
    d <- data_list[[name]]
    if (!is.na(critical_time) && nrow(d) > 0) {
      cdf_val <- sum(d$headway_sec <= critical_time, na.rm = TRUE) / nrow(d)
      tibble(group = name, cdf_at_tc = cdf_val)
    } else {
      tibble(group = name, cdf_at_tc = NA_real_)
    }
  })
  cdf_at_tc_df <- bind_rows(cdf_at_tc_list)
  
  # Sort by cdf_at_tc to position labels with spacing
  cdf_at_tc_df <- cdf_at_tc_df %>%
    arrange(cdf_at_tc) %>%
    filter(!is.na(cdf_at_tc))
  
  # Create vertical spacing for labels to prevent overlap
  # Minimum spacing of 0.05 (5%) between labels
  min_spacing <- 0.05
  if (nrow(cdf_at_tc_df) > 0) {
    cdf_at_tc_df$label_y <- cdf_at_tc_df$cdf_at_tc
    
    for (i in 2:nrow(cdf_at_tc_df)) {
      if (cdf_at_tc_df$label_y[i] - cdf_at_tc_df$label_y[i-1] < min_spacing) {
        cdf_at_tc_df$label_y[i] <- cdf_at_tc_df$label_y[i-1] + min_spacing
      }
    }
  }
  
  # Create CDF plot using manually calculated ECDF
  p <- ggplot(ecdf_data, aes(x = headway_sec, y = cdf, color = group)) +
    geom_step(linewidth = 1.2, direction = "hv") +
    scale_color_manual(values = colors, name = "") +
    scale_x_continuous(breaks = seq(0, x_limit, by = 10), limits = c(0, x_limit)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      x = "Headway (s)",
      y = "Cumulative %"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Add critical headway vertical line
  if (!is.na(critical_time) && critical_time <= x_limit) {
    p <- p +
      geom_vline(
        xintercept = critical_time,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = critical_time,
        y = 0.98,
        label = sprintf("t_c = %.1f s", critical_time),
        color = "red",
        size = 3.5,
        fontface = "bold",
        hjust = ifelse(critical_time < x_limit * 0.5, -0.1, 1.1)
      )
    
    # Add percentage labels with spacing (no horizontal lines)
    for (i in seq_len(nrow(cdf_at_tc_df))) {
      group_name <- cdf_at_tc_df$group[i]
      cdf_val <- cdf_at_tc_df$cdf_at_tc[i]
      label_y <- cdf_at_tc_df$label_y[i]
      
      if (!is.na(cdf_val)) {
        p <- p +
          annotate(
            "text",
            x = 2,
            y = label_y,
            label = sprintf("%.1f%%", cdf_val * 100),  # Changed to one decimal place
            color = colors[group_name],
            size = 3,
            fontface = "bold",
            hjust = 0,
            vjust = 0.5
          )
      }
    }
  }
  
  return(p)
}

## functions to save the CDF plots #######################################

## Save CDF plots to files
##
## @param cdf_results List containing CDF plots
## @param output_dir Directory for saving plots
## @return Character vector of file paths
save_cdf_plots <- function(cdf_results, output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  files <- c()
  
  # Save combined site plot
  if (!is.null(cdf_results$site_plot)) {
    site_path <- file.path(output_dir, "cdf_sites.svg")
    ggsave(site_path, plot = cdf_results$site_plot, 
           device = svglite, width = 6, height = 4)
    files <- c(files, site_path)
  }
  
  # Save combined spacing plot
  if (!is.null(cdf_results$spacing_plot)) {
    spacing_path <- file.path(output_dir, "cdf_spacing.svg")
    ggsave(spacing_path, plot = cdf_results$spacing_plot, 
           device = svglite, width = 6, height = 4)
    files <- c(files, spacing_path)
  }
  
  files
}


#' Build ECDF points for groups of headways
#'
#' @param grouped_headway Named list of tibbles. Each list element is a tibble
#'   containing a numeric column `headway_sec` (headway in seconds). List names
#'   are used as the `group` values in the output.
#' @return A tibble with columns:
#'   - `group` (character): name of the group (either sites or spacing types),
#'   - `headway_sec` (numeric): sorted, unique observed headway values,
#'   - `cdf` (numeric): empirical cumulative probability at each `headway_sec`.
#'   Rows for groups with no non-NA headways are omitted.
make_ecdf_data <- function(grouped_headway) {
  # interate through the list of tibbles, do some math, and put it together.
  imap_dfr(grouped_headway, function(tbl, name) {
    # sort the headway secs for this group
    hdwy <- tbl$headway_sec
    # filter out any NAs just in case, since we need to do math on these
    hdwy <- hdwy[!is.na(hdwy)]
    # find how many headways we have for this group
    n <- length(hdwy)
    # sort the headways for this group
    sorted <- sort(hdwy)
    # create ECDF points
    x_vals <- sort(unique(sorted))
    # calculate cumulative proportion at each x value
    y_vals <- vapply(x_vals, function(x) sum(sorted <= x) / n, numeric(1))
    # create a tibble for this group with the ECDF points
    tibble(group = name, headway_sec = x_vals, cdf = y_vals)
  })
  # imap takes all the individual tibbles andn binds them together.
}

# 2) Compute CDF values at the critical time for each group
compute_cdf_at_tc <- function(data_list, critical_time) {
  # data_list: named list of tibbles; each tibble must have headway_sec
  # critical_time: numeric (seconds) or NA
  if (is.na(critical_time)) {
    return(tibble::tibble(group = names(data_list), cdf_at_tc = NA_real_))
  }
  purrr::imap_dfr(data_list, function(tbl, name) {
    hdwy <- tbl$headway_sec
    hdwy <- hdwy[!is.na(hdwy)]
    if (length(hdwy) == 0) {
      tibble::tibble(group = name, cdf_at_tc = NA_real_)
    } else {
      tibble::tibble(group = name,
                     cdf_at_tc = sum(hdwy <= critical_time, na.rm = TRUE) / length(hdwy))
    }
  })
}

# 3) Plot ECDF points and annotate critical-time information
plot_ecdf_with_tc <- function(ecdf_data,
                              cdf_at_tc_df,
                              title = NULL,
                              critical_time = NA,
                              colors = NULL,
                              x_limit = 100) {
  # ecdf_data: tibble produced by make_ecdf_data (group, headway_sec, cdf)
  # cdf_at_tc_df: tibble produced by compute_cdf_at_tc (group, cdf_at_tc)
  # colors: named vector of colors (names must match groups in ecdf_data)
  groups <- unique(ecdf_data$group)
  if (is.null(colors)) {
    # default palette matching group order
    cols <- scales::hue_pal()(length(groups))
    colors <- setNames(cols, groups)
  } else {
    # ensure provided colors include all groups; fallback to hue for missing
    missing_groups <- setdiff(groups, names(colors))
    if (length(missing_groups) > 0) {
      extra_cols <- scales::hue_pal()(length(missing_groups))
      colors <- c(colors, setNames(extra_cols, missing_groups))
    }
  }

  p <- ggplot2::ggplot(ecdf_data, ggplot2::aes(x = headway_sec, y = cdf, color = group)) +
    ggplot2::geom_step(linewidth = 1.2, direction = "hv") +
    ggplot2::scale_color_manual(values = colors, name = "") +
    ggplot2::scale_x_continuous(breaks = seq(0, x_limit, by = 10), limits = c(0, x_limit)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(x = "Headway (s)", y = "Cumulative %", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank()
    )

  # add critical time vertical line + label if applicable
  if (!is.na(critical_time) && critical_time <= x_limit) {
    p <- p +
      ggplot2::geom_vline(xintercept = critical_time, color = "red", linetype = "dashed", linewidth = 1) +
      ggplot2::annotate("text",
                        x = critical_time,
                        y = 0.98,
                        label = sprintf("t_c = %.1f s", critical_time),
                        color = "red",
                        size = 3.5,
                        fontface = "bold",
                        hjust = ifelse(critical_time < x_limit * 0.5, -0.1, 1.1))
    # add percent labels at left with simple vertical spacing to avoid overlap
    cdf_at_tc_df <- cdf_at_tc_df %>% dplyr::filter(!is.na(cdf_at_tc))
    if (nrow(cdf_at_tc_df) > 0) {
      cdf_at_tc_df <- cdf_at_tc_df %>% dplyr::arrange(cdf_at_tc) %>% dplyr::mutate(label_y = cdf_at_tc)
      min_spacing <- 0.05
      for (i in seq_len(nrow(cdf_at_tc_df))) {
        if (i > 1 && (cdf_at_tc_df$label_y[i] - cdf_at_tc_df$label_y[i-1]) < min_spacing) {
          cdf_at_tc_df$label_y[i] <- cdf_at_tc_df$label_y[i-1] + min_spacing
        }
      }
      for (i in seq_len(nrow(cdf_at_tc_df))) {
        grp <- cdf_at_tc_df$group[i]
        lab_y <- cdf_at_tc_df$label_y[i]
        val <- cdf_at_tc_df$cdf_at_tc[i]
        p <- p + ggplot2::annotate("text",
                                   x = 2,
                                   y = lab_y,
                                   label = sprintf("%.1f%%", val * 100),
                                   color = colors[grp],
                                   size = 3,
                                   fontface = "bold",
                                   hjust = 0,
                                   vjust = 0.5)
      }
    }
  }

  p
}