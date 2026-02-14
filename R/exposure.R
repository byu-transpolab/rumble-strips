# Helper functions for making headway distributions and Work Exposure analysis.

#Used Libraries
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggrepel)   # for ggplot to position labels correctly
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

#' Plot the headway data as a cumulative distribution function
#' 
#' @param headway_data tibble with the all the headway information
#' @param critical_time dbl with the critical time in seconds
#' @param color_by str specifying how the plot should be colored
#' @return ggplot showing cdf curves of headway with critical time marked
plot_headway <- function(headway_data, critical_time, color_by) {
  
  # First, we compute where the critical time intersects with each CDF line
  # This is used to label the intersections for easier reading.
  ecdf_at_crit <- headway_data %>%
    mutate(.grp = .data[[color_by]]) %>%
    group_by(.grp) %>%
    summarise(
      # ECDF at x = critical_time
      p = mean(headway_sec <= critical_time, na.rm = TRUE),  
      .groups = "drop"
    ) %>%
    mutate(
      label = scales::percent(p, accuracy = 0.1)  # one decimal place
    )

  # Now we start building the plot.

  p <-ggplot(headway_data, aes(x=headway_sec, color = .data[[color_by]])) +
    # add cumulative distribution lines
    stat_ecdf() +
    # Add a vertical line to mark the critical time
    geom_vline(
      xintercept = critical_time, 
      linetype = "dashed", 
      linewidth = 0.6,
      color = "red"
    ) +
    # Add a label to the vertical line representing critical time
    annotate(
      "label",
      x = critical_time,
      y = 1.02,
      label = paste0("Critical Time: ", round(critical_time, 1), " s"),
      color = "red",
      fill = "white",
      vjust = -0.4,
      hjust = 1,
      size = 3.5,
      label.r = unit(0.1, "lines"),
      label.padding = unit(0.15, "lines")
    ) +
    # Fix the x_axis to 0-100 seconds without changing data
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 1), clip = "off") +
    # Make the why scale show percentage points
    scale_y_continuous(
      labels = scales::percent,
      # add a little headspace on the top for the critical time label
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      x = "Headway (s)",
      y = "Cumulative %",
      color = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold")
    ) +
    # Add labels to intersecting cdf and vertical line with white boxes
    geom_label_repel(
      data = ecdf_at_crit,
      aes(x = 0, y = p, label = label, color = .grp),
      inherit.aes = FALSE,
      direction = "y",            # only move vertically
      nudge_x = 0.6,              # small horizontal offset from the y-axis
      hjust = 0,                  # left-justify text inside the label
      box.padding = 0.15,
      point.padding = 0.1,
      label.size = 0.2,                    # border around the label
      label.r = unit(0.1, "lines"),        # slight rounded corners
      fill = "white",                      # white boxes
      show.legend = FALSE,
      seed = 123                           # reproducible place
    )
  
  return(p)
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