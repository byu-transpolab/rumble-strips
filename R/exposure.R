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

## find the Critical Time ######################################################


## Find the critical gap time workers need to adjust TPRS
##
## @param worker_exposure_data Worker exposure data from targets
## @return a dbl value representing the critical gap in seconds
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
  wfg_lookup <- prepare_wfg_events(we)
  er_lookup <- prepare_er_events(we)

   # Classify headways
  we_classified <- classify_headways(we_headways, wfg_lookup, er_lookup)
 
  # Summary and Raff metrics use classified headways
  raff_results <- compute_all_raff_metrics(we_classified)
  
return(raff_results$overall$t_c_critical_s)
}

## Helper Functions for find_critical_time() ##############################

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

prepare_wfg_events <- function(events) {
  wfg_events <- events %>%
    filter(event == "Waiting for Gap") %>%
    group_by(site, date) %>%
    summarise(wfg_ts_vec = list(ts), .groups = "drop")
  
  return(wfg_events)
}

## Prepare Entering Roadway lookup data -- prepare_er_events() ##
# Creates a lookup table of Entering Roadway timestamps by site and date
# @param events Tibble with parsed timestamps and event types
# @return Tibble with er_ts_vec list column containing vectors of ER timestamps
prepare_er_events <- function(events) {
  er_events <- events %>%
    filter(event == "Entering Roadway") %>%
    group_by(site, date) %>%
    summarise(er_ts_vec = list(ts), .groups = "drop")
  
  return(er_events)
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

## Compute event combination summaries -- compute_event_combinations() ##
# Analyzes sequences of key events (pairs and triplets)
# @param events Tibble with parsed timestamps and event types
# @return List containing pair_counts and triplet_counts tibbles
compute_event_combinations <- function(events) {
  key_events <- c("Waiting for Gap", "Entering Roadway", "Exiting Roadway")
  
  events_key <- events %>%
    filter(event %in% key_events) %>%
    group_by(site, date) %>%
    arrange(ts, .by_group = TRUE) %>%
    mutate(
      next_event1 = lead(event, 1),
      next_event2 = lead(event, 2),
      pair_label  = if_else(!is.na(next_event1),
                            paste(event, "→", next_event1),
                            NA_character_),
      triplet_label = if_else(!is.na(next_event2),
                              paste(event, "→", next_event1, "→", next_event2),
                              NA_character_)
    ) %>%
    ungroup()
  
  event_pair_counts <- events_key %>%
    filter(!is.na(pair_label)) %>%
    count(pair_label, sort = TRUE, name = "n")
  
  event_triplet_counts <- events_key %>%
    filter(!is.na(triplet_label)) %>%
    count(triplet_label, sort = TRUE, name = "n")
  
  return(list(
    pair_counts = event_pair_counts,
    triplet_counts = event_triplet_counts
  ))
}

## Generate headway summary by site and status -- generate_headway_summary() ##
# Summarizes headway counts and statistics by site and headway_status
# @param headways_couplets Tibble with classified headways
# @return Tibble with summary statistics by site and status
generate_headway_summary <- function(headways_couplets) {
  headway_summary <- headways_couplets %>%
    group_by(site, headway_status) %>%
    summarise(
      n = n(),
      mean_headway_s = mean(headway_s, na.rm = TRUE),
      median_headway_s = median(headway_s, na.rm = TRUE),
      min_headway_s = min(headway_s, na.rm = TRUE),
      max_headway_s = max(headway_s, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(headway_summary)
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

##Make CDF Plots #############################################################

# Generate CDF plots for targets pipeline
#
# @param camera_back Camera back data from targets
# @param raff_metrics Raff metrics from headway analysis
# @return List of ggplot objects
# Generate CDF plots for targets pipeline
#
# @param camera_back Camera back data from targets
# @param raff_metrics Raff metrics from headway analysis
# @return List of ggplot objects
make_cdf_plots <- function(camera_back_data, critical_time, observations) {
  hdwy_data <- compute_headways_with_spacing(camera_back_data, observations)
  
  t_c_critical_s <- critical_time
  
  # Define colors for sites and spacing types
  # Update this to pull the names from the inputs instead of fixing them here.
  site_colors <- c(
    "SR-12" = "#1f77b4",
    "US-6" = "#ff7f0e",
    "I-70" = "#2ca02c",
    "US-191" = "#d62728"
  )
  
  spacing_colors <- c(
    "NO TPRS" = "#1f77b4",
    "UDOT" = "#ff7f0e",
    "1:2" = "#2ca02c",
    "LONG" = "#d62728"
  )
  
  # Prepare site data list - using actual site names
  sites <- c("SR-12", "US-6", "I-70", "US-191")
  site_data_list <- lapply(sites, function(s) {
    hdwy_data %>% filter(site == s)
  })
  names(site_data_list) <- sites
  site_data_list <- site_data_list[sapply(site_data_list, nrow) > 0]
  
  # Prepare spacing data list - filter out NA values
  spacing_types <- c("NO TPRS", "UDOT", "1:2", "LONG")
  spacing_data_list <- lapply(spacing_types, function(sp) {
    hdwy_data %>% filter(!is.na(spacing_type), spacing_type == sp)
  })
  names(spacing_data_list) <- spacing_types
  spacing_data_list <- spacing_data_list[sapply(spacing_data_list, nrow) > 0]
  
  # Create combined plots
  site_plot <- if (length(site_data_list) > 0) {
    create_combined_cdf_plot(
      site_data_list,
      "CDF by Site",
      t_c_critical_s,
      site_colors
    )
  } else {
    NULL
  }
  
  spacing_plot <- if (length(spacing_data_list) > 0) {
    create_combined_cdf_plot(
      spacing_data_list,
      "CDF by Spacing Type",
      t_c_critical_s,
      spacing_colors
    )
  } else {
    NULL
  }
  
  summary_stats <- generate_summary_stats(hdwy_data)
  
  list(
    site_plot = site_plot,
    spacing_plot = spacing_plot,
    summary_stats = summary_stats,
    headway_data = hdwy_data
  )
}

## Helper Functions to make CDF Plots ######################################

## Compute headways and join with spacing data
##
## @param cb Camera back data
## @param obs_data Observation data with spacing
## @return Tibble with headways and spacing joined
compute_headways_with_spacing <- function(cb, obs_data) {
  cb %>%
    arrange(site, date, time) %>%
    group_by(site, date) %>%
    mutate(headway_sec = as.numeric(difftime(time, lag(time), units = "secs"))
    ) %>%
    ungroup() %>%
    filter(!is.na(headway_sec), headway_sec > 0) %>%
    left_join(obs_data, by = "site")
}

## Create a CDF plot for multiple data subsets (combined comparison)
##
## @param data_list Named list of tibbles with headway_sec column
## @param title Plot title
## @param t_c_critical_s Numeric value for critical headway
## @param colors Named vector of colors for each group
## @return ggplot object
create_combined_cdf_plot <- function(data_list, title, t_c_critical_s, colors = NULL) {
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
    x_vals <- unique(c(sorted_hdwy[sorted_hdwy <= x_limit], x_limit))
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
    if (!is.na(t_c_critical_s) && nrow(d) > 0) {
      cdf_val <- sum(d$headway_sec <= t_c_critical_s, na.rm = TRUE) / nrow(d)
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
      title = title,
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
  if (!is.na(t_c_critical_s) && t_c_critical_s <= x_limit) {
    p <- p +
      geom_vline(
        xintercept = t_c_critical_s,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = t_c_critical_s,
        y = 0.98,
        label = sprintf("t_c = %.1f s", t_c_critical_s),
        color = "red",
        size = 3.5,
        fontface = "bold",
        hjust = ifelse(t_c_critical_s < x_limit * 0.5, -0.1, 1.1)
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

## Generate summary statistics for headways
##
## @param hdwy_data Tibble with headway data
## @return Tibble with summary stats by site and spacing
generate_summary_stats <- function(hdwy_data) {
  hdwy_data %>%
    group_by(site, strip_spacing) %>%
    summarise(
      count = n(),
      mean_hdwy = mean(headway_sec),
      median_hdwy = median(headway_sec),
      sd_hdwy = sd(headway_sec),
      .groups = "drop"
    )
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
           device = svglite, width = 10, height = 7)
    files <- c(files, site_path)
  }
  
  # Save combined spacing plot
  if (!is.null(cdf_results$spacing_plot)) {
    spacing_path <- file.path(output_dir, "cdf_spacing.svg")
    ggsave(spacing_path, plot = cdf_results$spacing_plot, 
           device = svglite, width = 10, height = 7)
    files <- c(files, spacing_path)
  }
  
  files
}