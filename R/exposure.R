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

# ==============================================================================
# HEADWAY STATISTICAL ANALYSIS FUNCTIONS
# ==============================================================================


## Compute headways between vehicle passing events -- compute_vehicle_headways() ##
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

## Prepare Waiting for Gap lookup data -- prepare_wfg_events() ##
# Creates a lookup table of Waiting for Gap timestamps by site and date
# @param events Tibble with parsed timestamps and event types
# @return Tibble with wfg_ts_vec list column containing vectors of WFG timestamps
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

## Classify headways as Accepted, Rejected, or NotTracked -- classify_headways() ##
# Applies classification rules based on Waiting for Gap and Entering Roadway events
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

## Safe view function for data display -- safe_view() ##
# Attempts to open data in View window, falls back to print if not available
# @param data Tibble or data frame to display
# @param title Optional title for the view window
# @return Invisible NULL
safe_view <- function(data, title = NULL) {
  tryCatch({
    if (interactive() && capabilities("X11")) {
      utils::View(data, title = title)
    } else {
      cat("\n=== Preview:", title, "===\n")
      print(head(data, 10))
    }
  }, error = function(e) {
    cat("\n=== Preview:", title, "(View not available) ===\n")
    print(head(data, 10))
  })
  
  invisible(NULL)
}

## Export Raff overall metrics to CSV -- export_raff_overall_csv() ##
# Formats and exports overall Raff metrics to specified path
# @param raff_overall Tibble with overall Raff metrics
# @param output_path Path for CSV export
# @return Invisible NULL
export_raff_overall_csv <- function(raff_overall, 
                                    output_path = "/Library/CloudStorage/Box-Box/2024-tprs/output/Critical_headway_Raff_OVERALL.csv") {
  raff_overall_col <- raff_overall %>%
    tidyr::pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(
      # try to coerce to numeric and round to 3 decimals when possible
      value_num = suppressWarnings(as.numeric(value)),
      value = ifelse(!is.na(value_num), format(round(value_num, 3), nsmall = 3), as.character(value))
    ) %>%
    select(metric, value)
  
  readr::write_csv(raff_overall_col, output_path)
  cat("\n✓ Exported Raff overall metrics to:", output_path, "\n")
  
  invisible(NULL)
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


# ==============================================================================
# CDF CURVE GENERATION FUNCTIONS
# ==============================================================================

## Load observation data from CSV
##
## @return Tibble with site and spacing information
load_observation_data <- function() {
  read_csv("data/observation_data.csv", show_col_types = FALSE) %>%
    select(site, strip_spacing, spacing_type)
}

## Compute headways and join with spacing data
##
## @param cb Camera back data
## @param obs_data Observation data with spacing
## @return Tibble with headways and spacing joined
compute_headways_with_spacing <- function(cb, obs_data) {
  cb %>%
    arrange(site, date, time) %>%
    group_by(site, date) %>%
    mutate(headway_sec = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
    ungroup() %>%
    filter(!is.na(headway_sec), headway_sec > 0) %>%
    left_join(obs_data, by = "site")
}

## Create a CDF plot for a given data subset
##
## @param data Tibble with headway_sec column
## @param title Plot title
## @param t_c_critical_s Numeric value for critical headway
## @return ggplot object
create_cdf_plot <- function(data, title, t_c_critical_s) {
  # X-axis limit rule
  x_limit <- if (grepl("us191", title, ignore.case = TRUE)) 500 else 200
  
  # Calculate stats
  n <- nrow(data)
  mean_headway <- mean(data$headway_sec, na.rm = TRUE)
  sd_headway <- sd(data$headway_sec, na.rm = TRUE)
  
  # Filter data within x_limit for plotting
  plot_data <- data %>% filter(headway_sec <= x_limit)
  
  # Calculate CDF value at t_c using all data (not just filtered)
  cdf_at_tc <- if (!is.na(t_c_critical_s)) {
    sum(data$headway_sec <= t_c_critical_s, na.rm = TRUE) / nrow(data)
  } else {
    NA_real_
  }
  
  # Create CDF plot
  p <- ggplot(plot_data, aes(x = headway_sec)) +
    stat_ecdf(geom = "ribbon", aes(ymin = 0, ymax = after_stat(y)), fill = "steelblue", alpha = 0.5) +
    stat_ecdf(geom = "step", color = "steelblue", linewidth = 1) +
    scale_x_continuous(limits = c(0, x_limit)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      title = paste0("Cumulative Distribution Function - ", title),
      x = "Headway (seconds between cars)",
      y = "Cumulative Proportion"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
  
  # Stats annotation (bottom-right)
  fmt_val <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f s", x))
  stats_text <- paste0(
    "n = ", n, "\n",
    "mean = ", fmt_val(mean_headway), "\n",
    "sd = ", fmt_val(sd_headway)
  )
  
  p <- p +
    annotate(
      "label",
      x = x_limit * 0.95, y = 0.05,
      label = stats_text,
      vjust = 0, hjust = 1, size = 3.5,
      fill = "white", color = "black"
    )
  
  # Add critical headway horizontal line (shows % of headways below t_c)
  if (!is.na(t_c_critical_s) && !is.na(cdf_at_tc)) {
    p <- p +
      geom_hline(
        yintercept = cdf_at_tc,
        color = "red",
        linetype = "dashed",
        linewidth = 1
      ) +
      annotate(
        "text",
        x = x_limit * 0.95,
        y = cdf_at_tc,
        label = sprintf("CDF at t_c = %.1f s\n(%.1f%%)", t_c_critical_s, cdf_at_tc * 100),
        vjust = -0.5,
        hjust = 1,
        color = "red",
        size = 3.5,
        fontface = "bold"
      )
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

# ==============================================================================
# HISTOGRAM GENERATION FUNCTIONS
# ==============================================================================

## Process camera back data with combined grouping
##
## @param cb Camera back data
## @param obs_data Observation data with spacing
## @return Tibble with headways and combined groups
process_cb_with_combined_groups <- function(cb, obs_data) {
  cb %>%
    arrange(site, date, time) %>%
    group_by(site, date) %>%
    mutate(headway_sec = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
    ungroup() %>%
    filter(!is.na(headway_sec), headway_sec > 0) %>%
    left_join(obs_data, by = "site") %>%
    mutate(spacing_type = trimws(spacing_type))
}

## Build histogram statistics table
##
## @param data Tibble with headway data and grouping columns
## @return Tibble with summary statistics
build_histogram_stats_table <- function(data) {
  data %>%
    group_by(spacing_type) %>%
    summarise(
      n = n(),
      mean_hdwy = mean(headway_sec),
      sd_hdwy = sd(headway_sec),
      .groups = "drop"
    )
}

## Nest data with statistics for histogram generation
##
## @param data Tibble with headway data
## @param stats_table Tibble with summary statistics
## @return Nested tibble ready for plotting
nest_data_with_stats <- function(data, stats_table) {
  data %>%
    group_by(spacing_type) %>%
    nest() %>%
    left_join(stats_table, by = "spacing_type")
}

#######HdwyStatAnlys.R Functions Above#######

# ==============================================================================
# TARGETS PIPELINE FUNCTIONS
# ==============================================================================

## Perform headway statistical analysis for targets pipeline
##
## @param worker_exposure_data Worker exposure data from targets
## @return List containing classified data and raff metrics
make_headway_analysis <- function(worker_exposure_data) {
  we <- worker_exposure_data %>%
    mutate(
      across(where(is.character), trimws),
      ts = time,
      ts_display = format(time, "%Y-%m-%d %H:%M:%OS3")
    )
  
  # Compute headways (only for Vehicle Passing events)
  we_headways <- compute_vehicle_headways(we)
  
  wfg_lookup <- prepare_wfg_events(we) # Prepare lookups
  er_lookup <- prepare_er_events(we) # Prepare lookups
  we_classified <- classify_headways(we_headways, wfg_lookup, er_lookup) # Classify headways
  
  event_combos <- compute_event_combinations(we) # Event combinations uses ALL events (not just headways)
  hdwy_summary <- generate_headway_summary(we_classified) # Summary and Raff metrics use classified headways
  raff_results <- compute_all_raff_metrics(we_classified) # Summary and Raff metrics use classified headways
  
  list(
    worker_exposure_classified = we_classified,
    event_combinations = event_combos,
    headway_summary = hdwy_summary,
    raff_metrics = raff_results
  )
}

## Generate CDF plots for targets pipeline
##
## @param camera_back Camera back data from targets
## @param raff_metrics Raff metrics from headway analysis
## @return List of ggplot objects
make_cdf_plots <- function(camera_back, raff_metrics) {
  obs_data <- load_observation_data()
  hdwy_data <- compute_headways_with_spacing(camera_back, obs_data)
  
  t_c_critical_s <- raff_metrics %>%
    filter(grouping == "overall") %>%
    pull(t_c_critical_s)
  
  # Site plots
  sites <- c("sr12", "us6", "i70", "us191")
  site_plots <- lapply(sites, function(s) {
    site_data <- hdwy_data %>% filter(site == s)
    if (nrow(site_data) > 0) {
      create_cdf_plot(site_data, s, t_c_critical_s)
    } else {
      NULL
    }
  })
  names(site_plots) <- sites
  site_plots <- site_plots[!sapply(site_plots, is.null)]
  
  # Spacing type plots
  spacing_types <- c("NO TPRS", "UDOT", "PSS", "LONG")
  spacing_plots <- lapply(spacing_types, function(sp) {
    spacing_data <- hdwy_data %>% filter(spacing_type == sp)
    if (nrow(spacing_data) > 0) {
      create_cdf_plot(spacing_data, sp, t_c_critical_s)
    } else {
      NULL
    }
  })
  names(spacing_plots) <- spacing_types
  spacing_plots <- spacing_plots[!sapply(spacing_plots, is.null)]
  
  summary_stats <- generate_summary_stats(hdwy_data)
  
  list(
    site_plots = site_plots,
    spacing_plots = spacing_plots,
    summary_stats = summary_stats,
    headway_data = hdwy_data
  )
}

## Generate histogram plots for targets pipeline
##
## @param camera_back Camera back data from targets
## @param raff_metrics Raff metrics from headway analysis
## @return List of ggplot objects
make_histogram_plots <- function(camera_back, raff_metrics) {
  obs_data <- load_observation_data()
  cb_combined <- process_cb_with_combined_groups(camera_back, obs_data)
  
  t_c_critical_s <- raff_metrics %>%
    filter(grouping == "overall") %>%
    pull(t_c_critical_s)
  
  # Helper function to create histogram
  create_histogram <- function(data, title) {
    # Determine x-axis limit to keep outliers under 7
    # Start with base limit
    has_us191 <- any(data$site == "us191")
    base_limit <- if (has_us191) 500 else 200
    
    # Adjust limit to nearest hundred to keep outliers <= 7
    sorted_headways <- sort(data$headway_sec, decreasing = TRUE)
    if (length(sorted_headways) > 7) {
      # Find the 8th largest value (to keep 7 outliers)
      eighth_largest <- sorted_headways[8]
      # Round up to nearest hundred
      x_limit <- ceiling(eighth_largest / 100) * 100
      # Ensure it's at least the base_limit
      x_limit <- max(x_limit, base_limit)
    } else {
      x_limit <- base_limit
    }
    
    # Calculate stats
    n <- nrow(data)
    mean_headway <- mean(data$headway_sec, na.rm = TRUE)
    sd_headway <- sd(data$headway_sec, na.rm = TRUE)
    
    # Identify outliers
    outliers <- data %>%
      filter(headway_sec > x_limit) %>%
      pull(headway_sec) %>%
      round(1) %>%
      sort(decreasing = TRUE)
    
    # Create base plot
    p <- ggplot(data %>% filter(headway_sec <= x_limit), aes(x = headway_sec)) +
      geom_histogram(
        binwidth = 2,
        fill = "steelblue",
        alpha = 0.7,
        aes(y = after_stat(density)),
        na.rm = TRUE
      ) +
      geom_density(color = "steelblue", linewidth = 1, na.rm = TRUE) +
      scale_x_continuous(limits = c(0, x_limit)) +
      labs(
        title = paste0("Headway Distribution - ", title),
        x = "Headway (seconds between cars)",
        y = "Density"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14))
    
    # Stats annotation (top-right)
    fmt_val <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f s", x))
    stats_text <- paste0(
      "n = ", n, "\n",
      "mean = ", fmt_val(mean_headway), "\n",
      "sd = ", fmt_val(sd_headway)
    )
    
    p <- p +
      annotate(
        "label",
        x = x_limit * 0.95, y = Inf,
        label = stats_text,
        vjust = 1.1, hjust = 1, size = 3.5,
        fill = "white", color = "black", label.size = 0.2
      )
    
    # Outlier annotation if any exist
    if (length(outliers) > 0) {
      outlier_text <- paste(paste0(outliers, " s"), collapse = "\n")
      full_text <- paste0("Outliers (>", x_limit, " s):\n", outlier_text)
      
      p <- p +
        annotate(
          "label",
          x = x_limit * 0.70, y = Inf,
          label = full_text,
          vjust = 1.1, hjust = 0.5, size = 3.5,
          fill = "white", color = "black", label.size = 0.2
        )
    }
    
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
          y = Inf,
          label = sprintf("Critical headway\nt_c = %.1f s", t_c_critical_s),
          vjust = 1.5,
          hjust = ifelse(t_c_critical_s < x_limit * 0.5, -0.1, 1.1),
          color = "red",
          size = 3.5,
          fontface = "bold"
        )
    }
    
    return(p)
  }
  
  # Site plots
  sites <- c("sr12", "us6", "i70", "us191")
  site_plots <- lapply(sites, function(s) {
    site_data <- cb_combined %>% filter(site == s)
    if (nrow(site_data) > 0) {
      create_histogram(site_data, s)
    } else {
      NULL
    }
  })
  names(site_plots) <- sites
  site_plots <- site_plots[!sapply(site_plots, is.null)]
  
  # Spacing type plots
  spacing_types <- c("NO TPRS", "UDOT", "PSS", "LONG")
  spacing_plots <- lapply(spacing_types, function(sp) {
    spacing_data <- cb_combined %>% filter(spacing_type == sp)
    if (nrow(spacing_data) > 0) {
      create_histogram(spacing_data, sp)
    } else {
      NULL
    }
  })
  names(spacing_plots) <- spacing_types
  spacing_plots <- spacing_plots[!sapply(spacing_plots, is.null)]
  
  list(
    site_plots = site_plots,
    spacing_plots = spacing_plots
  )
}

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
  
  # Save site plots
  for (site_name in names(cdf_results$site_plots)) {
    filename <- file.path(output_dir, paste0("cdf_site_", site_name, ".svg"))
    ggsave(filename, plot = cdf_results$site_plots[[site_name]], 
           device = svglite, width = 10, height = 6)
    files <- c(files, filename)
  }
  
  # Save combined site plot
  if (length(cdf_results$site_plots) > 0) {
    combined_site <- wrap_plots(cdf_results$site_plots, ncol = 2, nrow = 2)
    combined_path <- file.path(output_dir, "cdf_sites_combined.svg")
    ggsave(combined_path, plot = combined_site, device = svglite, width = 16, height = 12)
    files <- c(files, combined_path)
  }
  
  # Save spacing plots
  for (spacing_name in names(cdf_results$spacing_plots)) {
    safe_name <- gsub(" ", "_", spacing_name)
    filename <- file.path(output_dir, paste0("cdf_spacing_", safe_name, ".svg"))
    ggsave(filename, plot = cdf_results$spacing_plots[[spacing_name]], 
           device = svglite, width = 10, height = 6)
    files <- c(files, filename)
  }
  
  # Save combined spacing plot
  if (length(cdf_results$spacing_plots) > 0) {
    combined_spacing <- wrap_plots(cdf_results$spacing_plots, ncol = 2, nrow = 2)
    combined_path <- file.path(output_dir, "cdf_spacing_combined.svg")
    ggsave(combined_path, plot = combined_spacing, device = svglite, width = 16, height = 12)
    files <- c(files, combined_path)
  }
  
  files
}

## Save histogram plots to files
##
## @param histogram_results List containing histogram plots
## @param camera_back Camera back data (for site names)
## @param output_dir Directory for saving plots
## @return Character vector of file paths
save_histogram_plots <- function(histogram_results, camera_back, output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  files <- c()
  
  # Save site plots
  for (site_name in names(histogram_results$site_plots)) {
    filename <- file.path(output_dir, paste0("histogram_site_", site_name, ".svg"))
    ggsave(filename, plot = histogram_results$site_plots[[site_name]], 
           device = svglite, width = 10, height = 6)
    files <- c(files, filename)
  }
  
  # Save combined site plot
  if (length(histogram_results$site_plots) > 0) {
    combined_site <- wrap_plots(histogram_results$site_plots, ncol = 2, nrow = 2)
    combined_path <- file.path(output_dir, "histogram_sites_combined.svg")
    ggsave(combined_path, plot = combined_site, device = svglite, width = 16, height = 12)
    files <- c(files, combined_path)
  }
  
  # Save spacing plots
  for (spacing_name in names(histogram_results$spacing_plots)) {
    safe_name <- gsub(" ", "_", spacing_name)
    filename <- file.path(output_dir, paste0("histogram_spacing_", safe_name, ".svg"))
    ggsave(filename, plot = histogram_results$spacing_plots[[spacing_name]], 
           device = svglite, width = 10, height = 6)
    files <- c(files, filename)
  }
  
  # Save combined spacing plot
  if (length(histogram_results$spacing_plots) > 0) {
    combined_spacing <- wrap_plots(histogram_results$spacing_plots, ncol = 2, nrow = 2)
    combined_path <- file.path(output_dir, "histogram_spacing_combined.svg")
    ggsave(combined_path, plot = combined_spacing, device = svglite, width = 16, height = 12)
    files <- c(files, combined_path)
  }
  
  files
}