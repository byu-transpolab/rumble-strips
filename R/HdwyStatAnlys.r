
# Packages ----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(stringr)
})

# Set option to display timestamps with millisecond precision (3 decimal places)
options(digits.secs = 3)

## Load and validate worker exposure data -- load_worker_exposure_data() ##
# Loads worker_exposure_data from targets pipeline and validates required columns
# @return Tibble containing validated worker exposure data
load_worker_exposure_data <- function() {
  # Load worker_exposure_data from targets pipeline if not already in environment
  if (!exists("worker_exposure_data")) {
    targets::tar_load(worker_exposure_data)
  } else {
    worker_exposure_data <- get("worker_exposure_data", envir = parent.frame())
  }
  
  # Validate expected columns
  required_cols <- c("site", "date", "time", "event")
  missing_cols <- setdiff(required_cols, names(worker_exposure_data))
  if (length(missing_cols) > 0) {
    stop("worker_exposure_data is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  return(worker_exposure_data)
}

## Parse timestamps with milliseconds -- parse_timestamps() ##
# Ensures timestamps are POSIXct and orders by timestamp
# @param exposure_data Tibble with site, date, time (POSIXct), and event columns
# @return Tibble with validated timestamps (ts) and formatted display timestamps (ts_display)
parse_timestamps <- function(exposure_data) {
  events <- exposure_data %>%
    mutate(
      ts = time, # Just copy the column
      ts_display = format(ts, "%Y-%m-%d %H:%M:%OS3") # Format with milliseconds
    ) %>%
    arrange(site, date, ts)
  
  return(events)
}

## Compute headways between vehicle passing events -- compute_vehicle_headways() ##
# Calculates time differences between consecutive Vehicle Passing events
# @param events Tibble with parsed timestamps and event types
# @return Tibble with headway calculations for Vehicle Passing events
compute_vehicle_headways <- function(events) {
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

## Main function to run complete headway analysis -- run_headway_analysis() ##
# Orchestrates the entire headway statistical analysis workflow
# @param export_csv Logical; whether to export Raff overall metrics to CSV (default: TRUE)
# @param csv_path Path for CSV export if export_csv is TRUE
# @param view_results Logical; whether to display results in View windows (default: TRUE)
# @return List containing all analysis results (events, headways, summaries, raff metrics)
run_headway_analysis <- function(export_csv = TRUE, 
                                 csv_path = "/Library/CloudStorage/Box-Box/2024-tprs/output/Critical_headway_Raff_OVERALL.csv",
                                 view_results = TRUE) {
  # Load and validate data
  worker_exposure_data <- load_worker_exposure_data()
  
  # Parse timestamps
  events <- parse_timestamps(worker_exposure_data)
  
  # Compute vehicle headways
  vehicle_headways <- compute_vehicle_headways(events)
  
  # Prepare lookup data
  wfg_events <- prepare_wfg_events(events)
  er_events <- prepare_er_events(events)
  
  # Classify headways
  headways_couplets <- classify_headways(vehicle_headways, wfg_events, er_events)
  
  # Compute event combinations
  event_combos <- compute_event_combinations(events)
  
  # Generate headway summary
  headway_summary_by_site <- generate_headway_summary(headways_couplets)
  
  # Compute Raff metrics
  raff_results <- compute_all_raff_metrics(headways_couplets)
  
  # Display results
  if (view_results) {
    safe_view(headways_couplets, title = "Headways: Vehicle Passing couplets only")
    safe_view(raff_results$overall, title = "Critical headway (Raff): OVERALL")
    safe_view(headway_summary_by_site, title = "Summary by site & status")
    safe_view(event_combos$pair_counts, title = "Event pair counts")
    safe_view(event_combos$triplet_counts, title = "Event triplet counts")
  }
  
  # Export if requested
  if (export_csv) {
    export_raff_overall_csv(raff_results$overall, csv_path)
  }
  
  # Make raff_overall available in parent environment for other scripts
  assign("raff_overall", raff_results$overall, envir = parent.frame())
  
  cat("\n✓ Analysis complete. Critical headway t_c =", 
      round(raff_results$overall$t_c_critical_s, 2), "seconds\n")
  cat("\nNote: We have a critical time! This t_c value represents the overall behavior\n")
  cat("of worker headway acceptance/rejection, which can be compared against the\n")
  cat("distributions of headways for each site/day.\n")
  
  # Return all results
  return(list(
    events = events,
    vehicle_headways = vehicle_headways,
    headways_couplets = headways_couplets,
    headway_summary = headway_summary_by_site,
    event_combinations = event_combos,
    raff_metrics = raff_results
  ))
}

# --- SCRIPT EXECUTION (if running file directly) -----------------------------
# Uncomment to run as standalone script:
# results <- run_headway_analysis()
