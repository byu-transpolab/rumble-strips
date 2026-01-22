# Helper functions for making headway distributions and Work Exposure analysis.

#Used Libraries
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(svglite)   # for ggsave(..., device = svglite)
library(readr)     # for reading observation_data.csv
library(patchwork) # for combining plots


# ==============================================================================
# HEADWAY STATISTICAL ANALYSIS FUNCTIONS
# ==============================================================================

## Load worker exposure data from targets store
##
## @return A tibble containing worker_exposure data
load_worker_exposure_data <- function() {
  tar_read(worker_exposure) %>%
    mutate(across(where(is.character), trimws))
}

## Parse timestamps with millisecond precision
##
## @param timestamp_str Character string with format "YYYY-MM-DD HH:MM:SS.mmm"
## @return POSIXct datetime object
parse_timestamps <- function(timestamp_str) {
  as.POSIXct(timestamp_str, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}

## Compute vehicle headways from timestamp data
##
## @param data Tibble with Site, Date, and Time columns
## @return Tibble with headway_sec column added
compute_vehicle_headways <- function(data) {
  data %>%
    arrange(Site, Date, Time) %>%
    group_by(Site, Date) %>%
    mutate(headway_sec = as.numeric(difftime(Time, lag(Time), units = "secs"))) %>%
    ungroup()
}

## Prepare Waiting for Gap (WFG) events for lookup
##
## @param data Tibble containing worker exposure data
## @return Tibble with WFG events keyed by Site, Date, Worker
prepare_wfg_events <- function(data) {
  data %>%
    filter(Event == "WFG") %>%
    select(Site, Date, Worker, wfg_time = Time)
}

## Prepare Entering Roadway (ER) events for lookup
##
## @param data Tibble containing worker exposure data
## @return Tibble with ER events keyed by Site, Date, Worker
prepare_er_events <- function(data) {
  data %>%
    filter(Event == "ER") %>%
    select(Site, Date, Worker, er_time = Time)
}

## Classify headways as Accepted, Rejected, or NotTracked
##
## @param data Tibble with vehicle timestamps
## @param wfg_lookup Tibble with WFG events
## @param er_lookup Tibble with ER events
## @return Tibble with Classification column added
classify_headways <- function(data, wfg_lookup, er_lookup) {
  data %>%
    left_join(wfg_lookup, by = c("Site", "Date", "Worker")) %>%
    left_join(er_lookup, by = c("Site", "Date", "Worker")) %>%
    mutate(
      Classification = case_when(
        is.na(wfg_time) & is.na(er_time) ~ "NotTracked",
        !is.na(wfg_time) & Time >= wfg_time & Time < er_time ~ "Rejected",
        !is.na(er_time) & Time >= er_time & Time < lead(wfg_time) ~ "Accepted",
        TRUE ~ "NotTracked"
      )
    ) %>%
    select(-wfg_time, -er_time)
}

## Compute event combinations statistics
##
## @param data Tibble with Classification column
## @return Summary tibble with event type counts
compute_event_combinations <- function(data) {
  data %>%
    filter(!is.na(headway_sec), headway_sec > 0) %>%
    group_by(Site, Date, Worker) %>%
    summarise(
      Total = n(),
      Accepted = sum(Classification == "Accepted"),
      Rejected = sum(Classification == "Rejected"),
      NotTracked = sum(Classification == "NotTracked"),
      .groups = "drop"
    )
}

## Generate headway summary statistics
##
## @param data Tibble with headway_sec and Classification columns
## @return Summary tibble with min, max, mean, median by Classification
generate_headway_summary <- function(data) {
  data %>%
    filter(!is.na(headway_sec), headway_sec > 0) %>%
    group_by(Classification) %>%
    summarise(
      count = n(),
      min_hdwy = min(headway_sec),
      max_hdwy = max(headway_sec),
      mean_hdwy = mean(headway_sec),
      median_hdwy = median(headway_sec),
      .groups = "drop"
    )
}

## Compute Raff critical headway metrics
##
## @param data Tibble with headway_sec and Classification columns
## @return List with raff_lower, raff_upper, raff_midpoint
compute_raff_metrics <- function(data) {
  accepted <- data %>%
    filter(Classification == "Accepted", !is.na(headway_sec), headway_sec > 0) %>%
    pull(headway_sec)
  
  rejected <- data %>%
    filter(Classification == "Rejected", !is.na(headway_sec), headway_sec > 0) %>%
    pull(headway_sec)
  
  if (length(accepted) == 0 || length(rejected) == 0) {
    return(list(raff_lower = NA_real_, raff_upper = NA_real_, raff_midpoint = NA_real_))
  }
  
  raff_lower <- max(accepted)
  raff_upper <- min(rejected)
  raff_midpoint <- (raff_lower + raff_upper) / 2
  
  list(raff_lower = raff_lower, raff_upper = raff_upper, raff_midpoint = raff_midpoint)
}

## Compute all Raff metrics (overall, by site, by site-date)
##
## @param data Tibble with classified headways
## @return Tibble with raff_overall, raff_by_site, raff_by_site_date
compute_all_raff_metrics <- function(data) {
  raff_overall <- compute_raff_metrics(data) %>%
    as_tibble() %>%
    mutate(grouping = "overall")
  
  raff_by_site <- data %>%
    group_by(Site) %>%
    group_map(~ compute_raff_metrics(.x) %>% as_tibble()) %>%
    bind_rows(.id = "Site") %>%
    mutate(grouping = "by_site")
  
  raff_by_site_date <- data %>%
    group_by(Site, Date) %>%
    group_map(~ compute_raff_metrics(.x) %>% as_tibble()) %>%
    bind_rows(.id = "id") %>%
    separate(id, into = c("Site", "Date"), sep = "\\.") %>%
    mutate(grouping = "by_site_date")
  
  bind_rows(raff_overall, raff_by_site, raff_by_site_date)
}

## Safe data viewing with optional printing
##
## @param data Tibble to view
## @param print_output Logical, whether to print the data
## @return The input data (invisibly)
safe_view <- function(data, print_output = FALSE) {
  if (print_output) print(data)
  invisible(data)
}

## Export raff_overall metrics to CSV
##
## @param raff_results Tibble containing raff metrics
## @param output_dir Directory path for saving CSV
## @return NULL (called for side effect)
export_raff_overall_csv <- function(raff_results, output_dir = "output") {
  raff_overall_export <- raff_results %>%
    filter(grouping == "overall") %>%
    select(raff_lower, raff_upper, raff_midpoint)
  
  write_csv(raff_overall_export, file.path(output_dir, "raff_overall.csv"))
  message("Exported raff_overall.csv to ", output_dir)
}

# ==============================================================================
# CDF CURVE GENERATION FUNCTIONS
# ==============================================================================

## Setup output directory for CDF plots
##
## @param output_dir Directory path to create
## @return NULL (called for side effect)
setup_output_directory <- function(output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
}

## Load camera back data from targets
##
## @return Tibble containing camera_back data
load_camera_back_data <- function() {
  tar_read(camera_back)
}

## Validate that critical headway data exists
##
## @param output_dir Directory containing raff_overall.csv
## @return Tibble with raff_overall metrics
validate_critical_headway <- function(output_dir = "output") {
  raff_path <- file.path(output_dir, "raff_overall.csv")
  if (!file.exists(raff_path)) {
    stop("raff_overall.csv not found. Run headway analysis first.")
  }
  read_csv(raff_path, show_col_types = FALSE)
}

## Load observation data from CSV
##
## @return Tibble with site and spacing information
load_observation_data <- function() {
  read_csv("data/observation_data.csv", show_col_types = FALSE) %>%
    select(site, spacing_ft)
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
## @param raff_overall Tibble with raff critical headway metrics
## @return ggplot object
create_cdf_plot <- function(data, title, raff_overall) {
  ggplot(data, aes(x = headway_sec)) +
    stat_ecdf(geom = "step", color = "blue") +
    geom_vline(xintercept = raff_overall$raff_midpoint, 
               linetype = "dashed", color = "red", linewidth = 1) +
    labs(
      title = title,
      x = "Headway (seconds)",
      y = "Cumulative Probability"
    ) +
    theme_minimal()
}

## Generate CDF plots for each site
##
## @param hdwy_data Tibble with headway data
## @param raff_overall Tibble with raff metrics
## @param output_dir Directory for saving plots
## @return List of ggplot objects
generate_site_cdfs <- function(hdwy_data, raff_overall, output_dir = "output") {
  sites <- unique(hdwy_data$site)
  
  plots <- lapply(sites, function(s) {
    site_data <- hdwy_data %>% filter(site == s)
    p <- create_cdf_plot(site_data, paste("CDF of Headways -", s), raff_overall)
    ggsave(file.path(output_dir, paste0("cdf_", s, ".svg")), 
           plot = p, device = svglite, width = 8, height = 6)
    p
  })
  
  names(plots) <- sites
  plots
}

## Generate CDF plots for each spacing category
##
## @param hdwy_data Tibble with headway and spacing data
## @param raff_overall Tibble with raff metrics
## @param output_dir Directory for saving plots
## @return List of ggplot objects
generate_spacing_cdfs <- function(hdwy_data, raff_overall, output_dir = "output") {
  spacings <- unique(hdwy_data$spacing_ft)
  
  plots <- lapply(spacings, function(sp) {
    spacing_data <- hdwy_data %>% filter(spacing_ft == sp)
    p <- create_cdf_plot(spacing_data, paste("CDF of Headways - Spacing:", sp, "ft"), raff_overall)
    ggsave(file.path(output_dir, paste0("cdf_spacing_", sp, "ft.svg")), 
           plot = p, device = svglite, width = 8, height = 6)
    p
  })
  
  names(plots) <- paste0(spacings, "ft")
  plots
}

## Generate summary statistics for headways
##
## @param hdwy_data Tibble with headway data
## @return Tibble with summary stats by site and spacing
generate_summary_stats <- function(hdwy_data) {
  hdwy_data %>%
    group_by(site, spacing_ft) %>%
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

## Setup output directory for histogram plots
##
## @param output_dir Directory path to create
## @return NULL (called for side effect)
setup_histogram_output_dir <- function(output_dir = "output") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
}

## Validate critical headway data for histogram generation
##
## @param output_dir Directory containing raff_overall.csv
## @return Tibble with raff_overall metrics
validate_critical_headway_histogram <- function(output_dir = "output") {
  raff_path <- file.path(output_dir, "raff_overall.csv")
  if (!file.exists(raff_path)) {
    stop("raff_overall.csv not found. Run headway analysis first.")
  }
  read_csv(raff_path, show_col_types = FALSE)
}

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
    mutate(
      combined_site = paste0(site, " (", spacing_ft, " ft)"),
      combined_spacing = paste0(spacing_ft, " ft")
    )
}

## Build histogram statistics table
##
## @param data Tibble with headway data and grouping columns
## @return Tibble with summary statistics
build_histogram_stats_table <- function(data) {
  data %>%
    group_by(combined_site, combined_spacing) %>%
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
    group_by(combined_site, combined_spacing) %>%
    nest() %>%
    left_join(stats_table, by = c("combined_site", "combined_spacing"))
}

## Create and save histogram plots
##
## @param nested_data Nested tibble with data and stats
## @param raff_overall Tibble with raff metrics
## @param output_dir Directory for saving plots
## @return List of ggplot objects
create_save_histograms <- function(nested_data, raff_overall, output_dir = "output") {
  plots <- nested_data %>%
    mutate(
      plot = pmap(list(data, combined_site, n, mean_hdwy, sd_hdwy), function(d, title, count, m, s) {
        ggplot(d, aes(x = headway_sec)) +
          geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
          geom_vline(xintercept = raff_overall$raff_midpoint, 
                     linetype = "dashed", color = "red", linewidth = 1) +
          labs(
            title = paste("Histogram -", title),
            subtitle = sprintf("n = %d, mean = %.2f, sd = %.2f", count, m, s),
            x = "Headway (seconds)",
            y = "Count"
          ) +
          theme_minimal()
      })
    )
  
  # Save plots
  walk2(plots$plot, plots$combined_site, function(p, site_name) {
    filename <- paste0("histogram_", gsub(" ", "_", site_name), ".svg")
    ggsave(file.path(output_dir, filename), plot = p, device = svglite, width = 8, height = 6)
  })
  
  plots$plot
}

# ==============================================================================
# MAIN ORCHESTRATION FUNCTIONS
# ==============================================================================

## Run complete headway statistical analysis
##
## @param output_dir Directory for saving results
## @param print_results Logical, whether to print intermediate results
## @return List containing all analysis results
run_headway_analysis <- function(output_dir = "output", print_results = FALSE) {
  setup_output_directory(output_dir)
  
  message("Loading worker exposure data...")
  we <- load_worker_exposure_data()
  
  message("Parsing timestamps...")
  we <- we %>%
    mutate(Time = parse_timestamps(Time))
  
  message("Computing headways...")
  we <- compute_vehicle_headways(we)
  
  message("Classifying headways...")
  wfg_lookup <- prepare_wfg_events(we)
  er_lookup <- prepare_er_events(we)
  we_classified <- classify_headways(we, wfg_lookup, er_lookup)
  
  message("Computing event combinations...")
  event_combos <- compute_event_combinations(we_classified)
  safe_view(event_combos, print_results)
  
  message("Generating headway summary...")
  hdwy_summary <- generate_headway_summary(we_classified)
  safe_view(hdwy_summary, print_results)
  
  message("Computing Raff metrics...")
  raff_results <- compute_all_raff_metrics(we_classified)
  safe_view(raff_results, print_results)
  
  message("Exporting raff_overall.csv...")
  export_raff_overall_csv(raff_results, output_dir)
  
  list(
    worker_exposure = we_classified,
    event_combinations = event_combos,
    headway_summary = hdwy_summary,
    raff_metrics = raff_results
  )
}

## Run CDF curve generation
##
## @param output_dir Directory for saving plots
## @return List containing CDF plots and summary stats
run_cdf_analysis <- function(output_dir = "output") {
  setup_output_directory(output_dir)
  
  message("Loading raff_overall metrics...")
  raff_overall <- validate_critical_headway(output_dir)
  
  message("Loading camera back data...")
  cb <- load_camera_back_data()
  
  message("Loading observation data...")
  obs_data <- load_observation_data()
  
  message("Computing headways with spacing...")
  hdwy_data <- compute_headways_with_spacing(cb, obs_data)
  
  message("Generating site CDFs...")
  site_plots <- generate_site_cdfs(hdwy_data, raff_overall, output_dir)
  
  message("Generating spacing CDFs...")
  spacing_plots <- generate_spacing_cdfs(hdwy_data, raff_overall, output_dir)
  
  message("Computing summary statistics...")
  summary_stats <- generate_summary_stats(hdwy_data)
  
  list(
    site_plots = site_plots,
    spacing_plots = spacing_plots,
    summary_stats = summary_stats,
    headway_data = hdwy_data
  )
}

## Run histogram generation
##
## @param output_dir Directory for saving plots
## @return List containing histogram plots
run_histogram_analysis <- function(output_dir = "output") {
  setup_histogram_output_dir(output_dir)
  
  message("Loading raff_overall metrics...")
  raff_overall <- validate_critical_headway_histogram(output_dir)
  
  message("Loading camera back data...")
  cb <- load_camera_back_data()
  
  message("Loading observation data...")
  obs_data <- load_observation_data()
  
  message("Processing data with combined groups...")
  cb_combined <- process_cb_with_combined_groups(cb, obs_data)
  
  message("Building statistics table...")
  stats_table <- build_histogram_stats_table(cb_combined)
  
  message("Nesting data for plotting...")
  nested_data <- nest_data_with_stats(cb_combined, stats_table)
  
  message("Creating and saving histograms...")
  plots <- create_save_histograms(nested_data, raff_overall, output_dir)
  
  list(
    histogram_plots = plots,
    stats_table = stats_table
  )
}

## Run complete exposure analysis workflow
##
## @param output_dir Directory for saving all results
## @param print_results Logical, whether to print intermediate results
## @return List containing all analysis results
run_exposure_analysis <- function(output_dir = "output", print_results = FALSE) {
  message("========================================")
  message("Starting Complete Exposure Analysis")
  message("========================================\n")
  
  message("Step 1: Headway Statistical Analysis")
  headway_results <- run_headway_analysis(output_dir, print_results)
  
  message("\nStep 2: CDF Curve Generation")
  cdf_results <- run_cdf_analysis(output_dir)
  
  message("\nStep 3: Histogram Generation")
  histogram_results <- run_histogram_analysis(output_dir)
  
  message("\n========================================")
  message("Exposure Analysis Complete!")
  message("========================================")
  
  list(
    headway_analysis = headway_results,
    cdf_analysis = cdf_results,
    histogram_analysis = histogram_results
  )
}

# ==============================================================================
# UNCOMMENT TO RUN THE COMPLETE ANALYSIS
# ==============================================================================
# results <- run_exposure_analysis(output_dir = "output", print_results = TRUE)