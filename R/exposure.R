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

# ==============================================================================
# CDF CURVE GENERATION FUNCTIONS
# ==============================================================================

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
## @param raff_midpoint Numeric value for critical headway
## @return ggplot object
create_cdf_plot <- function(data, title, raff_midpoint) {
  ggplot(data, aes(x = headway_sec)) +
    stat_ecdf(geom = "step", color = "blue") +
    geom_vline(xintercept = raff_midpoint, 
               linetype = "dashed", color = "red", linewidth = 1) +
    labs(
      title = title,
      x = "Headway (seconds)",
      y = "Cumulative Probability"
    ) +
    theme_minimal()
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

# ==============================================================================
# TARGETS PIPELINE FUNCTIONS
# ==============================================================================

## Perform headway statistical analysis for targets pipeline
##
## @param worker_exposure Worker exposure data from targets
## @return List containing classified data and raff metrics
make_headway_analysis <- function(worker_exposure) {
  we <- worker_exposure %>%
    mutate(across(where(is.character), trimws)) %>%
    mutate(Time = parse_timestamps(Time))
  
  we <- compute_vehicle_headways(we)
  
  wfg_lookup <- prepare_wfg_events(we)
  er_lookup <- prepare_er_events(we)
  we_classified <- classify_headways(we, wfg_lookup, er_lookup)
  
  event_combos <- compute_event_combinations(we_classified)
  hdwy_summary <- generate_headway_summary(we_classified)
  raff_results <- compute_all_raff_metrics(we_classified)
  
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
  
  raff_midpoint <- raff_metrics %>%
    filter(grouping == "overall") %>%
    pull(raff_midpoint)
  
  sites <- unique(hdwy_data$site)
  site_plots <- lapply(sites, function(s) {
    site_data <- hdwy_data %>% filter(site == s)
    create_cdf_plot(site_data, paste("CDF of Headways -", s), raff_midpoint)
  })
  names(site_plots) <- sites
  
  spacings <- unique(hdwy_data$spacing_ft)
  spacing_plots <- lapply(spacings, function(sp) {
    spacing_data <- hdwy_data %>% filter(spacing_ft == sp)
    create_cdf_plot(spacing_data, paste("CDF of Headways - Spacing:", sp, "ft"), raff_midpoint)
  })
  names(spacing_plots) <- paste0(spacings, "ft")
  
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
  
  stats_table <- build_histogram_stats_table(cb_combined)
  nested_data <- nest_data_with_stats(cb_combined, stats_table)
  
  raff_midpoint <- raff_metrics %>%
    filter(grouping == "overall") %>%
    pull(raff_midpoint)
  
  plots <- nested_data %>%
    mutate(
      plot = pmap(list(data, combined_site, n, mean_hdwy, sd_hdwy), function(d, title, count, m, s) {
        ggplot(d, aes(x = headway_sec)) +
          geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
          geom_vline(xintercept = raff_midpoint, 
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
  
  list(
    histogram_plots = plots$plot,
    stats_table = stats_table
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
    filename <- file.path(output_dir, paste0("cdf_", site_name, ".svg"))
    ggsave(filename, plot = cdf_results$site_plots[[site_name]], 
           device = svglite, width = 8, height = 6)
    files <- c(files, filename)
  }
  
  # Save spacing plots
  for (spacing_name in names(cdf_results$spacing_plots)) {
    filename <- file.path(output_dir, paste0("cdf_spacing_", spacing_name, ".svg"))
    ggsave(filename, plot = cdf_results$spacing_plots[[spacing_name]], 
           device = svglite, width = 8, height = 6)
    files <- c(files, filename)
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
  
  obs_data <- load_observation_data()
  cb_combined <- process_cb_with_combined_groups(camera_back, obs_data)
  combined_sites <- unique(cb_combined$combined_site)
  
  files <- c()
  for (i in seq_along(histogram_results$histogram_plots)) {
    site_name <- combined_sites[i]
    filename <- file.path(output_dir, paste0("histogram_", gsub(" ", "_", site_name), ".svg"))
    ggsave(filename, plot = histogram_results$histogram_plots[[i]], 
           device = svglite, width = 8, height = 6)
    files <- c(files, filename)
  }
  
  files
}