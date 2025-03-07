# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(googlesheets4)
# library(tarchetypes) # Load other packages as needed. # nolint

# Deauthenticate to prevent Google login prompts
gs4_deauth()

# Set target options:
tar_option_set(
  packages = c("tidyverse", "mlogit", "modelsummary"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts stored in R/ with your custom functions:
#for (file in list.files("R", full.names = TRUE)) source(file)
 source("R/functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # Load station list
  tar_target(
    station_list_raw,
    read_csv("~/rumble-strips/data/stations_in_region4")
  ),
  
  # Clean station list
  tar_target(
    station_list,
    clean_stations(station_list_raw)
  ),
  
  # Statistical parameters
  tar_target(o, 3),
  tar_target(z, 1.959964),
  tar_target(U, 1.04),
  tar_target(E, 1),
  tar_target(n, get_min_obs(o, z, U, E)),
  tar_target(sd, "2023-05-01"),
  tar_target(ed, "2023-08-31"),
  tar_target(st, 8),
  tar_target(et, 17),
  
  # Process each station's data
  tar_target(
    station_data,
    get_station_data(station),
    pattern = map(station_list$station_number)
  ),
  
  # Hourly volumes for all stations
  tar_target(
    hourly_volumes,
    get_hourly_volume(station_data, sd, ed),
    pattern = map(station_data)
  ),
  
  # Compute aggregated metrics
  tar_target(
    station_metrics,
    station_list %>%
      mutate(
        AADT = map_dbl(hourly_volumes, sum),
        AADT_percentage = map_dbl(hourly_volumes, ~ get_aadt_perc(.x, st, et)),
        hours = map_dbl(hourly_volumes, ~ get_obs_time(st, et, n, .x))
      )
  ),
  
  # Total hourly volume
  tar_target(
    total_hourly_volume,
    hourly_volumes %>%
      reduce(`+`) %>%
      `/`(length(hourly_volumes)) %>%
      round(0)
  ),
  
  # Generate plots
  tar_target(
    station_plots,
    plot_station(hourly_volumes, sd, ed, st, et, n),
    pattern = map(hourly_volumes)
  ),
  tar_target(
    total_plot,
    plot_station(total_hourly_volume, sd, ed, st, et, n)
  ),
  
  # Save final results
  tar_target(
    save_results,
    write_csv(
      station_metrics,
      "~/rumble-strips/data/station_summary"
    )
  )
)
