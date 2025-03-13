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


##target list#########################################

source("R/functions.R") # Source other scripts as needed. # nolint


# Replace the target list below with your own:
list(
  
  # Statistical parameters
  tar_target(o, 3),
  tar_target(z, 1.959964),
  tar_target(U, 1.04),
  tar_target(E, 1),
  
  # Date and time parameters
  tar_target(sd, "2023-05-01"),
  tar_target(ed, "2023-08-31"),
  tar_target(st, 8),
  tar_target(et, 17),
  
  # Load station list
  tar_target(
    station_list,
    read.csv("data/stations_in_region4")
  ),
  
  # Calculate minimum observations
  tar_target(
    n,
    get_min_obs(o, z, U, E)
  ),
  
  # Check availability of station data on Google Sheets
  tar_target(
    cleaned_station_list,
    clean_stations(station_list)
  ),
  
  # Pull station data from Google Sheets
  tar_target(
    station_data_list,
    map(cleaned_station_list$station_number, 
        get_station_data)
  ),
  
  # Summarize each station to their hourly volumes
  tar_target(
    hourly_volumes,
    map(station_data_list, 
        ~ get_hourly_volume(.x, sd, ed))
  ),
  
  #plot each station
  tar_target(
    plots,
    map(hourly_volumes, 
        ~ plot_station(.x, st, et))
  ),
  
  # Create station summary
  tar_target(
    station_summary,
    cleaned_station_list %>%
      mutate(
        AADT = map_dbl(hourly_volumes, sum),
        daytime_perc = map_dbl(hourly_volumes,
                                  ~ get_aadt_perc(.x, st, et)),
        min_hours = map_dbl(hourly_volumes,
                            ~ get_obs_time(st, et, n, .x))
      )
  ),
  
  #save the station summary
  tar_target(
    save_summary,
    write_csv(station_summary, "data/station_summary"),
  ),
)
