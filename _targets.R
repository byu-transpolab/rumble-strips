# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

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
for (file in list.files("R", full.names = TRUE)) source(file)
# source("other_functions.R") # Source other scripts as needed. # nolint



# Replace the target list below with your own:
list(
  tar_target(
    station_list,
    read_csv("~/rumble-strips/data/stations_in_region4")
  ),
  tar_target(
    cleaned_station_list,
    clean_stations(station_list)
  ),
  tar_target(
    n,
    get_min_obs(3, 1.959964, 1.04, 1)
  ),
  tar_target(
    station_data_list,
    map(cleaned_station_list$station_number, station_data)
  ),
  tar_target(
    hourly_volumes,
    map(station_data_list, ~ hourly_volume(.x, "2023-05-01", "2023-08-31"))
  ),
  tar_target(
    total_hourly_volume,
    reduce(hourly_volumes, `+`)
  ),
  tar_target(
    station_list_with_aadt,
    cleaned_station_list %>%
      mutate(
        AADT = map_dbl(hourly_volumes, sum),
        AADT_percentage = map_dbl(hourly_volumes, ~ AADT_perc(.x, 8, 17))
      )
  ),
  tar_target(
    plots,
    map2(hourly_volumes, cleaned_station_list$station_number, ~ plot_station(.x, "2023-05-01", "2023-08-31", 8, 17, n))
  ),
  tar_target(
    average_hourly_volume,
    round(total_hourly_volume / nrow(cleaned_station_list), digits = 0)
  ),
  tar_target(
    final_plot,
    plot_station(average_hourly_volume, "2023-05-01", "2023-08-31", 8, 17, n)
  ),
  tar_target(
    save_summary,
    write_csv(station_list_with_aadt, "~/rumble-strips/data/station_summary")
  )
)
