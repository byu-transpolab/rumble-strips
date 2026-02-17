# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(rstatix)
library(googledrive)
library(readxl)
library(mlogit)
library(modelsummary)
library(svglite)
# library(tarchetypes) # Load other packages as needed. # nolint
#setwd("~/Documents/GitHub/rumble-strips")

# Set target options:
tar_option_set(
  packages = c("tidyverse", 
               "mlogit", 
               "modelsummary", 
               "readxl", 
               "googledrive"), # updated: removed googlesheets4, added readxl and googledrive
  format = "rds" # default storage format
  # Set other options as needed.
)

# Boolean used to skip over hourly_volume analysis.

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts stored in R/ with your custom functions:
#for (file in list.files("R", full.names = TRUE)) source(file)


##target list#########################################

source("R/hourly_volumes.R")
source("R/csv2tibble.R")
source("R/observations.R")
source("R/speed.R")
source("R/braking_and_avoidance.R")
source("R/exposure.R")
source("R/displacement_by_volume.R")
source("R/displacement_by_energy.R")



list(


  ## ===== ANALYSIS ===== ##

  ### Hourly Volumes Analysis ################################################
  # Helper functions are found in R/hourly_volumes.R

  # This section created average hourly volume plots which
  # were used to evaluate how long potential sites would
  # need to be observed to reach minimum observations.

  # Statistical parameters
  tar_target(o, 3),        # dbl, standard deviation of miles per hour (mph)
  tar_target(z, 1.959964), # dbl, z-score 
  tar_target(U, 1.04),     # dbl, centrality adjustment (85th percentile)
  tar_target(E, 1),        # dbl, margin of error in mph

  # Date and time parameters
  tar_target(sd, "2023-05-01"), # Start Date
  tar_target(ed, "2023-08-31"), # End Date
  tar_target(st, 8), # start time in 24-hour format, integer
  tar_target(et, 17), # end time in 24-hour format, integer

  # Calculate minimum observations
  tar_target(n, get_min_obs(o, z, U, E)),

  # Load list of counting stations we want to examine.
  tar_target(station_list_file, "data/station_list", format = "file"),
  tar_target(station_list, get_station_list(station_list_file)),

  # Download UDOT's hourly counter data as Excel files. 
  # We found it more stable to download the whole workbook and process locally
  # rather than read individual sheets online.
  # This target often throws download errors. The function dnld_google_sheet()
  # has an error message with instructions on how to handle it.
  tar_target(download_sheets, dnld_google_sheet()),

  # Get the file paths to the downloaded excel files.
  tar_taget(excel_files, 
    list_excel_files("data/hourly_volumes"), format = "file"),

  # Get a list of which stations are available in the excel workbooks
  tar_target(available_stations, get_available_stations(excel_files)),

  # Filter station_list to only include available_stations
  tar_target(cleaned_station_list,
    clean_stations(station_list, available_stations)
  ),

  # Pull station data from local Excel files
  tar_target(
    all_station_data,
      map(cleaned_station_list$station_number, 
        get_station_data(.x, excel_files)),
  ),

  # Summarize each station to their hourly volumes and save the result
  tar_target(
    hourly_volumes,
    {
      hv <- tibble(cleaned_station_list,
        vector = I(map(all_station_data, 
          ~ get_hourly_volume(.x, sd, ed)
          )
        )
      )
      save(hv, file = "data/temp_data/hourly_station_data")
      hv
    }
  ),

  # Plot each station
  tar_target(
    plots,
      map2(
      hourly_volumes$vector, 
      hourly_volumes$station_number, 
      ~ ggsave(
        filename = paste0("output/", "plot_", .y,"_", sd, "_to_", ed,".svg"), 
        plot = plot_station(.x, st, et), 
        width = 7, 
        height = 5)
        )
  ),

  # Create station summary with initial values
  tar_target(
    station_summary,
    cleaned_station_list %>%
      mutate(
        AADT = 0,
        daytime_perc = 0,
        min_hours = 0
      )
  ),

  # Add AADT to station summary
  tar_target(
    AADT_summary,
    station_summary %>%
      mutate(AADT = map_int(hourly_volumes$vector, 
                            ~ {result <- sum(.x, na.rm = TRUE)
                                if (is.nan(result)) 0 
                                else as.integer(result)  # Replace NaN with 0
                              }
                            ))
  ),

  # Add daytime percentage to station summary
  tar_target(
    daytime_summary,
    AADT_summary %>%
      mutate(daytime_perc = map_dbl(hourly_volumes$vector, 
                              ~ get_aadt_perc(.x, st, et)))
  ),

  # Add minimum hours of observation to station summary
  tar_target(
    final_summary,
    daytime_summary %>%
      mutate(min_hours = map_dbl(hourly_volumes$vector, 
                           ~ get_obs_time(st, et, n, .x)))
  ),

  # Plot the station summary
  tar_target(
    plot_summary,
    plot_station_summary(final_summary)
  ),

  # Save the station summary
  tar_target(
    save_summary,
    write_csv(final_summary, "data/temp_data/station_summary")
  )
  
) # closes list of targets
