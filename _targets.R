# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
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

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts stored in R/ with your custom functions:
#for (file in list.files("R", full.names = TRUE)) source(file)


##target list#########################################

source("R/functions.R")

list(
  # Download Google Sheets as Excel files before anything else
  tar_target(
    download_sheets,
    {
      dnld_google_sheet()
    }
  ),

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

  # Load station list (depends on download_sheets)
  tar_target(
    station_list,
    {
      download_sheets
      read.csv("data/temp_data/stations_list", colClasses =  c("character"))
    }
  ),

  # Calculate minimum observations
  tar_target(
    n,
    get_min_obs(o, z, U, E)
  ),

  # Check availability of station data
  tar_target(
    cleaned_station_list, 
    {
      download_sheets
      cs <- clean_stations(station_list)
      write_csv(cs, "data/temp_data/cleaned_list")
      cs
    }
  ),

  # Pull station data from local Excel files
  tar_target(
    all_station_data,
    map(cleaned_station_list$station_number, get_station_data)
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
)

#Next Step: How to save the plot based on a given station number is what we have to figure out next.
#tbbl instead of a list of vectors. Each vector is the info for one station.
#instead of having tar_target hourly_volumes (line 85) return a list of vectors, have it return a table.

#Next Step: Can we run this code on data files of our own making?
#Can we make it easier for us to run our own data sets by asking for an input prompt for . . .  
# . . . the code to prompt an input file name for it to run?
