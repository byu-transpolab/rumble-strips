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

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts stored in R/ with your custom functions:
#for (file in list.files("R", full.names = TRUE)) source(file)


##target list#########################################

source("R/functions.R")
source("R/wavetronix.R")

list(
  # Download Google Sheets as Excel files before anything else
  tar_target(
    download_sheets,
    tryCatch(
      {
        dnld_google_sheet()
      },
      error = function(e) {
        message("\n*** ERROR: Failed to download Google Sheets as Excel files. ***\n",
                "Try running the program again, or manually download the file.\n",
                "See the README file in data/temp_data.\n",
                "Original error: ", e$message, "\n")
        stop(e) # re-throw to stop the pipeline
      }
    )
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
    tryCatch(
      map(cleaned_station_list$station_number, get_station_data),
      error = function(e) {
        message("\n*** ERROR: Failed to load station data from Excel files. ***\n",
                "Try running the program again, or manually check the Excel files in data/temp_data.\n",
                "See the README file in data/temp_data.\n",
                "Original error: ", e$message, "\n")
        stop(e)
      }
    )
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
  ),



  ## ===== ANALYSIS =====
  tar_target(observations_file, "data/observation_data.csv", format = "file"),
  tar_target(observations, read_observations(observations_file)),

  # puts all wavetronix data into one dataframe with columns:
  # site, unit, lane, volume, occupancy, speed, speed_85,
  # headway, gap, sensor_time, date, interval
  tar_target(wavetronix_files,
    "data/wavetronix",
    format = "file"
  ),
  tar_target(wavetronix,
    read_wavetronix_folder(wavetronix_files)
  ),

  # puts all camera top data into one dataframe with columns:
  # time, event, site
  tar_target(
    camera_top_files, 
    list.files("data/camera_top", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    camera_top_data,
    get_camera_top_data(camera_top_files)
  ),

  # puts all camera back data into one dataframe with columns:
  # site, date, time, session, class, brake, departure, flagged, lane
  tar_target(
    camera_back_files,
    list.files("data/camera_back", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    camera_back_data,
    get_camera_back_data(camera_back_files)
  ),

  # calculate cumulative traffic volume for each day from Wavetronix data
  # returns tibble with: time <dttm>, total, cumulative
  tar_target(cumulated_volume, cumulate_volume(wavetronix, observations)),

  # calculate cumulative traffic volume for each class from camera back data
  # returns tibble with: time <dttm>, class, total, cumulative
  tar_target(cumulated_class_volume,
    cumulate_class_volume(camera_back_data, observations)
  ),

  # Plot volume and events for each site with wavetronix data
  tar_target(displacement_plots_wave,
    make_displacement_plot_data(cumulated_volume, camera_top_data)
  ),

  # Plot class volumes and events for each site with camera back data
  #tar_target(displacement_plots_cb,
  #  make_displacement_plot_class_data(cumulated_class_volume, camera_top_data)
  #),

  # create tibble from wavetronix data with columns:
  # site, unit, date, time, speed_85, strip_spacing
  # for use in statistical tests of 85th percentile speed
  tar_target(speed_data, prepare_speed_data(wavetronix, observations)),

  # t-test of 85th percentile speed by unit (w1 vs w2)
  tar_target(paired_t_test, paired_test(speed_data)), 

  tar_target(confidence_bounds, plot_confidence_bounds(paired_t_test))
)

#Next Step: How to save the plot based on a given station number is what we have to figure out next.
#tbbl instead of a list of vectors. Each vector is the info for one station.
#instead of having tar_target hourly_volumes (line 85) return a list of vectors, have it return a table.

#Next Step: Can we run this code on data files of our own making?
#Can we make it easier for us to run our own data sets by asking for an input prompt for . . .  
# . . . the code to prompt an input file name for it to run?



