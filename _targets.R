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

  ### Reading in CSV files and returning tibbles #############################
  # helper functions are found in R/csv2tibble.R

  # read observation_data.csv into a tibble with columns:
  # date, site, strip_spacing, trailer_spacing, gopro_spacing, spacing_type
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

  # puts all worker exposure data into one dataframe with columns:
  # site, date, time, event
  # events are one of the following:
  #(arrive, depart, vehicle_pass, look for gap, enter road, exit road)
  tar_target(
    worker_exposure_files,
    list.files("data/worker_exposure", full.names = TRUE),
    format = "file"
  ),
  tar_target(
    worker_exposure_data,
    get_worker_exposure_data(worker_exposure_files, observations)
  ),

  ### Trailer and camera spacing #############################################
  # Helper functions are found in R/observations.R

  # pivot observations to show trailer and camera spacing
  tar_target(trailer_spacing, pivot_trailer_spacing(observations)),
  tar_target(camera_spacing, pivot_camera_spacing(observations)),

  ### Changes in Speed Analysis ##############################################
  # Helper functions are found in R/speed.R

  # create tibble from wavetronix data with columns:
  # site, unit, date, time, speed_85, strip_spacing
  # for use in statistical tests of 85th percentile speed
  tar_target(speed_data, prepare_speed_data(wavetronix, observations)),

  # t-test of 85th percentile speed by unit (w1 vs w2)
  tar_target(paired_t_test, paired_test(speed_data)), 

# plot confidence bounds for the t-test results of speed
  tar_target(confidence_bounds,
  plot_confidence_bounds(paired_t_test)
  ),

  ### Driver Braking and TPRS Avoidance Analysis #############################
  # Helper functions are listed in braking_and_departure.R

  # Combine camera_back_data and observations so driver behavior and
  # spacing_type are neatly assembled.
  tar_target(brake_and_departure,
    compile_brake_and_departure(camera_back_data, observations)),

  # Plot braking response in a bar chart, faceted by spacing_type and class.
  tar_target(braking_plot, plot_braking(brake_and_departure)),

  # # Plot departure response in a bar chart, faceted by spacing_type and class.
  tar_target(departure_plot, plot_departure(brake_and_departure)),

  ### TPRS displacement by volume Analysis ###################################
  # Helper functions are found in R/displacement_by_volume.R

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
  tar_target(displacement_plots_cb,
  make_displacement_plot_class_data(cumulated_class_volume, camera_top_data)
  ),

  ### TPRS Displacement by energy Analysis ###################################
  # Helper Functions are found in R/displacement_by_energy.R

  # compile speed, class, and displacement state into one data frame
  tar_target(displacement_data,
    compile_displacement_data(wavetronix,
                            camera_back_data,
                            camera_top_data,
                            observations)
  ),

  # summarize energy and traffic volume per transition
  tar_target(displacement_summary,
    summarize_displacement_data(displacement_data)
  ),

  # filter displacement_summary to acceptable transitions
  tar_target(transition_data,
    filter_displacement_summary(displacement_summary)
  ),

  # prepare the transition data for plotting
  tar_target(disp_plot_data,
    prep_transition_data(transition_data)
  ),

  # Plot the impact energy for each transition, colored by spacing
  tar_target(energy_per_transition_spacing,
    plot_energy_spacing(disp_plot_data)
  ),

  # Plot the impact energy for each transition, colored by site
  tar_target(energy_per_transition_site,
    plot_energy_site(disp_plot_data)
  ),

  ### Worker Exposure Analysis ###############################################
  # Helper functions are being developed

  ### Hourly Volumes Analysis ################################################
  # Helper functions are found in R/hourly_volumes.R

  # This section created average hourly volume plots which
  # were used to evaluate how long potential sites would
  # need to be observed to reach minimum observations.

  # Download Google Sheets as Excel files. They're large enough it's better
  # to download than to read and store a tibble.
  tar_target(
    download_sheets,
    tryCatch(
      {
        dnld_google_sheet()
      },
      error = function(e) {
        message(
          "\n*** ERROR: Failed to download UDOT's hourly volume data.***\n",
          "Either skip hourly volume estimates, or manually download the files.\n",
          "To skip, enter 'skip_hourly <- TRUE' and rerun targets.\n",
          "The required files can be downloaded at:\n",
          "https://docs.google.com/spreadsheets/d/1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw/edit?gid=2031380538#gid=2031380538\n",
          "https://docs.google.com/spreadsheets/d/1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI/edit?gid=1035130660#gid=1035130660\n",
          "The spreadsheets are called '2023 Station 501-733' and\n",
          "'2023 Station 301-431' respectively.\n",
          "Save these files in data/temp_data/2023_hourly_volumes_1.xlsx and\n",
          "data/temp_data/2023_hourly_volumes_2.xlsx.\n",
          "Original error: ", e$message, "\n"
          )
        stop(e) # re-throw to stop the pipeline
      }
    )
  ),

  # Get a list of which stations are available in the spreadsheets
  tar_target(available_stations, get_available_stations()),

  # Load list of counting stations we want to examine.
  tar_target(
    station_list,
    read.csv("data/stations_list", colClasses =  c("character"))
  ),

  # Check remove unavailable stations from station list
  tar_target(
    cleaned_station_list,
    clean_stations(station_list, available_stations)
  ),

  # Statistical parameters
  tar_target(o, 3),
  tar_target(z, 1.959964),
  tar_target(U, 1.04),
  tar_target(E, 1),

  # Date and time parameters
  tar_target(sd, "2023-05-01"), # Start Date
  tar_target(ed, "2023-08-31"), # End Date
  tar_target(st, 8), # start time
  tar_target(et, 17), # end time
  # Calculate minimum observations
  tar_target(
    n,
    get_min_obs(o, z, U, E)
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
  )
  
) # closes list of targets
