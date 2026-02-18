# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(rstatix)
library(googledrive)
library(readxl)
library(mlogit)
library(modelsummary)
library(svglite)
# library(tarchetypes)
# Load other packages as needed.
# setwd("~/Documents/GitHub/rumble-strips")

# Set target options:
tar_option_set(
  packages = c("tidyverse",
               "mlogit",
               "modelsummary",
               "readxl",
               "googledrive"),
  format = "rds" # default storage format
  # Set other options as needed.
)

# Boolean used to skip over hourly_volume analysis.

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}}
# to allow use_targets() to configure tar_make_future() options.

# Load the R scripts stored in R/ with your custom functions:
#for (file in list.files("R", full.names = TRUE)) source(file)


##target list#########################################

source("R/hourly_volumes.R")
source("R/csv2tibble.R")
source("R/observations.R")
source("R/speed.R")
source("R/braking_and_departure.R")
source("R/exposure.R")
source("R/displacement_by_volume.R")
source("R/displacement_by_momentum.R")



list(

  ## ===== Lit Review and Methodology ===== ##
  # These are the targets used in the literature review and methodology

  ### Spacing Specifications #############################################
  # Helper functions found in R/spacing_specs.R

  # read in, and process test spacing data
  tar_target(test_spacing_file, "data/test_spacing.csv", format = "file"),
  tar_target(test_spacing, get_test_spacing(test_spacing_file)),

  # read in, and process state spacing data
  tar_target(state_spacing_file, "data/state_spacing.csv", format = "file"),
  tar_target(state_spacing, get_state_spacing(state_spacing_file)),

  # read in, and process old test spacing data
  tar_target(old_test_spacing_file, "data/old_test_spacing.csv", format = "file"),
  tar_target(old_test_spacing, read.csv(old_test_spacing_file)),

  # Plot each data set individually
  tar_target(test_spacing_plot, plot_test_spacing(test_spacing)),
  tar_target(state_spacing_plot, plot_state_spacing(state_spacing)),
  tar_target(old_test_spacing_plot, plot_old_test_spacing(old_test_spacing)),

  # Save each plot to output
  tar_target(test_spacing_plot_file,
    ggsave(
      "output/test_spacing.svg",
      plot = test_spacing_plot,
      device = svglite,
      width = 6,
      height = 6,
      units = "in"
    )
  ),
  tar_target(state_spacing_plot_file,
    ggsave(
      "output/state_spacing.svg",
      plot = state_spacing_plot,
      device = svglite,
      width = 6,
      height = 8,
      units = "in"
    )
  ),
  tar_target(old_test_spacing_file,
    ggsave(
      "output/old_test_spacing.svg",
      plot = old_test_spacing_plot,
      device = svglite,
      width = 6,
      height = 6,
      units = "in"
    )
  ),


  ### Hourly Volumes ######################################################
  # Helper functions are kept in R/hourly_volumes.R

  # This section created average hourly volume plots which
  # were used to evaluate how long potential sites would
  # need to be observed to reach minimum observations.

  # Statistical parameters
  tar_target(o, 3),        # dbl, standard deviation of miles per hour (mph)
  tar_target(z, 1.959964), # dbl, z-score 
  tar_target(U, 1.04),     # dbl, centrality adjustment (85th percentile)
  tar_target(E, 1),        # dbl, margin of error in mph

  # Date and time parameters
  tar_target(start_date, as.Date("2023-05-01")), # start date of work season
  tar_target(end_date, as.Date("2023-08-31")), # end date of work season
  tar_target(start_time, 8), # start time of work day in 24-hour format, integer
  tar_target(end_time, 17), # end time of work day in 24-hour format, integer

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
  tar_target(excel_files, 
    list_excel_files("data/hourly_volumes"), format = "file"),

  # Get a list of which stations are available in the excel workbooks
  tar_target(available_stations, get_available_stations(excel_files)),

  # Filter station_list to only include available_stations
  tar_target(cleaned_station_list,
    clean_stations(station_list, available_stations)
  ),

  # Pull station data from local Excel files into one big tibble
  tar_target(
    all_station_data, 
    get_all_station_data(cleaned_station_list, excel_files)
  ),

  # Summarize each station to their hourly volumes
  tar_target(
    hourly_volumes,
    get_hourly_volume(all_station_data, start_date, end_date)
  ),

  # Plot all the stations in a faceted plot
  tar_target(
    hourly_volume_plot, 
    plot_hourly_volumes(hourly_volumes, start_time, end_time, n)
  ),

  # Save the faceted plot to output
  tar_target(
    hourly_volume_plot_file,
    ggsave(
      "output/hourly_volume_plot.svg",
      plot = hourly_volume_plot,
      device = svglite,
      width = 6,
      height = 8,
      units = "in"
    )
  ),

  ## ===== ANALYSIS ===== ##
  # these are the targets used in the data analysis

  ### Reading in CSV files and returning tibbles #############################
  # helper functions are found in R/csv2tibble.R

  # Targets related to rainy days. 
  # Returns a boolean and a list of rainy days to be exlcuded
  # The boolean must be set to true to exclude rain days from the data.
  # Each of the 3 main tibbles (wavetronix, camera_top and camera_back)
  # have follow up targets to filter out rainy days using filter_rainy_days().
  tar_target(exclude_rain, TRUE),
  tar_target(rainy_periods, 
    list(
      # Dates are accurate, but time estimates exceed time of bad weather
      # US-6, NO TPRS.
      interval(ymd_hms("2025-07-16 15:00:00"), ymd_hms("2025-07-16 17:00:00")),
      # # US-6, 1:2.
      interval(ymd_hms("2025-07-15 16:00:00"), ymd_hms("2025-07-15 19:00:00"))
    )
  ),

  # read observation_data.csv into a tibble with columns:
  # date, site, strip_spacing, trailer_spacing, gopro_spacing, spacing_type
  tar_target(observations_file, "data/observation_data.csv", format = "file"),
  tar_target(observations, read_observations(observations_file)),
  # observations should not be filtered by rain, or it will cause errors with
  # worker_exposure_data further down the pipeline.

  # puts all wavetronix data into one dataframe with columns:
  # site, unit, lane, volume, occupancy, speed, speed_85,
  # headway, gap, sensor_time, date, interval
  tar_target(wavetronix_files,
    "data/wavetronix",
    format = "file"
  ),
  tar_target(wavetronix_complete,
    read_wavetronix_folder(wavetronix_files)
  ),
  tar_target(wavetronix,
    filter_rainy_days(wavetronix_complete, exclude_rain, rainy_periods)
  ),

  # puts all camera top data into one dataframe with columns:
  # time, event, site
  tar_target(
    camera_top_files, 
    list.files("data/camera_top", full.names = TRUE),
    format = "file"
  ),
  tar_target(camera_top_complete,
    get_camera_top_data(camera_top_files)
  ),
  tar_target(camera_top_data,
    filter_rainy_days(camera_top_complete, exclude_rain, rainy_periods)
  ),

  # puts all camera back data into one dataframe with columns:
  # site, date, time, session, class, brake, departure, flagged, lane
  tar_target(
    camera_back_files,
    list.files("data/camera_back", full.names = TRUE),
    format = "file"
  ),
  tar_target(camera_back_complete,
    get_camera_back_data(camera_back_files)
  ),
  tar_target(camera_back_data,
    filter_rainy_days(camera_back_complete, exclude_rain, rainy_periods)
  ),

  # Download truck counts from BTS.gov and process into tibble
  tar_target(
    truck_counts_file,
    dnld_bts_truck_counts(),
    format = "file"
  ),
  tar_target(
    bts_truck_counts,
    process_bts_truck_counts(truck_counts_file)
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
  # worker_exposure_data does not need to be filtered by rainy days.
  # The workers never worked in the rain, though they did move strips on 07/15.

  ### Trailer and camera spacing #############################################
  # Helper functions are found in R/observations.R

  # pivot observations to show trailer and camera spacing
  tar_target(trailer_spacing, pivot_trailer_spacing(observations)),
  tar_target(camera_spacing, pivot_camera_spacing(observations)),

  ### Changes in Speed #######################################################
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

  tar_target(confidence_bounds_file,
    ggsave(
      "output/change-in-speeds.svg", 
      plot = confidence_bounds,
      width = 6,
      height = 4,
      units = "in")
  ),

  # t-test of 85th percentile speed by each unit alone
  tar_target(single_unit_t_test_w1, 
    run_single_unit_t_test(speed_data, unit = "w1")
  ),
  tar_target(single_unit_t_test_w2, 
    run_single_unit_t_test(speed_data, unit = "w2")
  ),

  # plot confidence bounds for the single unit t-test results
  tar_target(single_unit_confidence_bounds_w1,
    plot_single_unit_confidence_bounds(single_unit_t_test_w1, unit = "w1")
  ),
  tar_target(single_unit_confidence_bounds_w2,
    plot_single_unit_confidence_bounds(single_unit_t_test_w2, unit = "w2")
  ),

  # Save the single unit confidence bounds plots
  tar_target(single_unit_speed_w1_file,
    ggsave(
      "output/single_unit_speed_w1.svg",
      plot = single_unit_confidence_bounds_w1,
      width = 6,
      height = 4,
      units = "in")
  ),
  tar_target(single_unit_speed_w2_file,
    ggsave(
      "output/single_unit_speed_w2.svg",
      plot = single_unit_confidence_bounds_w2,
      width = 6,
      height = 4,
      units = "in")
  ),

  ### Driver Braking and TPRS Avoidance ######################################
  # Helper functions are listed in braking_and_departure.R

  # Combine camera_back_data and observations so driver behavior and
  # spacing_type are neatly assembled.
  tar_target(brake_and_departure,
    compile_brake_and_departure(camera_back_data, observations)),

  # Plot braking response in a bar chart, faceted by spacing_type and class.
  tar_target(braking_plot, plot_braking(brake_and_departure)),

  # # Plot departure response in a bar chart, faceted by spacing_type and class.
  tar_target(departure_plot, plot_departure(brake_and_departure)),

  # Save the braking and departure plots
  tar_target(braking_plot_file,
    ggsave(
      "output/braking_plot.svg",
      plot = braking_plot,
      width = 5,
      height = 6,
      units = "in")
  ),
  tar_target(departure_plot_file,
    ggsave(
      "output/departure_plot.svg",
      plot = departure_plot,
      width = 4,
      height = 6,
      units = "in")
  ),

  # models of braking and avoidance
  tar_target(brake_models, estimate_brake_models(brake_and_departure)),
  tar_target(avoid_models, estimate_avoid_models(brake_and_departure)),

  ### TPRS displacement by volume ############################################
  # Helper functions are found in R/displacement_by_volume.R

  # calculate cumulative traffic volume for each day from Wavetronix data
  # returns tibble with: time <dttm>, total, cumulative
  tar_target(cumulated_volume, cumulate_volume(wavetronix, observations)),

  # calculate cumulative traffic volume for each class from camera back data
  # returns tibble with: time <dttm>, class, total, cumulative
  tar_target(cumulated_class_volume,
    cumulate_class_volume(camera_back_data, observations)
  ),

  # List the available site names
  tar_target(site_names,
    unique(cumulated_volume$site)
  ),

  # Plot class volumes and events for each site
  tar_target(displacement_by_class_plots,
    make_displacement_plot_class_data(
      cumulated_class_volume,
      camera_top_data,
      site_info = site_names),
    # Pattern here is what lets the target iterate over each site
    pattern = map(site_names)
  ),

  # Save each of the displacement by class plots
  tar_target(displacement_by_class_plot_files,
    save_displacement_plots(
      site_info = site_names,
      plot = displacement_by_class_plots,
      output_dir = "output"
    ),
    # Pattern is what lets the target iterate over each site
    pattern = map(site_names, displacement_by_class_plots),
    format = "file"
  ),

  ### TPRS Displacement by momentum ##########################################
  # Helper Functions are found in R/displacement_by_momentum.R

  # define vehicle weights
  tar_target(motorcycle_weight, 800), # lbs, initial Google search result
  tar_target(passenger_weight, 4419), # lbs, Based on EPA data for 2024.
  tar_target(truck_weight,            # lbs, calculated from BTS data from 2021
    calc_truck_weight(bts_truck_counts)    # using a weighted average
  ),

  # compile speed, class, and displacement state into one data frame
  tar_target(displacement_data,
    compile_displacement_data(
      wavetronix,
      camera_back_data,
      camera_top_data,
      observations,
      motorcycle_weight,
      passenger_weight,
      truck_weight)
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

  # Plot the impact momentum for each transition, colored by spacing
  tar_target(momentum_per_transition_spacing,
    plot_momentum(disp_plot_data, FALSE)
  ),

  # Plot the impact momentum for each transition, colored by site
  tar_target(momentum_per_transition_site,
    plot_momentum(disp_plot_data, TRUE)
  ),

  # Save the momentum per transition plots
  tar_target(momentum_per_transition_spacing_file,
    ggsave(
      "output/momentum_per_transition_spacing.svg",
      plot = momentum_per_transition_spacing,
      width = 6,
      height = 4,
      units = "in")
  ),
  tar_target(momentum_per_transition_site_file,
    ggsave(
      "output/momentum_per_transition_site.svg",
      plot = momentum_per_transition_site,
      width = 6,
      height = 4,
      units = "in")
  ),

  ### Worker Exposure Analysis ###############################################
  # Helper functions located in R/exposure.R

  # Find the critical time workers need to replace the TPRS
  tar_target(
    critical_time,
    find_critical_time(worker_exposure_data)
  ),

  # Get headway data for each vehicle and add which spacing type they were in
  tar_target(
    headway_data,
    compute_headways(camera_back_data, observations)
  ),

  # Plot headway CDF with critical time has a vertical dashed line
  # Colored by site:
  tar_target(
    headway_cdf_site_plot,
    plot_headway(headway_data, critical_time, "site")
  ),
  # Colored by spacing_type:
  tar_target(
    headway_cdf_spacing_plot,
    plot_headway(headway_data, critical_time, "spacing_type")
  ),

  # Save the CDF plots
  tar_target(
    headway_cdf_site_plot_file,
    ggsave(
      "output/cdf_sites.svg",
      headway_cdf_site_plot,
      device = svglite,
      width = 6,
      height = 4,
      units = "in"
    )
  ),
  tar_target(
    headway_cdf_spacing_plot_file,
    ggsave(
      "output/cdf_spacing.svg",
      headway_cdf_spacing_plot,
      device = svglite,
      width = 6,
      height = 4,
      units = "in"
    )
  )
) # closes list of targets
