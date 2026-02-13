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


  ## ===== ANALYSIS ===== ##

  ### Reading in CSV files and returning tibbles #############################
  # helper functions are found in R/csv2tibble.R

  # Targets related to rainy days. 
  # Returns a boolean and a list of rainy days to be exlcuded
  # The boolean must be set to true to exclude rain days from the data.
  # Each of the 4 main tibbles (observations, wavetronix, camera_top and camera_back)
  # have follow up targets to filter out rainy days using filter_rainy_days().
  tar_target(exclude_rain, TRUE),
  tar_target(rainy_days, 
    as.Date(c(
    "2025-07-16", # most of the day was dark clouds and rainy
    "2025-07-15"  # only some of the day was dark and rainy
    ))
  ),

  # read observation_data.csv into a tibble with columns:
  # date, site, strip_spacing, trailer_spacing, gopro_spacing, spacing_type
  tar_target(observations_file, "data/observation_data.csv", format = "file"),
  tar_target(observations_complete, read_observations(observations_file)),
  tar_target(observations,
    filter_rainy_days(observations_complete, exclude_rain, rainy_days)
  ),

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
    filter_rainy_days(wavetronix_complete, exclude_rain, rainy_days)
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
    filter_rainy_days(camera_top_complete, exclude_rain, rainy_days)
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
    filter_rainy_days(camera_back_complete, exclude_rain, rainy_days)
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
    compute_headways_with_spacing(camera_back_data, observations)
  ),
  
  # Group headway data by site and spacing_type and prepare for CDF plotting
  tar_target(
    headway_by_site,
    group_headway(headway_data, group_by_site = TRUE)
  ),
  tar_target(
    headway_by_spacing,
    group_headway(headway_data, group_by_site = FALSE)
  ),

  # Take grouped headway data and create ecdf data frames for plotting

  # Find where the critical time falls on the CDF for each site and spacing type

  # Pass ecdf data frames and critical time intercepts to plotting function


  # Generate and return the headyway CDF plots by site and spacing type.
  # Mark the critical time on the plots.
  tar_target(
    headway_cdf_by_site_plot,
    create_combined_cdf_plot(
      headway_by_site,
      "CDF by Site",
      critical_time,
    )
  ),
  tar_target(
    headway_cdf_by_spacing_plot,
    create_combined_cdf_plot(
      headway_by_spacing,
      "CDF by Spacing Type",
      critical_time
    )
  ),

  # Combine the CDF plots into a list for easier saving in the next target
  tar_target(
    cdf_plots,
    list(
      headway_cdf_by_site_plot,
      headway_cdf_by_spacing_plot
    )
  ),

  # Save the CDF plots to disk
  tar_target(
    cdf_plot_files,
    save_cdf_plots(cdf_plots, output_dir = "output"),
    format = "file"
  )

) # closes list of targets
