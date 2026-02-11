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
      width = 10,
      height = 8)
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
      width = 10,
      height = 8)
  ),
  tar_target(single_unit_speed_w2_file,
    ggsave(
      "output/single_unit_speed_w2.svg",
      plot = single_unit_confidence_bounds_w2,
      width = 10,
      height = 8)
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
      width = 10,
      height = 13)
  ),
  tar_target(departure_plot_file,
    ggsave(
      "output/departure_plot.svg",
      plot = departure_plot,
      width = 10,
      height = 13)
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
    plot_momentum_spacing(disp_plot_data)
  ),

  # Plot the impact momentum for each transition, colored by site
  tar_target(momentum_per_transition_site,
    plot_momentum_site(disp_plot_data)
  ),

  # Save the momentum per transition plots
  tar_target(momentum_per_transition_spacing_file,
    ggsave(
      "output/momentum_per_transition_spacing.svg",
      plot = momentum_per_transition_spacing,
      width = 10,
      height = 8)
  ),
  tar_target(momentum_per_transition_site_file,
    ggsave(
      "output/momentum_per_transition_site.svg",
      plot = momentum_per_transition_site,
      width = 10,
      height = 6)
  ),

  ### Worker Exposure Analysis ###############################################
  # Helper functions located in R/exposure.R

  # Find the critical gap time the workers need to adjust the strips
  tar_target(
    critical_time,
    find_critical_time(worker_exposure_data)
  ),
  
  # Find the headway in secs for each vehicle observed.
  # Add the spacing type to each row.
  tar_target(
    headway_data,
    compute_headways(camera_back_data, observations)
  ),

  # Sort the headway data into two lists: 
  # headway__by_site and headway_by_spacing
  # Each item in each list is a subset of the headway data for that one site
  # or spacing
  tar_target(
    sorted_headway,
    sort_headway_data(headway_data)
  ),

  # Generate the Cumulative Distribution Function plots for headway
  # Add a vertical line to mark the critical time and label the percentage
  # At each intersection
  tar_target(
    cdf_plots,
    make_cdf_plots(
      sorted_headway,
      critical_time
    )
  ),

  # Save the plots to files
  tar_target(
    cdf_plot_files,
    save_cdf_plots(cdf_plots, output_dir = "output"),
    format = "file"
  )

) # closes list of targets
