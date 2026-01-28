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
  # Helper functions are located in R/exposure.R

  # Displacement by energy
  tar_target(
    name = displacement_by_energy,
    command = make_displacement_by_energy(
      camera_back = camera_back,
      deflection_table = deflection_table
    )
  ),
  tar_target(
    name = displacement_by_energy_plot,
    command = plot_displacement_by_energy(displacement_by_energy),
    format = "file"
  ),
  
  # Exposure analysis - Headway statistics
  tar_target(
    name = headway_analysis,
    command = make_headway_analysis(worker_exposure = worker_exposure)
  ),
  
  # Exposure analysis - CDF plots
  tar_target(
    name = cdf_plots,
    command = make_cdf_plots(
      camera_back = camera_back,
      raff_metrics = headway_analysis$raff_metrics
    )
  ),
  tar_target(
    name = cdf_plot_files,
    command = save_cdf_plots(cdf_plots, output_dir = "output"),
    format = "file"
  ),
  
  # Exposure analysis - Histogram plots
  tar_target(
    name = histogram_plots,
    command = make_histogram_plots(
      camera_back = camera_back,
      raff_metrics = headway_analysis$raff_metrics
    )
  ),
  tar_target(
    name = histogram_plot_files,
    command = save_histogram_plots(
      histogram_plots, 
      camera_back = camera_back,
      output_dir = "output"
    ),
    format = "file"
  )

) # closes list of targets
