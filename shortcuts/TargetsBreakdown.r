# _targets.R BREAKDOWN
# =====================================================================
# This file explains what each section of _targets.R does
# WITHOUT changing any code in _targets.R

# LINES 1-5: File Header
# =====================================================================
# Introduction and reference to the targets manual
# This is just documentation

# LINES 7-14: Load Required Libraries
# =====================================================================
# library(targets)
# library(tidyverse)
# library(rstatix)
# library(googledrive)
# library(readxl)
# library(mlogit)
# library(modelsummary)
# library(svglite)
#
# Loads all packages needed to run the pipeline

# LINES 18-27: Pipeline Configuration
# =====================================================================
# tar_option_set(
#   packages = c("tidyverse", "mlogit", "modelsummary", "readxl", "googledrive"),
#   format = "rds"
# )
#
# Sets default options for all targets:
# - which packages to load in parallel workers
# - storage format (RDS files)

# LINES 42-43: Source Custom Functions
# =====================================================================
# source("R/functions.R")
# source("R/wavetronix.R")
#
# Loads your custom R functions from the R/ folder that the targets use

# LINES 45-60: Download Data
# =====================================================================
# tar_target(download_sheets, ...)
#
# Downloads Google Sheets as Excel files before anything else starts

# LINES 62-71: Set Statistical Parameters
# =====================================================================
# tar_target(o, 3)
# tar_target(z, 1.959964)
# tar_target(U, 1.04)
# tar_target(E, 1)
#
# o, z, U, E: Statistical constants used in calculations

# LINES 73-77: Set Date/Time Parameters
# =====================================================================
# tar_target(sd, "2023-05-01")     # Start date
# tar_target(ed, "2023-08-31")     # End date
# tar_target(st, 8)                # Start time (8am)
# tar_target(et, 17)               # End time (5pm)

# LINES 79-135: Station Data Processing Pipeline
# =====================================================================
# 1. station_list (lines 79-84)
#    - Reads station list from Excel
#
# 2. n (lines 86-89)
#    - Calculates minimum observations needed
#
# 3. cleaned_station_list (lines 91-99)
#    - Cleans station data and saves it
#
# 4. all_station_data (lines 101-112)
#    - Loads station data from Excel files
#
# 5. hourly_volumes (lines 114-125)
#    - Converts to hourly volumes
#
# 6. plots (lines 127-135)
#    - Creates SVG plots for each station

# LINES 137-185: Station Summary Creation
# =====================================================================
# 1. station_summary (lines 137-143)
#    - Creates base summary table with AADT, daytime_perc, min_hours = 0
#
# 2. AADT_summary (lines 145-153)
#    - Adds Annual Average Daily Traffic counts
#
# 3. daytime_summary (lines 155-161)
#    - Adds daytime percentage calculations
#
# 4. final_summary (lines 163-169)
#    - Adds minimum hours of observation
#
# 5. plot_summary (lines 171-174)
#    - Creates summary visualization
#
# 6. save_summary (lines 176-179)
#    - Saves summary to CSV

# LINES 182-212: Analysis Data Assembly
# =====================================================================
# 1. observations (lines 182-183)
#    - Reads observation data from CSV
#
# 2. wavetronix (lines 185-192)
#    - Combines all wavetronix traffic sensor data into one dataframe
#    - Columns: site, unit, lane, volume, occupancy, speed, speed_85,
#              headway, gap, sensor_time, date, interval
#
# 3. camera_top_data (lines 194-202)
#    - Combines camera top data (event detection data)
#    - Columns: time, event, site
#
# 4. camera_back_data (lines 204-212)
#    - Combines camera back data (vehicle classification data)
#    - Columns: site, date, time, session, class, brake, departure,
#               flagged, lane

# LINES 214-243: Volume & Speed Analysis
# =====================================================================
# 1. cumulated_volume (lines 214-216)
#    - Calculates cumulative traffic volume over time
#    - Columns: time <dttm>, total, cumulative
#
# 2. cumulated_class_volume (lines 218-222)
#    - Calculates cumulative volume by vehicle class
#    - Columns: time <dttm>, class, total, cumulative
#
# 3. displacement_plots_wave (lines 224-227)
#    - Prepares displacement plot data using wavetronix
#
# 4. displacement_plots_cb (lines 229-232)
#    - Prepares displacement plot data using camera classifications
#
# 5. speed_data (lines 234-236)
#    - Prepares speed data for statistical testing
#    - Columns: site, unit, date, time, speed_85, strip_spacing
#
# 6. paired_t_test (lines 238-240)
#    - Runs paired t-test on 85th percentile speed between two units
#
# 7. confidence_bounds (lines 242-243)
#    - Creates confidence bound plots

# =====================================================================
# COMMANDS YOU CAN RUN:
# =====================================================================
#
# tar_make()
#   - Runs the entire pipeline from start to finish
#
# tar_make(target_name)
#   - Runs only a specific target
#   - Example: tar_make(wavetronix)
#
# tar_read(target_name)
#   - Loads a cached target into memory
#   - Example: tar_read(camera_back_data)
#
# tar_visnetwork()
#   - Shows a visual graph of dependencies between targets
#
# tar_outdated()
#   - Shows which targets need to be re-run
#
# tar_load(target_name)
#   - Loads a target without returning it
