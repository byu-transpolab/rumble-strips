# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tidyverse)
library(googlesheets4)
library(mlogit)
library(modelsummary)
library(svglite)
# library(tarchetypes) # Load other packages as needed. # nolint

# Deauthenticate to prevent Google login prompts
gs4_deauth()

# Set target options:
tar_option_set(
  packages = c("tidyverse", 
               "mlogit", 
               "modelsummary", 
               "googlesheets4"), # packages that your targets need to run
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


# Replace the target list below with your own:
list(
  
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
  
  # Load station list
  tar_target(
    station_list,
    read.csv("data/stations_in_region4",
             colClasses =  c("character"))
  ),
  
  # Calculate minimum observations
  tar_target(
    n,
    get_min_obs(o, z, U, E)
  ),
  
  # Check availability of station data on Google Sheets
  tar_target(
    cleaned_station_list,
    clean_stations(station_list)
  ),
  
  # # Load all_station_data
  # tar_target(
  #   all_station_data, {
  #   load("data/all_station_data.rds")
  #   get("result") #for reasons unknown, the previous load command saves all_station_data.rds as "result" in the global environment. 
  #   }
  # ),
  # 
  # # Pull and save station data from Google Sheets
  # tar_target(
  #   save_station_data,
  #   {
  #     station_data <- map(cleaned_station_list$station_number, 
  #                         get_station_data)
  #     save(station_data, 
  #          file = "data/all_station_data.rds")
  #     "data/all_station_data.rds"
  #   },
  #   format = "file" # Declare the target output as a file
  # ),
  
  # pull station data from Google sheets
  tar_target(
    all_station_data,
    map(cleaned_station_list$station_number, get_station_data)
  ),
  
  # Summarize each station to their hourly volumes and save the result
  
  # to-do: return a tibble with station names so we can label plots
  tar_target(
    hourly_volumes,
    {
      hv <- tibble(cleaned_station_list,
                   vector = I(map(all_station_data, 
                                 ~ get_hourly_volume(.x, sd, ed)
                                 )
                             )
                  )
      save(hv, file = "data/all_station_data")
      hv
    }
  ),
  
  #plot each station
  
  # how can we save each plot with the file_name stating station, sd, ed?
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
      mutate(AADT = map_int(hourly_volumes, sum))
  ),
  
  # Add daytime percentage to station summary
  tar_target(
    daytime_summary,
    AADT_summary %>%
      mutate(daytime_perc = map_dbl(hourly_volumes, 
                              ~ get_aadt_perc(.x, st, et)))
  ),
  
  # Add minimum hours of observation to station summary
  tar_target(
    final_summary,
    daytime_summary %>%
      mutate(min_hours = map_dbl(hourly_volumes, 
                           ~ get_obs_time(st, et, n, .x)))
  ),
  
  #plot the station summary
  tar_target(
    plot_summary,
    plot_station_summary(final_summary),
  ),
  
  #save the station summary
  tar_target(
    save_summary,
    write_csv(final_summary, "data/station_summary"),
  )
)
