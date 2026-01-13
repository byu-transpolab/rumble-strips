# Helper Functionsn related to calculating TPRS Displacement

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

#' compile the wavetronix speed data, camera_back_data, and camera_top_data into a single data frame
#' @param wavetronix a data frame containing the wavetronix data
#' @param camera_back_data a data frame containing the camera back data
#' @param camera_top_data a data frame containing the camera top data
#' 
compile_displacement_data <- function(wavetronix, camera_back_data, camera_top_data) {
  # Process wavetronix data
  # only keep site, unit=w1, lane=01, volume, speed, sensor_time
  wav <- wavetronix %>%
    filter(unit=="w1") %>%
    filter(lane=="01") %>%
    mutate(bin = floor_date(sensor_time, unit = "15 minutes")) %>%
    select(speed, bin)
  
  # Process camera back data
  cb <- camera_back_data %>%
    filter(lane == "lane 1") %>%
    select(site, time, class) %>%
    mutate(bin = floor_date(time, unit = "15 minutes"))
  
  # Process camera top data
  ct <- camera_top_data %>%
    select(event_time = time, state = event)
  
  # Combine all processed data into a single data frame
displacement_data <- cb %>%
  left_join(wav, by = "bin") %>%
  left_join(ct, join_by(time >= event_time)) %>%
  group_by(time) %>%
  slice_max(event_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(site, time, class, speed, state)

  
  return(displacement_data)
}

