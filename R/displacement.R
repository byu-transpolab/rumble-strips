# Helper Functionsn related to calculating TPRS Displacement

library(tidyverse)
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

#' Estimate the state transition probabilities based on the compiled displacement data
#' 
#' @param displacement a data frame containing the compiled displacement data
#'
#' @return a data frame containing the state transition probabilities and associated 
#'         traffic characteristics for each site and time bin
#' 
estimate_state_transition <- function(displacement_data) {
  # Identify state transitions and number each period of continuous state for each site
  displacement <- displacement_data %>%
    arrange(site, time) %>%
    group_by(site) %>%
    mutate(prev_state = lag(state),
           state_change = state != prev_state & !is.na(prev_state),
           period_id = cumsum(state_change) +1) %>%
    ungroup()
  
    # Summarize traffic per period
traffic_summary <- displacement %>%
    group_by(site, period_id) %>%
    summarize(total_volume = n(),
              mean_speed = mean(speed, na.rm = TRUE),
              passenger_volume = sum(class == "passenger", na.rm = TRUE),
              truck_volume = sum(class == "truck", na.rm = TRUE),
              motorcycle_volume = sum(class == "motorcycle", na.rm = TRUE),
              start_time = min(time),
              end_time = max(time),
              state = first(state),
              .groups = "drop") %>%
    arrange(state) %>%
    group_by(site) %>%
    mutate(next_state = lead(state)) %>%
    ungroup() %>%
    filter(!is.na(next_state) & next_state != "Reset") # Only keep periods with an end transition
  
  return(traffic_summary)
}
