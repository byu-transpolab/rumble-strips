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
compile_displacement_data <- function(wavetronix, camera_back_data, camera_top_data, observations) {
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

  # Process observation data
  obs <- observations %>%
    select(date, spacing_type)
  
  # Combine all processed data into a single data frame
displacement_data <- cb %>%
  # Add speed data from wav
  left_join(wav, by = "bin") %>%
  # Add state data from ct (match each cb time to the most recent ct event_time)
  left_join(ct, join_by(time >= event_time)) %>%
  group_by(time) %>%
  slice_max(event_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Add spacing type from observation data (match by date)
  mutate(date = as.Date(time)) %>%
  left_join(obs, by = "date") %>%
  # Add levels to state factor
  mutate(state = factor(state, 
    levels = c("Reset",
              "Some Movement", 
              "Moderate Movement", 
              "Significant Movement", 
              "Out of Specification")),
        # Energy is a made up metric: speed * weight (lbs)
        # Weight assumptions came from top Google search results in lbs
        energy = case_when(
          class == "motorcycle" ~ speed * 800,
          class == "passenger"  ~ speed * 4419,
          class == "truck"      ~ speed * 40000)
        ) %>%
  select(site, time, class, speed, energy, spacing_type, state)

  
  return(displacement_data)
}

#' Estimate the state transition probabilities based on the compiled displacement data
#' 
#' @param displacement a data frame containing the compiled displacement data
#'
#' @return a data frame containing the state transition probabilities and associated 
#'         traffic characteristics for each site and time bin
estimate_state_transition <- function(displacement_data) {
  # Identify state transitions and number each period of continuous state for each site
  displacement <- displacement_data %>%
    mutate(date = as.Date(time)) %>%
    arrange(time) %>%
    group_by(date) %>%
    mutate(prev_state = lag(state),
           state_change = state != prev_state & !is.na(prev_state),
           period_id = cumsum(state_change) +1) %>%
    ungroup()
  
    # Summarize traffic per period
  transition_data <- displacement %>%
    arrange(date, time) %>%
    group_by(date, period_id) %>%
    summarise(
      site              = first(site),
      mean_speed        = mean(speed, na.rm = TRUE),
      energy            = sum(energy, na.rm = TRUE),
      motorcycle_volume = sum(class == "motorcycle", na.rm = TRUE),
      passenger_volume  = sum(class == "passenger",  na.rm = TRUE),
      truck_volume      = sum(class == "truck",      na.rm = TRUE),
      start_time        = min(time, na.rm = TRUE),
      end_time          = max(time, na.rm = TRUE),
      spacing_type      = first(spacing_type),
      state             = first(state),
      .groups = "drop") %>%
    # Ensure chronological order within day
    arrange(date, start_time) %>%
    group_by(date) %>%
    # Calculate duration of each period in minutes
    mutate(duration = as.numeric(difftime(end_time, start_time, units = "mins"))) %>%,
    # The "state the period turned into" is the next period's state that day
    mutate(next_state = lead(state)) %>%
    ungroup() %>%
    # Keep only periods that actually transition into something
    filter(!is.na(next_state)) %>%
    # Final shape: one row per period with the "turned-into" state
    transmute(
      site,
      date,
      duration,
      mean_speed,
      energy,
      total_volume = motorcycle_volume + passenger_volume + truck_volume,
      motorcycle_volume,
      passenger_volume,
      truck_volume,
      spacing_type,
      start_state = state,
      end_state = next_state) %>%
      # Exclude transitions into Reset and periods less than 1 minute
      filter(end_state != "Reset" & duration > 1) %>%
      # Add an ID for each entry (needed for plotting later)
      mutate(transition_id = row_number())


  return(transition_data)
}

plot_transition_data <-function(transition_data) {

plot_data <- transition_data %>%
  #create a unique label for each transition period
  mutate(transition_id = paste(site, "-", spacing_type, "-", transition_id)) %>%
  #put the periods in order of end_state, then transition_id, then site...
  arrange(end_state, transition_id, site) %>%
  #...and THEN make transition_id a factor with levels in that order
  mutate(transition_id = factor(transition_id, levels = unique(transition_id))) %>%
  # Reshape data to long format for stacked bar plotting
  pivot_longer(cols = c(passenger_volume, truck_volume, motorcycle_volume),
    names_to = "vehicle_type", values_to = "volume")
  
# Compute ranges for each end_state, used to make colored background rectangles
state_ranges <- plot_data %>%
  distinct(end_state, transition_id) %>%
  mutate(x_pos = row_number()) %>%
  group_by(end_state) %>%
  summarize(xmin = min(x_pos) - 0.5,
            xmax = max(x_pos) + 0.5,
            .groups = "drop") %>%
  # Assign a background color to each end_state category
  mutate(bg_fill = case_when(
    end_state == "Some Movement" ~ "#A5D6A7",       # light green
    end_state == "Moderate Movement" ~ "#FFF59D",   # light yellow
    end_state == "Significant Movement" ~ "#FFCC80",# light orange
    end_state == "Out of Specification" ~ "#EF9A9A" # light red
  ))

# Build chart
p <- ggplot(plot_data, aes(x = transition_id, y = volume, fill = vehicle_type)) +
  # Background rectangles
  geom_rect(data = state_ranges,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = state_ranges$bg_fill, alpha = 0.3) +
  # Stacked bars
  geom_bar(stat = "identity", width = 0.8) +
  # Mean speed labels above bars
  geom_text(data = plot_data,
            aes(x = transition_id, y = total_volume, label = round(mean_speed, 1)),
            inherit.aes = FALSE,
            vjust = -0.6, fontface = "bold", size = 3.5) +
  scale_fill_brewer(palette = "Set2", name = "Vehicle Type",
                    labels = c("motorcycle_volume" = "Motorcycle",
                               "passenger_volume" = "Passenger",
                               "truck_volume" = "Truck")) +
  labs(x = "Site - Spacing Type", y = "Vehicle Volume") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "right") +
  expand_limits(y = max(transition_data$total_volume) * 1.08) +
  coord_cartesian(clip = "off")

  # Save and return
  ggsave("output/volume-per-transition.svg", plot = p, width = 14, height = 14)
  p
}