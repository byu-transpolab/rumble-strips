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

# Summarize the energy and other values per state transition
summarize_displacement_data <- function(displacement_data) {
  # Identify state transitions and number each period of continuous state for each site
  state_change <- displacement_data %>%
    arrange(time) %>%
    mutate(date = as.Date(time),
           prev_state = lag(state),
           state_change = state != prev_state & !is.na(prev_state),
           period_id = cumsum(state_change) +1)
  
    # Summarize traffic per state transition
  displacement_summary <- state_change %>%
    # For each period, create one row that ...
    group_by(period_id) %>%
    # ... summarizes the following data.
    summarise(
      site              = first(site),
      date              = first(date),
      spacing_type      = first(spacing_type),
      start_time        = min(time, na.rm = TRUE),
      end_time          = max(time, na.rm = TRUE),
      start_state       = first(state),
      # convert to million lb*mi/hr for easier plotting
      energy            = sum(energy, na.rm = TRUE) / 1000000, 
      motorcycle_volume = sum(class == "motorcycle", na.rm = TRUE),
      passenger_volume  = sum(class == "passenger",  na.rm = TRUE),
      truck_volume      = sum(class == "truck",      na.rm = TRUE),
      mean_speed        = mean(speed, na.rm = TRUE),
      .groups = "drop") %>%
    # Calculate duration and next state for each summary row
    mutate(duration = as.numeric(difftime(end_time, start_time, units = "mins")),
           next_state = lead(start_state)) %>%
    # Filter out transitions that meet the following criteria:
    filter(next_state != "Reset" & 
           !is.na(next_state) & 
           start_state != "Out of Specification")

  return (displacement_summary)
}

filter_displacement_summary <- function(displacement_data) {
  # Focus on just energy per transition (exclude volumes and mean speeds)
  # I know we put a lot of effort into summarizing that data, but it's not used.
  # I keep the summary there in case we want to use it later.
  transition_data <- displacement_summary %>%
    select(site, date, start_time, spacing_type, start_state, next_state, 
          energy) %>%
    arrange(date, start_time) %>%
    group_by(date) %>%
    # Only keep series of transitions that start with "Reset"
    # OR where start_state == lag(next_state)
    mutate(
    # Apply row-by-row validation using accumulate
    valid = accumulate(
      .x = row_number(),
      .f = function(prev_valid, i) {
        # First row rule of the day must start with Reset
        if (i == 1) {
          return(start_state[i] == "Reset")
        }

        # If previous row invalid, current row is invalid
        if (!prev_valid) {
          # Only valid if this row resets
          return(start_state[i] == "Reset")
        }

        # If this row is a Reset → always valid
        if (start_state[i] == "Reset") {
          return(TRUE)
        }

        # Otherwise normal rule: must match previous next_state
        return(start_state[i] == next_state[i - 1])
      },
      .init = TRUE
    )[-1] # remove the initial TRUE used to start accumulate
    ) %>% 
    filter(valid) %>%
    select(-valid, -start_time) %>%
    ungroup()
  
  return(transition_data)
}

prep_transition_data <- function(transition_data) {
  # Take in the filtered transition_data provided by filter_displacement_summary
  # and prepare it for plotting

  # Step 1: make the basis for the final tibble, plot_data
  base_rows <- transition_data %>%
    transmute(
      site,
      date,
      spacing_type,
      state = next_state,
      energy
    )

  # Step 2: Add rows where state = "Reset" and energy = 0.0
  reset_rows <- transition_data %>%
    filter(start_state == "Reset") %>%
    transmute(
      site,
      date, 
      spacing_type,
      state = start_state, # "Reset"
      energy = 0.0
    )

  # Step 3: Bind the reset_rows and base_rows together...
  plot_data <- bind_rows(reset_rows, base_rows) %>%
    arrange(spacing_type, site) %>%
    mutate(
        # ...Ensure state factors are properly ordered...
        state = factor(
          state,
          levels = c(
            "Reset",
            "Some Movement",
            "Moderate Movement",
            "Significant Movement",
            "Out of Specification"
          ),
          ordered = TRUE
        ),
        # ...Group lines so they start at Reset. Used in later plotting.
        segment_id = cumsum(state == "Reset")
      ) %>%
      # Add cumulative energy column and keeping previous energy column.
      group_by(site, date, spacing_type, segment_id) %>%
      arrange(state, .by_group = TRUE) %>%
      mutate(cum_energy = cumsum(energy)) %>%
      ungroup()

  return(plot_data)
}

plot_energy_spacing <- function(plot_data) {
# Plot the prepared plot_data and color by spacing type.

  p <- plot_data %>%
ggplot(
    aes(
      x = state,
      y = cum_energy, # <----- Can change between cum_energy and energy.
      color = spacing_type,
      group = interaction(spacing_type, segment_id)
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Displacement",
    y = " Cumulative Energy (millions of lbs * mi / hr)",
    color = "Spacing Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


  # Save and return
  ggsave("output/energy-per-transition-by-spacing.svg", 
    plot = p, width = 10, height = 6)
  p
}

plot_energy_site <- function(plot_data) {
# Plot the prepared plot_data and color by spacing type.

  p <- plot_data %>%
ggplot(
    aes(
      x = state,
      y = cum_energy, # <----- Can change between cum_energy and energy.
      color = site,
      group = interaction(spacing_type, segment_id)
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Displacement",
    y = " Cumulative Energy (millions of lbs * mi / hr)",
    color = "Spacing Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


  # Save and return
  ggsave("output/energy-per-transition-by-site.svg", 
    plot = p, width = 10, height = 6)
  p
}

# Deprecated: plot is no longer used in report
bar_plot_transition_data <-function(transition_data) {

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