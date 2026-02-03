# Helper Functionsn related to calculating TPRS Displacement

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

# Function to calculate weighted average truck weight from BTS truck counts.
#' @param bts_truck_counts a df with columns: class, count, weight 
calc_truck_weight <- function(bts_truck_counts) {

  # Only include class 4 trucks and above (busses, dualies, semi's, etc.)
  bts_truck_counts <- bts_truck_counts %>%
    filter(class >= 4)
  
  # Get the total count of trucks
  total_count <- sum(bts_truck_counts$count)

  # Calculate weighted average of truck weights
  weighted_sum <- sum(bts_truck_counts$count * bts_truck_counts$weight)
  weighted_avg_weight <- weighted_sum / total_count

  return(weighted_avg_weight)
}

#' compile the wavetronix speed data, camera_back_data, and camera_top_data into a single data frame
#' @param wavetronix a data frame containing the wavetronix data
#' @param camera_back_data a data frame containing the camera back data
#' @param camera_top_data a data frame containing the camera top data
#' @param observations a data frame containing the observation data
#' @param motorcycle_weight weight of motorcycle in lbs (default 800 lbs)
#' @param passenger_weight weight of passenger vehicle in lbs (default 4419 lbs)
#' @param truck_weight weight of truck in lbs (default 40000 lbs)
#' 
compile_displacement_data <- function(
  wavetronix,       # Provides mean speed data
  camera_back_data, # Provides vehicles class, counts, and time data
  camera_top_data,  # Provides displacement event data
  observations,     # Provides spacing type data
  # Default vehicle weights were set by casual Google search results
  motorcycle_weight = 800,    # in lbs
  passenger_weight  = 4419,   # in lbs
  truck_weight      = 40000   # in lbs
  ) {
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
  mutate(
        # Calculate momentum per vehicle as energy proxy
        # Weight assumptions came from top Google search results in lbs
        momentum = case_when(
          class == "motorcycle" ~ speed * motorcycle_weight,
          class == "passenger"  ~ speed * passenger_weight,
          class == "truck"      ~ speed * truck_weight)
        ) %>%
  select(site, time, class, speed, momentum, spacing_type, state)

  
  return(displacement_data)
}

# Summarize the momentum and other values per state transition
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
      # Convert to millions lbs*mi/hr for easier plotting
      # Converts to metric in prep_transition_data() function.
      momentum            = sum(momentum, na.rm = TRUE) / 1000000, 
      motorcycle_volume = sum(class == "motorcycle", na.rm = TRUE),
      passenger_volume  = sum(class == "passenger",  na.rm = TRUE),
      truck_volume      = sum(class == "truck",      na.rm = TRUE),
      mean_speed        = mean(speed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculate duration and next state for each summary row
    mutate(
      duration = as.numeric(difftime(end_time, start_time, units = "mins")),
      next_state = lead(start_state)
    ) %>%
    # Filter out transitions that meet the following criteria:
    filter(next_state != "Reset" &
           !is.na(next_state) &
           start_state != "Out of Specification" &
           duration <= 480)

  return (displacement_summary)
}

filter_displacement_summary <- function(displacement_summary) {
  # Focus on just momentum per transition (exclude volumes and mean speeds)
  # I know we put a lot of effort into summarizing that data, but it's not used.
  # I keep the summary there in case we want to use it later.
  transition_data <- displacement_summary %>%
    select(site, date, start_time, spacing_type, start_state, next_state, 
      momentum) %>%
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
    select(-valid) %>%
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
      start_time,
      spacing_type,
      state = next_state,
      momentum
    )

  # Step 2: Add rows where state = "Reset" and momentum = 0.0
  reset_rows <- transition_data %>%
    filter(start_state == "Reset") %>%
    transmute(
      site,
      date,
      start_time,
      spacing_type,
      state = start_state, # "Reset"
      momentum = 0.0 # All Reset states haven't had any vehicles hit them yet
    )

  # Step 3: Bind the reset_rows and base_rows together...
  disp_plot_data <- bind_rows(reset_rows, base_rows) %>%
    arrange(start_time) %>%
    mutate(
        # Group lines so they start at Reset. Used in later plotting.
        segment_id = cumsum(state == "Reset")
      ) %>%
      # Add cumulative momentum column and keeping previous momentum column.
      group_by(site, date, spacing_type, segment_id) %>%
      arrange(state, .by_group = TRUE) %>%
      mutate(cum_momentum = cumsum(momentum)) %>%
      ungroup() 

  # Calculate the conversion factor from million lbs*mi/hr to kg*m/s
  conversion_factor <- 0.4535924 * 1609.344 / 3600 # = 0.2027739

  # Apply conversion factor to momentum and cum_momentum columns
  disp_plot_data <- disp_plot_data %>%
    mutate(
      momentum = momentum * conversion_factor,
      cum_momentum = cum_momentum * conversion_factor
    )

  # Add a small jitter to the x position based on segment_id to separate lines
  jitter_width <- 0.01

  disp_plot_data <- disp_plot_data %>%
    dplyr::group_by(state) %>%
    dplyr::mutate(
      seg_idx = as.numeric(factor(segment_id)),
      seg_center = mean(unique(seg_idx)),
      state_jitter = as.numeric(state) + (seg_idx - seg_center) * jitter_width
    ) %>%
    dplyr::ungroup()

  return(disp_plot_data)
}

# Plot the prepared plot_data and color by spacing type.
plot_momentum_spacing <- function(plot_data) {

  p <- plot_data %>%
ggplot(
    aes(
      x = state,
      y = cum_momentum, # <----- Can change between cum_momentum and momentum.
      color = spacing_type,
      group = interaction(spacing_type, segment_id)
    )
  ) +
  geom_line(
    linewidth = 0.7,
    alpha = 0.5
  ) +
  geom_point(size = 2) +
  labs(
    x = "Displacement",
    y = " Cumulative Momentum (million kg*m/s)",
    color = "Spacing Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


  # Save and return
  ggsave("output/momentum-per-transition-by-spacing.svg", 
    plot = p, width = 10, height = 6)
  p
}

# Plot the prepared plot_data and color by site.
plot_momentum_site <- function(plot_data) {
  p <- plot_data %>%
  ggplot(
    aes(
      x = state_jitter,
      y = cum_momentum, # <----- Can change between cum_momentum and momentum.
      color = site,
      group = interaction(spacing_type, segment_id)
    )
  ) +
  geom_line(
    linewidth = 0.7,
    alpha = 0.5
  ) +
  geom_point(size = 1) +
  scale_x_continuous(
    breaks = unique(as.numeric(plot_data$state)),
    labels = unique(plot_data$state)
  ) +
  labs(
    x = "Displacement",
    y = " Cumulative Momentum (million kg*m/s)",
    color = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

  # Save and return
  ggsave("output/momentum-per-transition-by-site.svg", 
    plot = p, width = 10, height = 6)
  p
}

# Deprecated: plot is no longer used in report
bar_plot_transition_data <-function(transition_data) {

plot_data <- transition_data %>%
  #create a unique label for each transition period
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