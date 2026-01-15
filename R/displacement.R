# Helper Functionsn related to calculating TPRS Displacement

library(tidyverse)
library(lubridate)
library(ggplot2)

#' compile the wavetronix speed data, camera_back_data, and camera_top_data into a single data frame
#' @param wavetronix a data frame containing the wavetronix data
#' @param camera_back_data a data frame containing the camera back data
#' @param camera_top_data a data frame containing the camera top data
#' 
compile_displacement_data <- function(wavetronix, camera_back_data, camera_top_data, observation_data) {
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
  obs <- observation_data %>%
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
              "Out of Specification"))) %>%
  select(site, time, class, speed, spacing_type, state)

  
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
    mutate(date = as.Date(time)) %>%
    arrange(time) %>%
    group_by(date) %>%
           prev_state = lag(state),
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
    # The "state the period turned into" is the next period's state that day
    mutate(next_state = lead(state)) %>%
    ungroup() %>%
    # Compute duration after we know start/end
    mutate(duration = as.numeric(difftime(end_time, start_time, units = "mins"))) %>%
    # Keep only periods that actually transition into something
    filter(!is.na(next_state)) %>%
    # Final shape: one row per period with the "turned-into" state
    transmute(
      site,
      date,
      duration,
      mean_speed,
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
  distinct(end_state, site_spacing) %>%
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
ggplot(plot_data, aes(x = site_spacing, y = volume, fill = vehicle_type)) +
  # Background rectangles
  geom_rect(data = state_ranges,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = state_ranges$bg_fill, alpha = 0.3) +
  # Stacked bars
  geom_bar(stat = "identity", width = 0.8) +
  # Mean speed labels above bars
  geom_text(data = plot_data,
            aes(x = site_spacing, y = total_volume, label = round(mean_speed, 1)),
            inherit.aes = FALSE,
            vjust = -0.6, fontface = "bold", size = 3.5) +
  scale_fill_brewer(palette = "Set2", name = "Vehicle Type",
                    labels = c("motorcycle_volume" = "Motorcycle",
                               "passenger_volume" = "Passenger",
                               "truck_volume" = "Truck")) +
  labs(title = "Traffic Volume by Spacing Type and End State",
       x = "Site - Spacing Type", y = "Vehicle Volume") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "right") +
  expand_limits(y = max(transition_data$total_volume) * 1.08) +
  coord_cartesian(clip = "off")

}

##### Version 2: Add vertical dashed lines between next_state groups and labels above bars

# 1) Build a single, consistently ordered data frame with a numeric x-index
ordered_df <- transition_data %>%
  arrange(next_state, site, period_id) %>%
  mutate(
    site_period = str_c(site, "#", period_id),
    x_pos = row_number()   # stable x for all layers
  )

# 2) Long format for stacking
plot_data <- ordered_df %>%
  pivot_longer(
    cols = c(passenger_volume, truck_volume, motorcycle_volume),
    names_to = "vehicle_type",
    values_to = "volume"
  )

# 3) Compute shaded background ranges per next_state
state_ranges <- ordered_df %>%
  group_by(next_state) %>%
  summarize(
    xmin = min(x_pos) - 0.5,
    xmax = max(x_pos) + 0.5,
    .groups = "drop"
  ) %>%
  # (Optional) Alternate fills by group to improve contrast
  mutate(bg_fill = if_else(row_number() %% 2 == 1, "gray95", "gray90"))

# 3b) (Optional) Vertical lines at boundaries between next_state groups
state_boundaries <- ordered_df %>%
  group_by(next_state) %>%
  summarize(end_pos = max(x_pos), .groups = "drop") %>%
  mutate(xintercept = end_pos + 0.5)

# 4) Plot
ggplot(plot_data, aes(x = x_pos, y = volume, fill = vehicle_type)) +
  # Background rectangles by next_state
  geom_rect(
    data = state_ranges,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = after_stat(NULL)),
    inherit.aes = FALSE,
    color = NA,
    fill = state_ranges$bg_fill,  # Use precomputed colors
    alpha = 0.6
  ) +
  # Stacked bars
  geom_bar(stat = "identity", width = 0.85) +
  # (Optional) Also add vertical lines to emphasize boundaries
  geom_vline(
    data = state_boundaries,
    aes(xintercept = xintercept),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "gray55",
    linewidth = 0.3
  ) +
  # Mean speed labels above the full bar
  geom_text(
    data = ordered_df,
    aes(x = x_pos, y = total_volume, label = round(mean_speed, 1)),
    inherit.aes = FALSE,
    vjust = -0.6,
    fontface = "bold",
    size = 3.5
  ) +
  # Vehicle class palette & legends
  scale_fill_brewer(palette = "Set2", name = "Vehicle class",
                    labels = c("motorcycle_volume" = "Motorcycle",
                               "passenger_volume" = "Passenger",
                               "truck_volume" = "Truck")) +
  # Show x labels as site_period
  scale_x_continuous(
    breaks = ordered_df$x_pos,
    labels = ordered_df$site_period,
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  # Make room for labels above bars
  expand_limits(y = max(ordered_df$total_volume) * 1.08) +
  labs(
    title = "Traffic Volume Composition and Speed by Upcoming State",
    subtitle = "Stacked by vehicle class; shaded groups indicate next_state",
    x = "Site and Period",
    y = "Volume (count)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 10)  # a little room on top/right
  ) +
  # Prevent text clipping if labels go above panel
  coord_cartesian(clip = "off")



###### Version 3: Add background shading by next_state with a green → red gradient

# -----------------------
# 1) Configure next_state order and prepare an ordered data frame
# -----------------------
state_levels <- c("some movement", "moderate movement", "significant movement", "out of specification")

ordered_df <- transition_data %>%
  mutate(
    next_state = factor(next_state, levels = state_levels, ordered = TRUE)
  ) %>%
  arrange(next_state, site, period_id) %>%
  mutate(
    site_period = str_c(site, "#", period_id),
    x_pos = row_number()   # stable numeric x for all layers
  )

# -----------------------
# 2) Long format for stacked bars
# -----------------------
plot_data <- ordered_df %>%
  pivot_longer(
    cols = c(passenger_volume, truck_volume, motorcycle_volume),
    names_to = "vehicle_type",
    values_to = "volume"
  )

# -----------------------
# 3) Shaded background ranges + label positions
# -----------------------
state_ranges <- ordered_df %>%
  group_by(next_state) %>%
  summarize(
    xmin = min(x_pos) - 0.5,
    xmax = max(x_pos) + 0.5,
    xlab = min(x_pos) - 0.45,   # near left edge inside the band
    .groups = "drop"
  )

# Green → Red gradient for background by next_state (named vector)
bg_colors <- c(
  "some movement"         = "#2E7D32",  # deep green
  "moderate movement"     = "#C0CA33",  # yellow-green
  "significant movement"  = "#FB8C00",  # orange
  "out of specification"  = "#C62828"   # red
)

# Compute y-positions
y_max <- max(ordered_df$total_volume, na.rm = TRUE)
y_lab_inside <- y_max * 0.97   # place labels just inside the top of each shaded region

# -----------------------
# 4) Optional vertical dashed separators between next_state groups
# -----------------------
state_boundaries <- ordered_df %>%
  group_by(next_state) %>%
  summarize(end_pos = max(x_pos), .groups = "drop") %>%
  mutate(xintercept = end_pos + 0.5)

# -----------------------
# 5) Plot
# -----------------------
ggplot(plot_data, aes(x = x_pos, y = volume, fill = vehicle_type)) +
  # Shaded rectangles per next_state (use precomputed color; no legend for background)
  geom_rect(
    data = state_ranges,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    color = NA,
    fill  = bg_colors[as.character(state_ranges$next_state)],
    alpha = 0.20
  ) +
  # Stacked bars by vehicle class
  geom_bar(stat = "identity", width = 0.85) +
  # Optional dashed separators
  geom_vline(
    data = state_boundaries,
    aes(xintercept = xintercept),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "gray55",
    linewidth = 0.3
  ) +
  # Mean speed labels above each full bar
  geom_text(
    data = ordered_df,
    aes(x = x_pos, y = total_volume, label = round(mean_speed, 1)),
    inherit.aes = FALSE,
    vjust = -0.6,
    fontface = "bold",
    size = 3.5
  ) +
  # ---- Labels INSIDE each shaded region ----
  geom_text(
    data = state_ranges,
    aes(x = xlab, y = y_lab_inside, label = next_state),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1,            # top-left inside the band
    fontface = "bold",
    color = "gray15",
    size = 4.2
  ) +
  # Vehicle class palette for the bars
  scale_fill_brewer(
    palette = "Set2",
    name = "Vehicle class",
    labels = c("motorcycle_volume" = "Motorcycle",
               "passenger_volume" = "Passenger",
               "truck_volume" = "Truck")
  ) +
  # X-axis ticks as site_period labels
  scale_x_continuous(
    breaks = ordered_df$x_pos,
    labels = ordered_df$site_period,
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  # Make room above bars for speed labels
  expand_limits(y = y_max * 1.08) +
  labs(
    title = "Traffic Volume Composition and Speed by Upcoming State",
    subtitle = "Bars stacked by vehicle class; background shading reflects next_state (green → red)",
    x = "Site and Period",
    y = "Volume (count)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 10)
  ) +
  coord_cartesian(clip = "off")
