# R/displacement_by_momentum.R
# Helper Functionsn related to calculating TPRS Displacement

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

#' Calculate weighted average truck weight from BTS truck counts
#'
#' @param bts_truck_counts data.frame or tibble. Must contain numeric columns
#'   `class`, `count`, and `weight`. Only classes >= 4 are used by this function.
#' @return Numeric. Weighted average truck weight (same units as the `weight`
#'   column) computed as sum(count * weight) / sum(count) for classes >= 4.
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

#' Compile displacement dataset from multiple sources
#'
#' @param wavetronix data.frame or tibble. Wavetronix measurements expected to
#'   include at minimum `unit`, `lane`, `sensor_time`, and `speed`.
#' @param camera_back_data data.frame or tibble. Camera back records expected
#'   to include at minimum `site`, `time`, `class`, and `lane`.
#' @param camera_top_data data.frame or tibble. Camera top events expected to
#'   include at minimum `time` and `event` (state).
#' @param observations data.frame or tibble. Observation records expected to
#'   include at minimum `date` and `spacing_type`.
#' @param motorcycle_weight Numeric. Assumed motorcycle weight in lbs (default 800).
#' @param passenger_weight Numeric. Assumed passenger vehicle weight in lbs (default 4419).
#' @param truck_weight Numeric. Assumed truck weight in lbs (default 40000).
#' @return A tibble. Combined displacement rows with columns: `site`, `time`,
#'   `class`, `speed`, `momentum` (speed * weight proxy), `spacing_type`, and
#'   `state`. Rows with non-positive momentum are filtered out.
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
  
  # Process camera back data, only keep lane 1 to match wavetronix data
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
  select(site, time, class, speed, momentum, spacing_type, state) %>%
  filter(momentum > 0)

  
  return(displacement_data)
}

#' Summarize displacement rows into state-transition summaries
#'
#' @param displacement_data data.frame or tibble. Output of
#'   compile_displacement_data containing at minimum `time`, `site`, `class`,
#'   `speed`, `momentum`, `spacing_type`, and `state`.
#' @return A tibble. One row per detected state period (period_id) containing:
#'   site, date, spacing_type, start_time, end_time, start_state, momentum
#'   (sum, in millions lbs*mi/hr), motorcycle_volume, passenger_volume,
#'   truck_volume, mean_speed, duration (mins), and next_state. Rows that do
#'   not meet filtering criteria (e.g., next_state is Reset or NA, start_state
#'   Out of Spec., duration > 480 mins) are removed.
summarize_displacement_data <- function(displacement_data) {
  # Identify state transitions and number each period of constant state uniquely
  state_change <- displacement_data %>%
    arrange(time) %>%
    mutate(date = as.Date(time),
           prev_state = lag(state),
           state_change = state != prev_state & !is.na(prev_state),
           period_id = cumsum(state_change) +1)
    # The period_id increments each time a state change occurs, so summarizing
    # by period_id will only include one state. No off-by-one errors here.
  
    # Summarize traffic per state transition
  displacement_summary <- state_change %>%
    # For each period... 
    group_by(period_id) %>%
    # ...create one row that summarizes the following:
    summarise(
      site              = first(site),
      date              = first(date),
      spacing_type      = first(spacing_type),
      start_time        = min(time, na.rm = TRUE),
      end_time          = max(time, na.rm = TRUE),
      start_state       = first(state),
      # Convert to millions lbs*mi/hr for easier plotting
      # Converts to metric late in function prep_transition_data()
      momentum            = sum(momentum, na.rm = TRUE) / 1000000, 
      motorcycle_volume = sum(class == "motorcycle", na.rm = TRUE),
      passenger_volume  = sum(class == "passenger",  na.rm = TRUE),
      truck_volume      = sum(class == "truck",      na.rm = TRUE),
      mean_speed        = mean(speed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculate duration and next state for each summary row.
    # duration and next_state cannot be found in the previous summarise step.
    mutate(
      duration = as.numeric(difftime(end_time, start_time, units = "mins")),
      next_state = lead(start_state)
    ) %>%
    # Only keep transitions that meet the following criteria:
    filter(
      # a) don't transition to Reset state
      next_state != "Reset" &
      # b) transition to a known state
      !is.na(next_state) &
      # c) don't start from Out of Spec. state
      start_state != "Out of Spec." &
      # d) have durations of less than 8 hours. (480 minutes)
      # This removes long gaps most likely due to overnight periods.
      duration <= 480)

  return (displacement_summary)
}

#' Filter summarized transitions to valid chains starting at Reset
#'
#' @param displacement_summary data.frame or tibble. Output of
#'   summarize_displacement_data containing at minimum `site`, `date`,
#'   `start_time`, `spacing_type`, `start_state`, `next_state`, and `momentum`.
#' @return A tibble. Filtered transition-level rows (chronological) that form
#'   valid chains: either they start with "Reset" or their start_state equals
#'   the previous row's next_state. The returned tibble includes columns:
#'   site, date, start_time, spacing_type, start_state, next_state, momentum.
filter_displacement_summary <- function(displacement_summary) {
  # Focus on just momentum per transition (exclude volumes and mean speeds)
  # I know we put a lot of effort into summarizing that data, but it's not used.
  # I keep the summary there in case we want to use it later.

  transition_data <- displacement_summary %>%
    # Only keep relevant columns
    select(
      site, date, start_time, spacing_type, start_state, next_state, momentum
    ) %>%
    # Make sure everything is in chronological order
    arrange(date, start_time) %>%
    # For each day...
    group_by(date) %>%
    # ...Only keep series of transitions that start with "Reset",
    # OR where start_state is the same as the previous row's next_state.
    # We want unbroken chains of transitions that start from Reset.
    mutate(
      # Create a helper column 'valid' which is a true/false flag.
      valid = accumulate( # Gotta be honest, not sure how it works, but it does.
        .x = row_number(),# Iterate over each row in the day, and check validity
        .f = function(prev_valid, i) {
          # Are we looking at the first row?
          if (i == 1) {
            # Yes? Then it's only valid if it starts with Reset.
            # Set valid value accordingly and skip the remaining checks.
            return(start_state[i] == "Reset")
          } # No? Go to the next check.

          # Is the previous row valid?
          if (!prev_valid) {
            # No? Is it a Reset row? No? Then it's invalid.
            return(start_state[i] == "Reset")
          } # Yes? Go to the next check.


          # Is this row a Reset row?
          if (start_state[i] == "Reset") {
            # Yes? Always valid. Skip remaining check.
            return(TRUE)
          } # no? Go to the next check.

          # Is this row's start_state the same as the previous row's next_state?
          # Yes? Then it's valid. No? Then it's invalid.
          return(start_state[i] == next_state[i - 1])
        },
        # This initializes the accumulate with a TRUE value for the first call.
        # Not sure why it's needed, but it is.
        .init = TRUE
    )[-1] # remove the initial TRUE used to start accumulate
    ) %>%
     # Only keep rows we marked as valid
    filter(valid) %>%
    # Remove helper 'valid' column
    select(-valid) %>%
    ungroup()

    # A unique identifier gets added later in prep_transition_data()
  
  return(transition_data)
}

#' Prepare transition summaries for plotting (add resets, cumulative momentum)
#'
#' @param transition_data data.frame or tibble. Output of
#'   filter_displacement_summary containing at minimum `site`, `date`,
#'   `start_time`, `spacing_type`, `start_state`, `next_state`, and `momentum`.
#' @return A tibble. Plot-ready rows including explicit "Reset" rows,
#'   `segment_id`, `cum_momentum` (cumulative momentum per segment), converted
#'   momentum units (kg*m/s), and `state_jitter` for plotting x-axis separation.
prep_transition_data <- function(transition_data) {
  # Take in the filtered transition_data provided by filter_displacement_summary
  # and prepare it for plotting

  # Step 1: make the basis for the final tibble, plot_data
  # This is just the transition_data with some columns renamed.
  # next_state becomes state, and we only keep relevant columns.
  base_rows <- transition_data %>%
    transmute(
      site,
      date,
      start_time,
      spacing_type,
      state = next_state,
      momentum
    )

  # Step 2: Create rows where state = "Reset" and momentum = 0.0.
  # These will get added to base_rows to create the full plot data.
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
    arrange(site, date, start_time) %>%
    # create unique segment_id so they start at Reset. Used in later plotting.
    mutate(segment_id = cumsum(state == "Reset")) %>%
    # Add cumulative momentum column and keeping previous momentum column.
    group_by(site, date, spacing_type, segment_id) %>%
    arrange(start_time, .by_group = TRUE) %>%
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

  # Apply jitter calculation to shift the whole segment left/right such that
  # segments stay continuous lines.
  disp_plot_data <- disp_plot_data %>%
    group_by(state) %>%
    mutate(
      seg_idx = as.numeric(factor(segment_id)),
      seg_center = mean(unique(seg_idx)),
      state_jitter = as.numeric(state) + (seg_idx - seg_center) * jitter_width
    ) %>%
    ungroup()

  return(disp_plot_data)
}

#' Plot cumulative momentum per transition
#'
#' @param plot_data data.frame or tibble. Output of prep_transition_data;
#'   expected to include `state_jitter`, `cum_momentum`, `site`, `date`,
#'   `segment_id`, and `state`.
#' @param color_by_site logical. If TRUE (default) color lines by `site`,
#'   otherwise color by `spacing_type`.
#' @return A ggplot object. Line/point plot of cumulative momentum across
#'   displacement states, with x-axis labels set to state levels and a legend
#'   indicating the chosen color grouping.
plot_momentum <- function(plot_data, color_by_site = TRUE) {
  # Decide which column to color by + legend title
  if (color_by_site) {
    column_name  <- "site"
    column_label <- "Site"
  } else {
    column_name  <- "spacing_type"
    column_label <- "Spacing Type"
  }

  # Ensure state labels are stable
  state_levels <- levels(as.factor(plot_data$state))

  p <- ggplot(
    plot_data,
    aes(
      x = state_jitter,
      y = cum_momentum,  # change to 'momentum' here if needed
      color = .data[[column_name]],
      group = interaction(site, date, segment_id)
    )
  ) +
    geom_line(linewidth = 0.7, alpha = 0.5) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = sort(unique(as.numeric(plot_data$state))),
      labels = state_levels
    ) +
    labs(
      x = "Displacement",
      y = "Cumulative Momentum (million kg*m/s)",
      color = column_label
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p
}