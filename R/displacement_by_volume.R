# R/displacement_by_volume.R
# This file has the helper functions related to analyzing
# TPRS displacement as it relates to vehicle volume.

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

#' Compute cumulative Wavetronix volumes per date with 85th-percentile speed
#'
#' @param combined_df data.frame or tibble. Combined Wavetronix measurements.
#'   Expected columns include at least: `site`, `date`, `time`, `unit`,
#'   `lane`, `volume`, and `speed_85`.
#' @param observation_data NULL, a data.frame/tibble, or a character path to a
#'   CSV. If provided, must include `site`, `date`, and `spacing_type`. When
#'   supplied the function will join spacing_type into the result.
#' @param lane_value character. Lane identifier to filter (defaults to "01").
#'   The function accepts variants with or without leading zeros.
#' @param unit_value character. Unit identifier to filter (defaults to "w1").
#' @return A tibble with columns: `site`, `date`, `time` (string "HHMMSS"),
#'   `cumulative` (running sum of volumes), `speed` (85th percentile speed for
#'   the day; NA if none), and `spacing_type` (joined from observation_data or
#'   NA if not provided).
cumulate_volume <- function(combined_df, observation_data = NULL, lane_value = "01", unit_value = "w1") {
  # prepare observations table if provided (accepts path or data.frame)
  obs_df <- NULL
  if (!is.null(observation_data)) {
    if (is.character(observation_data) && length(observation_data) == 1) {
      obs_df <- readr::read_csv(observation_data, show_col_types = FALSE)
    } else {
      obs_df <- observation_data
    }
    obs_df <- obs_df %>%
      dplyr::select(site, date, spacing_type)
  }

  combined_df %>%
    # filter to the unit/lane of interest (defaults to w1 lane 01)
    dplyr::filter(unit == unit_value) %>%
    dplyr::filter(stringr::str_detect(lane, paste0("^0*", lane_value, "$"))) %>%
    # ensure numeric columns
    dplyr::mutate(
      volume = as.numeric(volume),
      speed_85 = as.numeric(speed_85)
    ) -> df

  # per-site, per-day total by timestamp
  daily_by_time <- df %>%
    dplyr::group_by(site, date, time) %>%
    dplyr::summarise(total = sum(volume, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(site, date, time) %>%
    dplyr::group_by(site, date) %>%
    dplyr::mutate(cumulative = cumsum(total)) %>%
    dplyr::ungroup()

  # compute 85th percentile speed per site per day
  daily_speed <- df %>%
    dplyr::group_by(site, date) %>%
    dplyr::summarise(
      speed = if(all(is.na(speed_85)))
          NA_real_
        else
          as.numeric(stats::quantile(speed_85, probs = 0.85, na.rm = TRUE)),
      .groups = "drop"
    )

  result <- daily_by_time %>%
    dplyr::left_join(daily_speed, by = c("site", "date"))

  if (!is.null(obs_df)) {
    # preserve obs_df's spacing_type type by joining directly
    result <- result %>% dplyr::left_join(obs_df, by = c("site", "date"))
  } else {
    # add a placeholder column when no observations are provided
    result <- result %>% dplyr::mutate(spacing_type = NA)
  }

  result %>%
    dplyr::mutate(time = format(time, "%H%M%S")) %>%
    dplyr::select(site, date, time, cumulative, speed, spacing_type)
}

#' Compute cumulative class-specific volumes per date
#'
#' @param class_volume data.frame or tibble. Row-wise class volume records.
#'   Expected to include at least: `site`, `date` or `time`, and `class`.
#' @param observation_data NULL, a data.frame/tibble, or a character path to a
#'   CSV. If provided, must include `site`, `date`, and `spacing_type`. When
#'   supplied the function will join spacing_type into the result.
#' @return A tibble with columns: `site`, `time`, `session`, `class`, `cumulative`
#'   (row-wise cumulative count within site/date/class), and `spacing_type`
#'   (joined from observation_data or NA if not provided).
cumulate_class_volume <- function(class_volume, observation_data = NULL) {
  # prepare observations table if provided (accepts path or data.frame)
  obs_df <- NULL
  if (!is.null(observation_data)) {
    if (is.character(observation_data) && length(observation_data) == 1) {
      obs_df <- readr::read_csv(observation_data, show_col_types = FALSE)
    } else {
      obs_df <- observation_data
    }
    obs_df <- obs_df %>%
      dplyr::select(site, date, spacing_type)
  }

  # Define vehicle class groups
  class_volume <- class_volume %>%
    mutate(class = case_when(
      class %in% c("passenger", "motorcycle") ~ "Passenger",
      class == "truck" ~ "Truck",
      TRUE ~ "other"
    ))

  # keep all original rows and add a cumulative counter per site/date/class
  result <- class_volume %>%
    dplyr::arrange(site, date, class, time) %>%
    dplyr::group_by(site, date, class) %>%
    dplyr::mutate(cumulative = dplyr::row_number()) %>%
    dplyr::ungroup()

  if (!is.null(obs_df)) {
    # join observations by site and date
    result <- result %>% dplyr::left_join(obs_df, by = c("site", "date"))
  } else {
    # add a placeholder column when no observations are provided
    result <- result %>% dplyr::mutate(spacing_type = NA)
  }

  result %>%
    dplyr::select(site, time, session, class, cumulative, spacing_type)
}

#' Create and save displacement plots (cumulative volume with TPRS events)
#'
#' @param cumulated_volume data.frame or tibble. Output of cumulate_volume.
#'   Expected columns include: `site`, `date`, `time` (HHMMSS or POSIX), `cumulative`,
#'   `speed`, and `spacing_type`.
#' @param camera_top_data data.frame or tibble. Camera top events containing at
#'   minimum `site`, `time` (POSIXct), and `event` (factor/character).
#' @param output_dir character. Directory where SVG files will be written
#'   (defaults to "output"). The function will create one SVG per site.
#' @return A named list of file paths (character) pointing to the saved SVGs,
#'   names correspond to site values. Side effect: saves SVG files via ggsave.
make_displacement_plot_data <- function(cumulated_volume,
                                        camera_top_data,
                                        output_dir = "output") {

  # Ensure datetime is properly formatted
  cumulated_volume <- cumulated_volume %>%
    mutate(datetime = ymd_hms(paste(date, str_pad(time, 6, pad = "0")))) %>%
    select(site, date, datetime, cumulative, speed, spacing_type)


  # Get unique sites
  sites <- unique(cumulated_volume$site)
  output_paths <- list()

  for (s in sites) {
    wv_site <- cumulated_volume %>% filter(site == s)

    # Get unique spacing_type-date pairs for this site
    spacing_dates <- wv_site %>%
      arrange(date) %>%
      group_by(site, spacing_type) %>%
      slice(1) %>%  # keep only the first date per spacing
      ungroup() %>%
      mutate(strip_label = paste0(spacing_type, " ", speed, " mph"),
            strip_label = fct_inorder(strip_label)) %>%
      select(spacing_type, target_date = date, strip_label)


    # Filter wavetronix to only include rows matching spacing-date pairs
    wv_filtered <- wv_site %>%
      inner_join(spacing_dates,
                by = c("spacing_type", "date" = "target_date")) %>%
      rename(time = datetime)

    # Filter camera data to same site and matching dates
    cam_filtered <- camera_top_data %>%
      filter(site == s) %>%
      mutate(date = as.Date(time)) %>%
      inner_join(spacing_dates, by = c("date" = "target_date"))

    # Plot
    p <- ggplot() +
      geom_line(data = wv_filtered,
                aes(x = time, y = cumulative, group = date), color = "black") +
      geom_vline(data = cam_filtered,
             aes(xintercept = time, color = event),
             linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
      scale_color_manual(values = c(
        "Reset"                = "#1E822F",
        "Some"        = "#879D35",
        "Moderate"    = "#F0B73B",
        "Significant" = "#E96123",
        "Out of Spec." = "#E10A0A"),
      breaks = c("Passenger", "Truck",
                 "Reset", "Some", "Moderate",
                 "Significant", "Out of Spec.")) +
      facet_wrap(~strip_label, scales = "free_x") +
      labs(x = "Time", y = "Cumulative Volume", color = "TPRS Status") +
      theme_minimal()

    # Save plot
    out_path <- file.path(output_dir, paste0("displacement_plot_", s, ".svg"))
    ggsave(out_path, plot = p, width = 8, height = 4.5)
    output_paths[[s]] <- out_path
  }

  return(output_paths)
}

#' Create displacement plot of cumulative class volumes with TPRS events
#'
#' @param class_volume data.frame or tibble. Output of cumulate_class_volume.
#'   Expected to include `site`, `time`, `class`, `cumulative`, and `spacing_type`.
#' @param camera_top_data data.frame or tibble. Camera top events with at least
#'   `site`, `time` (POSIXct), and `event`.
#' @param site_info character. Site identifier to plot (e.g., "SR-12").
#' @return A ggplot object showing cumulative class volumes (passenger, truck)
#'   faceted by spacing_type with vertical lines for camera events. The plot is
#'   returned but not saved.
make_displacement_plot_class_data <- function(
    class_volume,
    camera_top_data,
    site_info) {

  # Filter class volume data to this site, all days
  cv_site <- class_volume %>% filter(site == site_info)
  # Remove July 11 data if present (no displacement data that day)
  cv_site <- cv_site %>% filter(as.Date(time) != as.Date("2025-07-11"))
  # Filter camera data to this site, all days
  cam_site <- camera_top_data %>% filter(site == site_info)

  # Get unique spacing_type-date pairs for this site
  spacing_dates <- cv_site %>%
    mutate(date = as.Date(time)) %>%
    arrange(date) %>%
    group_by(site, spacing_type) %>%
    slice(1) %>%  # keep only the first date per spacing
    ungroup() %>%
    select(spacing_type, date) %>%
    rename(target_date = date)

  # Add spacing_type to camera data
  cam_site <- cam_site %>%
    mutate(date = as.Date(time)) %>%
    inner_join(spacing_dates, by = c("date" = "target_date"))

  # Plot: two cumulative lines (passenger and truck)
  p <- ggplot() +
    geom_line(data = cv_site,
              aes(x = time, y = cumulative, color = class, group = class),
              linewidth = 1) +
    geom_vline(data = cam_site,
              aes(xintercept = time, color = event),
              linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
    scale_color_manual(values = c(
      "Passenger"    = "blue",
      "Truck"        = "brown",
      "Reset"        = "#1E822F",
      "Some"         = "#879D35",
      "Moderate"     = "#F0B73B",
      "Significant"  = "#E96123",
      "Out of Spec." = "#E10A0A"),
    breaks = c("Passenger", "Truck",
                "Reset", "Some", "Moderate",
                "Significant", "Out of Spec.")) +
    facet_wrap(~spacing_type, scales = "free_x") +
    labs(x = "Time", y = "Cumulative Volume", color = "Legend") +
    theme_minimal()

  # return the plot object
  return(p)
}

#' Save a class-displacement plot to disk as SVG
#'
#' @param site_info character. Site identifier used to build the filename.
#' @param plot ggplot. Plot object to save.
#' @param output_dir character. Directory where the SVG will be written
#'   (defaults to "output").
#' @return Character. File path to the saved SVG.
save_displacement_plots <- function(site_info, plot, output_dir = "output") {
  
  out_path <- file.path(
    output_dir, 
    paste0("displacement_plot_class_", 
    site_info, 
    ".svg")
  )

  ggsave(
    out_path,
    plot = plot,
    width = 8,
    height = 4.5,
    device = "svg"
  )

  return(out_path)
}