# This file has the helper functions related to analyzing
# TPRS displacement as it relates to vehicle volume.

# Return cummulative wavetronix volumes per date with average speed per date
# tibble with columns: site, date, time, cumulative, speed
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
    dplyr::group_by(site, date, time = sensor_time) %>%
    dplyr::summarise(total = sum(volume, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(site, date, time) %>%
    dplyr::group_by(site, date) %>%
    dplyr::mutate(cumulative = cumsum(total)) %>%
    dplyr::ungroup()

  # compute 85th percentile speed per site per day
  daily_speed <- df %>%
    dplyr::group_by(site, date) %>%
    dplyr::summarise(
      speed = if (all(is.na(speed_85))) NA_real_ else as.numeric(stats::quantile(speed_85, probs = 0.85, na.rm = TRUE)),
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

# Return cumulative class volumes per date
# tibble with columns: site, date, time, class, cumulative, spacing_type
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

# Plots cumulative volume and TPRS displacement events using wavetronix data
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
      mutate(spacing_type = fct_relevel(spacing_type,
                                        "NO TPRS",
                                        "UDOT",
                                        "PSS",
                                        "LONG")) %>%
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
        "Some Movement"        = "#879D35",
        "Moderate Movement"    = "#F0B73B",
        "Significant Movement" = "#E96123",
        "Out of Specification" = "#E10A0A"),
      breaks = c("Passenger", "Truck",
                 "Reset", "Some Movement", "Moderate Movement",
                 "Significant Movement", "Out of Specification")) +
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

# Plots cumulative class volumes and TPRS displacement events using cb data
make_displacement_plot_class_data <- function(class_volume, camera_top_data, output_dir = "output") {

  # Get unique sites
  sites <- unique(class_volume$site)

  # Make an empty list to store output paths
  output_paths <- list()

  for (s in sites) {
    # Filter class volume data to this site, all days
    cv_site <- class_volume %>% filter(site == s)
    # Remove July 11 data if present (no displacement data that day)
    cv_site <- cv_site %>% filter(as.Date(time) != as.Date("2025-07-11"))
    # Filter camera data to this site, all days
    cam_site <- camera_top_data %>% filter(site == s)

    # Get unique spacing_type-date pairs for this site
    spacing_dates <- cv_site %>%
      mutate(date = as.Date(time)) %>%
      arrange(date) %>%
      mutate(spacing_type = fct_relevel(spacing_type,
                            "NO TPRS", "UDOT", "PSS", "LONG")) %>%
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
        "Passenger"            = "blue",
        "Truck"                = "brown",
        "Reset"                = "#1E822F",
        "Some Movement"        = "#879D35",
        "Moderate Movement"    = "#F0B73B",
        "Significant Movement" = "#E96123",
        "Out of Specification" = "#E10A0A"),
      breaks = c("Passenger", "Truck",
                 "Reset", "Some Movement", "Moderate Movement",
                 "Significant Movement", "Out of Specification")) +
      facet_wrap(~spacing_type, scales = "free_x") +
      labs(x = "Time", y = "Cumulative Volume", color = "Legend") +
      theme_minimal()

    # Save plot
    out_path <- file.path(output_dir, paste0("class_displacement_plot_", s, ".svg"))
    ggsave(out_path, plot = p, width = 8, height = 4.5)
    output_paths[[s]] <- out_path
  }

  return(output_paths)

}