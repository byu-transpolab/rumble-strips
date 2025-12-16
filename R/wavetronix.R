library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

#' Read in a single raw file from Wavetonix
#' 
#' @param file_path path to Wavetronix file
#' 
read_wavetronix <- function(file_path) {

  path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(file_path)), "-"))
  road <- path_elements[2]
  unit <- path_elements[3]

  # Read the file
  df <- readr::read_csv(file_path, skip = 2, skip_empty_rows = TRUE) 

  df |>
    dplyr::transmute(
      site = road,
      unit = unit,
      lane = stringr::str_remove(`LANE/APPROACH NAME`, "LANE_"),
      volume = VOLUME,
      occupancy = `OCCUPANCY(%)`,
      speed = `SPEED (mph)`,
      speed_85 = `85% SPEED (mph)`,
      headway = HEADWAY,
      gap = GAP,
      sensor_time = lubridate::as_datetime(`SENSOR TIME (MM/dd/yy  HH:mm:ss)`, format = "%m/%d/%y %H:%M:%S"),
      date = lubridate::date(sensor_time),
      interval = `INTERVAL (sec)`
  ) |>
    dplyr::filter(lane != "03")

}


#' Read in all Wavetronix files in a folder
#' 
#' @param folder_path path to Wavetronix folder
#' 
read_wavetronix_folder <- function(folder_path) {
  
  # Read all files in the folder
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)


  
  # Read each file
  dfs <- purrr::map(files, read_wavetronix) |>
    # Combine into a single data frame
    dplyr::bind_rows()
  
  dfs
}

read_observations <- function(file_path) {

  read_csv(file_path) |>
    mutate(
      spacing_type = ifelse(is.na(spacing_type), 0, spacing_type),
      spacing_type = as_factor(spacing_type),
      date = lubridate::mdy(date))


}


### Helper functions for reading camera top files and wavetronix files

# depreciated
# return per-date aggregated wavetronix (time, total, cumulative)
get_wavetronix_for_date <- function(combined_df, date_code, lane_value = "1") {
  target_date <- as.Date(date_code, format = "%Y%m%d")
  combined_df %>%
    filter(date == target_date) %>%
    # keep only requested lane (accept "1" or "01", etc.)
    filter(stringr::str_detect(lane, paste0("^0*", lane_value, "$"))) %>%
    group_by(time = sensor_time) %>%
    summarise(total = sum(as.numeric(volume), na.rm = TRUE), .groups = "drop") %>%
    arrange(time) %>%
    mutate(cumulative = cumsum(total))
}

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

# read camera_top files (time,event)
read_camera_top <- function(path) {

  path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(path)), "-"))
  # get the date code and site from the file name
  date_code <- path_elements[1]
  site <- path_elements[2]


  readr::read_csv(path, col_names = c("time", "event"), 
    show_col_types = FALSE) |>
    mutate(
      site = site,
      time = str_c(date_code, time, sep = " "),
      # remove the last 3 digits of the time
      # because they are the milliseconds
      time = substr(time, start = 1, stop= nchar(time) - 4),
      time = lubridate::as_datetime(time, format = "%Y%m%d %H:%M:%S"),
      event = case_when(
        event == "o" ~ "No movement",
        event == "i" ~ "Movement detected",
        event == "u" ~ "Ineffective placement"
      )
      
    )

}

get_camera_top_data <- function(folder_path) {
  # Read all files in the folder
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

  # Read each file
  dfs <- purrr::map(files, read_camera_top) |>
    # Combine into a single data frame
    dplyr::bind_rows()
  
  dfs
}

#' Read a single camera_back CSV file
#' 
#' @param path path to camera_back CSV file (e.g., 20250708_sr12_cb_B.csv)
#' 
read_camera_back <- function(path) {
  # Extract date, site and session data from filename
  # Example: 20250708_sr12_cb_B.csv
  path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(path)), "_"))
  date_code <- path_elements[1]
  site <- path_elements[2]
  session <- path_elements[4]

  # Read the CSV file
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # Remove rows with NA in class column
  df <- df %>% dplyr::filter(!is.na(class))

  # Parse timestamp with date_code and convert to POSIXct
  df %>%
    dplyr::mutate(
      site = site,
      session = session,
      date = as.Date(date_code, format = "%Y%m%d"),
      # normalize milliseconds separator (HH:MM:SS:MMM -> HH:MM:SS.MMM)
      timestamp = sub(":(\\d{1,3})$", ".\\1", timestamp),
      # combine date_code and time string for full datetime
      time = as.POSIXct(paste0(date_code, " ", timestamp),
                        format = "%Y%m%d %H:%M:%OS",
                        tz = Sys.timezone()),

      # interpret j, k, l as passenger, truck, motorcycle
      class = case_when(
        class == "j" ~ "passenger",
        class == "k" ~ "truck",
        class == "l" ~ "motorcycle",
        TRUE ~ as.character(class)
      ),
      # interpret d, f, NA as before, after, no brake
      brake = case_when(
        brake == "d" ~ "before",
        brake == "f" ~ "after",
        is.na(brake) ~ "no brake",
        TRUE ~ as.character(brake)
      ),
      # interpret b , NA as avoided, not avoided
      departure = case_when(
        avoidance == "b" ~ "avoided",
        is.na(avoidance) ~ "not avoided",
        TRUE ~ as.character(avoidance)
      ),
      # interpret y as yes, NA as no
      flagged = case_when(
        displacement == "y" ~ "yes",
        is.na(displacement) ~ "no",
        TRUE ~ as.character(displacement)
      ),
      # interpret '9' as lane 2, NA as lane 1
      lane = case_when(
        state == "9" ~ "lane 2",
        is.na(state) ~ "lane 1",
        TRUE ~ as.character(state)
      )
    ) %>%
    dplyr::select(site, date, time, session, class, brake, departure, flagged, lane)
}

# Read camera back data from folder and return combined dataframe
get_camera_back_data <- function(folder_path) {
   # Accept either a single folder path or a character vector of file paths
  if (length(folder_path) == 1 && dir.exists(folder_path)) {
    files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  } else {
    files <- folder_path
  }

  # Exclude cb_offsets.csv
  files <- files[!grepl("cb_offsets\\.csv$", files)]

  # Ensure we have files
  if (length(files) == 0) {
    return(tibble(
      site = character(),
      date = as.Date(character()),
      time = as.POSIXct(character()),
      session = character(),
      class = character(),
      brake = character(),
      departure = character(),
      flagged = character(),
      lane = character()
    ))
  }

  # Read each file
  dfs <- purrr::map(files, read_camera_back) |>
    # Combine into a single data frame
    dplyr::bind_rows()
  
  # Apply offsets to timestamps
  dfs <-add_offsets_to_cb(dfs)

  dfs
}

# Find how much time passed between session A and B in camera_back_data
time_between_sessions <- function(cb_data) {
  cb_data %>%
    group_by(site, date) %>%
    summarise(
      last_A   = if (any(session == "A")) max(time[session == "A"]) else NA,
      first_B  = if (any(session == "B")) min(time[session == "B"]) else NA,
      .groups = "drop"
    ) %>%
    # keep only rows where both A and B exist
    filter(!is.na(last_A) & !is.na(first_B)) %>%
    # calculate time difference in minutes
    mutate(
      diff_mins  = as.numeric(difftime(first_B, last_A, units = "mins"))
    )
}

# List which sessions do not have the correct time offset
missing_offsets <- function(cb_data) {
  cb_data %>%
    group_by(date, site, session) %>%
    # find the first timestamp of each session
    summarise(
      first_time = min(time),
      .groups = "drop"
    ) %>%
    # extract time-of-day
    mutate(first_tod = hms::as_hms(first_time)) %>%
    # keep only rows where the first time-of-day <= 01:00:00
    filter(first_tod <= hms::as_hms("01:00:00")) %>%
    select(site, date, session, first_time)

}

# Add Offsets to camera back data
add_offsets_to_cb <- function(cb_data, 
                              offsets_csv = "data/camera_back/cb_offsets.csv") {
  # read offsets file
  offsets <- read_csv(offsets_csv, col_types = cols(
    date = col_date(),
    session = col_character(),
    offset = col_time()
  ))
  
  # compute the adjustment needed for each session
  adjustments <- cb_data %>%
    group_by(date, session) %>%
    summarise(first_time = min(time), .groups = "drop") %>%
    left_join(offsets, by = c("date", "session")) %>%
    mutate(
      adjustment = if_else(!is.na(offset),
                           as.numeric(difftime(offset, 
                           hms::as_hms(first_time), 
                           units = "secs")),
                           NA_real_)
    )
  
  # apply the adjustment back to cb_data
  cb_data %>%
    left_join(adjustments %>% select(date, session, adjustment),
              by = c("date", "session")) %>%
    mutate(
      time = if_else(!is.na(adjustment),
                     time + seconds(adjustment),
                     time)
    ) %>%
    select(-adjustment)
}

# Plots cumulative volume and TPRS displacement events using wavetronix data
make_displacement_plot_data <- function(wavetronix,
                                        camera_top_data,
                                        output_dir = "output") {

  # Ensure datetime is properly formatted
  wavetronix <- wavetronix %>%
    mutate(datetime = ymd_hms(paste(date, str_pad(time, 6, pad = "0"))))


  # Get unique sites
  sites <- unique(wavetronix$site)
  output_paths <- list()

  for (s in sites) {
    wv_site <- wavetronix %>% filter(site == s)

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
  select(site, spacing_type, date, strip_label) %>%
  rename(target_date = date)


    # Filter wavetronix to only include rows matching spacing-date pairs
    wv_filtered <- wv_site %>%
      inner_join(spacing_dates, by = c("spacing_type", "date" = "target_date"))

    # Filter camera data to same site and matching dates
    cam_filtered <- camera_top_data %>%
      filter(site == s) %>%
      mutate(date = as_date(time)) %>%
      inner_join(spacing_dates, by = c("site", "date" = "target_date"))

    # Plot
    p <- ggplot() +
      geom_line(data = wv_filtered,
                aes(x = datetime, y = cumulative, group = date), color = "black") +
      geom_vline(data = cam_filtered,
             aes(xintercept = time, color = event),
             linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
      scale_color_manual(values = c(
        "No movement" = "forestgreen",
        "Movement detected" = "orange",
        "Ineffective placement" = "red"),
        breaks = c("No movement", "Movement detected", "Ineffective placement")) +
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
    cv_site <- class_volume %>% filter(site == s)
    cam_site <- camera_top_data %>% filter(site == s)

    # Get unique spacing_type-date pairs for this site
    spacing_dates <- cv_site %>%
    arrange(date) %>%
    mutate(spacing_type = fct_relevel(spacing_type, "NO TPRS", "UDOT", "PSS", "LONG")) %>%
    group_by(site, spacing_type) %>%
    slice(1) %>%  # keep only the first date per spacing
    ungroup() %>%
    select(spacing_type, date) %>%
    rename(target_date = date)

    # Plot: two cumulative lines (passenger and truck)
    p <- ggplot() + 
      geom_line(data = cv_site,
                aes(x = time, y = cumulative, color = class, group = class),
                linewidth = 1) +
      #geom_vline(data = cam_site,
      #          aes(xintercept = time, color = event),
      #          linetype = "dashed", alpha = 0.7, linewidth = 0.8) +
      scale_color_manual(values = c(
        "Passenger" = "blue",
        "Truck" = "brown",
        "No movement" = "forestgreen",
        "Movement detected" = "orange",
        "Ineffective placement" = "red"),
        breaks = c("Passenger", "Truck", 
                  "No movement", "Movement detected", "Ineffective placement")) +
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


## Statistical analysis of speed ##

# prepare tibble for statistical tests
prepare_speed_data <- function(wavetronix, observations) {
  wavetronix %>%
    dplyr::filter(lane == "01") %>%
    select(site, unit, date, time = sensor_time, speed_85) %>%
    mutate(unit = as.factor(as.character(unit)),
           speed_85 = as.numeric(speed_85)) %>%
    left_join(observations %>% select(site, date, spacing_type), by = c("site", "date")) %>%
    filter(!is.na(speed_85), !is.na(spacing_type)) %>%
    mutate(spacing_type = fct_relevel(spacing_type, "NO TPRS", "UDOT", "PSS", "LONG"))
}


# perform t-tests on speed_85 grouped by site, unit, spacing_type
paired_test <- function(speed_data) {
  grouped <- speed_data %>%
    group_by(site, spacing_type) %>%
    nest()
  
  results <- grouped %>%
    mutate(
      t_test = map(data, ~ {
        wide <- .x %>%
          select(time, unit, speed_85) %>%
          pivot_wider(names_from = unit, values_from = speed_85) %>%
          filter(!is.na(w1), !is.na(w2))
        
        if ("w1" %in% names(wide) && "w2" %in% names(wide) &&
            nrow(wide) >= 2) {
          test <- tryCatch(
            t.test(wide$w2, wide$w1, paired = TRUE),
            error = function(e) NULL
          )
          
          if (!is.null(test)) {
            tibble(
              statistic = test$statistic,
              p_value = test$p.value,
              mean_diff = test$estimate,
              conf_low = test$conf.int[1],
              conf_high = test$conf.int[2]
            )
          } else {
            tibble(statistic = NA, p_value = NA, mean_diff = NA,
                   conf_low = NA, conf_high = NA)
          }
        } else {
          tibble(statistic = NA, p_value = NA, mean_diff = NA,
                 conf_low = NA, conf_high = NA)
        }
      })
    ) %>%
    unnest(t_test)

  results
}

plot_confidence_bounds <- function(paired_t_test) {
  # Prepare data
  plot_data <- paired_t_test %>%
    mutate(spacing_type = as.factor(spacing_type),
    site = case_when(
      site == "sr12" ~ "SR-12",
      site == "us6" ~ "US-6",
      site == "i70" ~ "I-70",
      site == "us191" ~ "US-191",
      TRUE ~ site)
    ) %>%
    filter(!is.na(conf_low), !is.na(conf_high))

  # Build plot
  p <- ggplot(plot_data, aes(y = site, 
                             x = mean_diff, 
                             color = spacing_type)) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                  orientation = "y",
                  position = position_dodge(width = 0.6),
                  height = 0.4,
                  linewidth = 1.2) +
    geom_point(position = position_dodge(width = 0.6), size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c(
      "NO TPRS" = "#2E4756",
      "UDOT" = "#3C7A89",
      "PSS" = "#F49D37",
      "LONG" = "#D81159" ),
      breaks = c("LONG", "PSS", "UDOT", "NO TPRS")) +
    labs(
      x = "Speed Difference",
      y = "Site",
      color = "Spacing Type"
    )

  # Save and return
  ggsave("output/confidence_bounds.svg", plot = p, width = 10, height = 8)
  p
}

