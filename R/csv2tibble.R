# This file holds the R functions used to convert the raw csv
# files into tibbles for additional processing.

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)

### observation_data.csv ###############################################

read_observations <- function(file_path) {

  read_csv(file_path) |>
    mutate(
      spacing_type = ifelse(is.na(spacing_type), 0, spacing_type),
      spacing_type = factor(spacing_type,
      levels = c("NO TPRS", "UDOT", "PSS", "LONG")),
      date = lubridate::mdy(date))

}

### Wavetronix data ##################################################

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
      sensor_time = lubridate::as_datetime(`SENSOR TIME (MM/dd/yy  HH:mm:ss)`,
                    format = "%m/%d/%y %H:%M:%S",
                    tz = "America/Denver"),
      date = lubridate::date(sensor_time),
      interval = `INTERVAL (sec)`
  ) |>
    dplyr::filter(lane != "03")

}

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

### Camera top data ##################################################

# expects a vector of files and returns a combined dataframe
get_camera_top_data <- function(folder_path) {
 # Accept either a single folder path or a character vector of file paths
  if (length(folder_path) == 1 && dir.exists(folder_path)) {
    files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  } else {
    files <- folder_path
  }

  # Read each file
  dfs <- purrr::map(files, read_camera_top) |>
    # Combine into a single data frame
    dplyr::bind_rows()

  dfs <- dfs %>%
    arrange(time) %>%
    filter(event != lag(event) | is.na(lag(event)))
  
  dfs
}

# read camera_top files (time,event)
read_camera_top <- function(path) {

  path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(path)), "-"))
  # get the date code and site from the file name
  date_code <- path_elements[1]
  site <- path_elements[2]


  df<-read_csv(path, show_col_types = FALSE)
  
  df %>%
    mutate(
      site = site,
      time = as.POSIXct(paste0(date_code, " ", timestamp),
                        format = "%Y%m%d %H:%M:%OS",
                        tz = "America/Denver"),
      event = case_when(
        state == "0" ~ "Reset",
        state == "1" ~ "Some Movement",
        state == "2" ~ "Moderate Movement",
        state == "3" ~ "Significant Movement",
        state == "4" ~ "Out of Specification",
        TRUE ~ as.character(state)
      ),
      event = factor(event,
        levels = c(
          "Reset",
          "Some Movement",
          "Moderate Movement",
          "Significant Movement",
          "Out of Specification"))
    ) %>%
    select(site, time, event)
}

### Camera back data ##################################################

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

  # Read each file
  dfs <- purrr::map(files, read_camera_back) |>
    # Combine into a single data frame
    dplyr::bind_rows()
  
  # Apply offsets to timestamps
  dfs <-add_offsets_to_cb(dfs)

  # Remove flagged entries, and then the column itself
  dfs <- dfs %>%
    dplyr::filter(flagged != "yes") %>%
    dplyr::select(-flagged)

  dfs
}

#' Read a single camera_back CSV file
#' 
#' @param path path to camera_back CSV file (e.g., 20250708_sr12_cb_B.csv)
#' 
read_camera_back <- function(path) {
  # Extract date, site and session data from filename
  # Example: 20250708_sr12_cb_B.csv
  path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(path)), "-"))
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
                        tz = "America/Denver"),

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
  
  # join offsets and apply them directly
  cb_data %>%
    left_join(offsets, by = c("date", "session")) %>%
    mutate(
      time = if_else(!is.na(offset),
                     time + hms::as_hms(offset),
                     time)
    ) %>%
    select(-offset)

}

### Worker exposure data ##################################################

get_worker_exposure_data <- function(folder_path, observations) {
   # Accept either a single folder path or a character vector of file paths
  if (length(folder_path) == 1 && dir.exists(folder_path)) {
    files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
  } else {
    files <- folder_path
  }

  # Read each file
  dfs <- purrr::map(files, function(path) {
    # Extract date from filename
    path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(path)), "-"))
    date_code <- path_elements[1]

    # Find site from observations data
    site <- observations %>%
      filter(date == as.Date(date_code, format = "%Y%m%d")) %>%
      pull(site) %>%
      unique()

    df <- readr::read_csv(path, show_col_types = FALSE)

    df %>%
      dplyr::mutate(
        site = site,
        date = as.Date(date_code, format = "%Y%m%d"),
        event = case_when(  
          brake == "d" ~ "Arrival",
          brake == "f" ~ "Departure",
          avoidance == "b" ~ "Vehicle Passing",
          class == "j" ~ "Waiting for Gap",
          class == "k" ~ "Entering Roadway",
          class == "l" ~ "Exiting Roadway",
          TRUE ~ as.character(state)
        ),
        # normalize milliseconds separator
        timestamp = sub(":(\\d{1,3})$", ".\\1", timestamp),
        time = as.POSIXct(paste0(date_code, " ", timestamp),
                          format = "%Y%m%d %H:%M:%OS",
                          tz = "America/Denver")
      ) %>%
      dplyr::select(site, date, time, event) %>%
      # Filter out numeric events (the event counting software
      # used 0-9 for internal purposes)
      dplyr::filter(!event %in% as.character(0:9))
  }) |>
    # Combine into a single data frame
    dplyr::bind_rows()
  
  dfs
}

