# This file holds the R functions used to convert the raw csv
# files into tibbles for additional processing.

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)

### Filter Rainy Days #################################################

#' Check whether to remove certain rainy days from a tibble
#' 
#' @param df a tibble with column 'date'
#' @param list with a boolean and a list of rainy days in date format.
filter_rainy_days <- function(df, exclude_rainy_days) {
  # Break out the individual items in the list
  exclude_rain <- exclude_rainy_days[[1]]
  rainy_days <- exclude_rainy_days[[2]]

  # if we are not excluding rain days, return the df and exit
  if (!exclude_rain) return df

  # observations has column: date
  # wavetronix has column:   date
  # camera_top has column:  'time' ddtm
  # camera_top has column:   date

  # create a date column if df doesn't already have one
  
  # Ensure there is a date column
  if (!("date" %in% names(df))) { # is there not a date column?
    if ("time" %in% names(df)) {  # is there a time column?
      df <- df %>% mutate(date = as.Date(time)) # make date from time column
    } else { # if neither exist, throw an error
      stop("Rainy Days: the data frame must contain either 'date' or 'time' column.")
    }
  }


  # if we've gotten here, it's because exclude rain is TRUE
  # so filter to only include days not in the list of rainy_days.
  df %>% 
    filter(!(date %in% rainy_days))
}

### observation_data.csv ###############################################

#' Read observation CSV into a tibble
#'
#' @param file_path Character. Path to the observations CSV file.
#' @return A tibble with the observation rows. Columns are adjusted/cleaned
#'   (spacing_type recoded, site standardized, and date parsed to Date).
read_observations <- function(file_path) {

  read_csv(file_path) |>
    mutate(
      # Rename PSS to 1:2 spacing since it's not the official recommendation
      spacing_type = fct_recode(spacing_type, "1:2" = "PSS"),
      # Assign factor levels to spacing_type
      spacing_type = fct_relevel(
        spacing_type,
        "NO TPRS",
        "UDOT",
        "1:2",
        "LONG"),
      # Add capitalization and hyphens to site names
      site = case_when(
        site == "sr12" ~ "SR-12",
        site == "i70" ~ "I-70",
        site == "us191" ~ "US-191",
        site == "us6" ~ "US-6",
        TRUE ~ as.character(site)
      ),
      date = lubridate::mdy(date)
    )

}

### Wavetronix data ##################################################

#' Read in all Wavetronix files in a folder
#'
#' @param folder_path Character. Path to a folder containing Wavetronix CSV files
#'   or a character vector of file paths.
#' @return A tibble combining all Wavetronix files in the folder.
read_wavetronix_folder <- function(folder_path) {
  
  # Read all files in the folder
  files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

  
  
  # Read each file
  dfs <- purrr::map(files, read_wavetronix) |>
    # Combine into a single data frame
    dplyr::bind_rows()
  
  dfs
}

#' Read a single raw Wavetronix file
#'
#' @param file_path Character. Path to a single Wavetronix CSV file.
#' @return A tibble with columns: site, unit, lane, volume, occupancy, speed,
#'   speed_85, headway, gap, sensor_time (POSIXct), date (Date), interval.
read_wavetronix <- function(file_path) {

  path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(file_path)), "-"))
  road <- path_elements[2]
  unit <- path_elements[3]

  # Read the file
  df <- readr::read_csv(file_path, skip = 2, skip_empty_rows = TRUE) 

  df |>
    dplyr::transmute(
      site = case_when(
        road == "sr12" ~ "SR-12",
        road == "i70" ~ "I-70",
        road == "us191" ~ "US-191",
        road == "us6" ~ "US-6",
        TRUE ~ as.character(road)
      ),
      unit = unit,
      lane = stringr::str_remove(`LANE/APPROACH NAME`, "LANE_"),
      volume = VOLUME,
      occupancy = `OCCUPANCY(%)`,
      speed = `SPEED (mph)`,
      speed_85 = `85% SPEED (mph)`,
      headway = HEADWAY,
      gap = GAP,
      sensor_time = lubridate::as_datetime(`SENSOR TIME (MM/dd/yy  HH:mm:ss)`,
                    format = "%m/%d/%y %H:%M:%OS",
                    tz = "America/Denver"),
      date = lubridate::date(sensor_time),
      interval = `INTERVAL (sec)`
  ) |>
    dplyr::filter(lane != "03")

}

#' Aggregate wavetronix for a single date (deprecated)
#'
#' @param combined_df Data frame. Combined Wavetronix data (as produced by
#'   read_wavetronix_folder).
#' @param date_code Character. Date in "%Y%m%d" format identifying the target
#'   day to aggregate.
#' @param lane_value Character. Lane identifier to filter (defaults to "1").
#' @return A tibble with columns time, total (sum of volume) and cumulative
#'   (running total). Deprecated: kept for backward compatibility.
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

#' Read and combine camera top event files from a folder or a list
#'
#' @param folder_path Character. Path to a folder containing camera top CSV files
#'   or a character vector of file paths.
#' @return A tibble with site, time and event for combined files. Consecutive
#'   duplicate events are removed.
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

#' Read a single camera top CSV
#'
#' @param path Character. Path to a camera top CSV file (filename expected to
#'   include a date prefix used to build timestamps).
#' @return A tibble with columns site, time (POSIXct) and event (factor).
read_camera_top <- function(path) {

  path_elements <- unlist(stringr::str_split(tools::file_path_sans_ext(basename(path)), "-"))
  # get the date code and site from the file name
  date_code <- path_elements[1]
  site <- path_elements[2]


  df<-read_csv(path, show_col_types = FALSE)
  
  df %>%
    mutate(
      site = case_when(
        site == "sr12" ~ "SR-12",
        site == "i70" ~ "I-70",
        site == "us191" ~ "US-191",
        site == "us6" ~ "US-6",
        TRUE ~ as.character(site)
      ),
      time = as.POSIXct(paste0(date_code, " ", timestamp),
                        format = "%Y%m%d %H:%M:%OS",
                        tz = "America/Denver"),
      event = case_when(
        state == "0" ~ "Reset",
        state == "1" ~ "Some",
        state == "2" ~ "Moderate",
        state == "3" ~ "Significant",
        state == "4" ~ "Out of Spec.",
        TRUE ~ as.character(state)
      ),
      event = factor(event,
        levels = c(
          "Reset",
          "Some",
          "Moderate",
          "Significant",
          "Out of Spec."))
    ) %>%
    select(site, time, event)
}

### Camera back data ##################################################

#' Read and combine camera back files from a folder or list
#'
#' @param folder_path Character. Path to a folder containing camera back CSV
#'   files or a character vector of file paths.
#' @return A tibble with camera back records; offsets from offsets CSV are
#'   applied and flagged records are removed.
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
#' @param path Character. Path to a camera_back CSV file (e.g.,
#'   20250708_sr12_cb_B.csv).
#' @return A tibble with columns: site, date (Date), time (POSIXct), session,
#'   class (factor), brake (factor), departure (factor), flagged (character),
#'   lane (character).
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
    mutate(
      site = case_when(
        site == "sr12" ~ "SR-12",
        site == "i70" ~ "I-70",
        site == "us191" ~ "US-191",
        site == "us6" ~ "US-6",
        TRUE ~ as.character(site)
      ),
      session = session,
      date = as.Date(date_code, format = "%Y%m%d"),
      # normalize milliseconds separator (HH:MM:SS:MMM -> HH:MM:SS.MMM)
      timestamp = sub(":(\\d{1,3})$", ".\\1", timestamp),
      # combine date_code and time string for full datetime
      time = as.POSIXct(paste0(date_code, " ", timestamp),
                        format = "%Y%m%d %H:%M:%OS",
                        tz = "America/Denver"),

      # interpret j, k, l as passenger, truck, motorcycle
      class = factor(
          case_when(
            class == "j" ~ "passenger",
            class == "k" ~ "truck",
            class == "l" ~ "motorcycle",
            TRUE ~ as.character(class)
          ),
          levels = c("motorcycle", "passenger", "truck")
      ),

      # interpret d, f, NA as before, after, no brake
      brake = factor(
          case_when(
            brake == "d" ~ "before",
            brake == "f" ~ "after",
            is.na(brake) ~ "no brake",
            TRUE ~ as.character(brake)
          ),
          levels = c("before", "no brake", "after")
      ),

      # interpret b , NA as avoided, not avoided
      departure = factor(
          case_when(
            avoidance == "b" ~ "avoided",
            is.na(avoidance) ~ "not avoided",
            TRUE ~ as.character(avoidance)
          ),
          levels = c("avoided", "not avoided")
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

#' Find time between session A and B in camera back data
#'
#' @param cb_data Data frame. Camera back data as returned by
#'   get_camera_back_data or read_camera_back.
#' @return A tibble with site, date, last_A, first_B and diff_mins (numeric
#'   minutes between sessions).
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

#' List camera_back sessions with missing/incorrect offsets
#'
#' @param cb_data Data frame. Camera back data with time column.
#' @return A tibble (site, date, session, first_time) for sessions whose first
#'   timestamp is at or before 01:00:00, indicating a likely missing offset.
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

#' Add offsets to camera back timestamps
#'
#' @param cb_data Data frame. Camera back data with columns date and session.
#' @param offsets_csv Character. Path to CSV containing offsets (defaults to
#'   "data/camera_back/cb_offsets.csv"). The offsets file must contain columns
#'   date, session and offset.
#' @return cb_data with the time column adjusted by the matching offset and the
#'   offset column removed.
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

### BTS Truck Counts ######################################################

#' Check for and instruct manual download of BTS truck counts
#'
#' @return Character path to the existing BTS truck counts Excel file if it
#'   already exists; otherwise returns NULL after printing instructions to the
#'   console.
dnld_bts_truck_counts <- function() {
  # check if the file already exists
  output_file <- "data/bts_truck_counts.xlsx"
  if (file.exists(output_file)) {
    message("BTS truck counts file already exists. Skipping download.")
    return(output_file)
  } else {
    # provide instructions to download manually
    url = "https://www.bts.gov/sites/bts.dot.gov/files/2024-08/table_01_22a_082624.xlsx" # nolint
    print("You must download the BTS truck counts data manually due to\n",
          "website restrictions. Please visit the following URL to download:\n",
          url, "\n",
          "and save the file as 'data/bts_truck_counts.xlsx'.")
    return()
  }
}

#' Process BTS truck counts Excel into a tidy tibble
#'
#' @param file_path Character. Path to the BTS truck counts Excel file.
#' @return A tibble with columns: weight (integer, mean of the class), count
#'   (numeric, in thousands) and class (integer truck class).
process_bts_truck_counts <- function(
  file_path = "data/bts_truck_counts.xlsx") {

  # Read the Excel file
  bts_truck_counts <- read_excel(
    path = file_path,
    sheet = "1-22",
    range = "A2:E16",
    col_names = c("weight",  "col2", "col3", "col4", "count")
    ) %>%
    # Select only relevant columns and rename them
    transmute(
      weight = weight, # weight class
      count = count  # count of 1000's of trucks in this weight class
      # Note: keep in 1000's to avoid integer overflow
    ) %>%
    # Remove rows with any NA values
    filter(!is.na(weight) & !is.na(count)) %>%
    # Remove the "All TRUCKS" row
    filter(weight != "ALL TRUCKS") %>%
      mutate(
        # Add a class column based on text found in weight
        class = case_when(
          grepl("Class 1", weight) ~ "1",
          grepl("Class 2", weight) ~ "2",
          grepl("Class 3", weight) ~ "3",
          grepl("Class 4", weight) ~ "4",
          grepl("Class 5", weight) ~ "5",
          grepl("Class 6", weight) ~ "6",
          grepl("Class 7", weight) ~ "7",
          grepl("Class 8", weight) ~ "8",
          TRUE ~ as.character(weight)
        ),
        # Change weight to mean of weights listed
        weight = case_when(
          grepl("Less than 6,001 ", weight) ~  "6000",
          grepl("6,001 to 10,000 ", weight) ~  "8000",
          grepl("10,001 to 14,000", weight) ~ "12000",
          grepl("14,001 to 16,000", weight) ~ "15000",
          grepl("16,001 to 19,500", weight) ~ "17750",
          grepl("19,501 to 26,000", weight) ~ "22750",
          grepl("26,001 to 33,000", weight) ~ "29500",
          # 50,000 lbs is an arbitrary choice to account for trucks with cargo
          grepl("More than 33,000", weight) ~ "50000",
          TRUE ~ as.character(weight)
        )
      ) %>%
      # Convert weight and class to integer
      mutate(
        weight = as.integer(weight),
        class = as.integer(class)
      )

  return(bts_truck_counts)
}

### Worker exposure data ##################################################

#' Read worker exposure CSVs and join with observations to get site
#'
#' @param folder_path Character. Path to a folder of worker exposure CSVs or a
#'   character vector of file paths.
#' @param observations Data frame. Observations tibble used to map date -> site.
#' @return A combined tibble with columns site, date, time and event for worker
#'   exposure entries.
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
      mutate(
        site = case_when(
        site == "sr12" ~ "SR-12",
        site == "i70" ~ "I-70",
        site == "us191" ~ "US-191",
        site == "us6" ~ "US-6",
        TRUE ~ as.character(site)
        ),
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
