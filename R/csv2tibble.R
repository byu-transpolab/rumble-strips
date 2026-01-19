# This file holds the R functions used to convert the raw csv
# files into tibbles for additional processing.

library(tidyverse)
library(readr)
library(readxl)

# Read observation_data csv into a tibble
read_observations <- function(file_path) {

  read_csv(file_path) |>
    mutate(
      spacing_type = ifelse(is.na(spacing_type), 0, spacing_type),
      spacing_type = as_factor(spacing_type),
      date = lubridate::mdy(date))


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
        )
      ) %>%
      dplyr::select(site, date, time = timestamp, event) %>%
      # Filter out numeric events (the event counting software
      # used 0-9 for internal purposes)
      dplyr::filter(!event %in% as.character(0:9))
  }) |>
    # Combine into a single data frame
    dplyr::bind_rows()
  
  dfs
}

