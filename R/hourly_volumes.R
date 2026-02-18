# R/hourly_volumes.R
# List of helper functions used in the hourly_volumes pipeline

#' Calculate the miniumum required observations based on provided assumptions
#' 
#' @param o, dbl, standard deviation 
#' @param z, dbl, z-score 
#' @param U, dbl, centrality adjustment 
#' @param E, dbl, margin of error 
#' @return number of required observations
get_min_obs <- function(o = 3, z = 1.959964, 
                        U = 1.04, E = 1) {
  
  #equation to find required observations
  n = ((o^2) * (z^2) * ((U^2) + 2)) / (2 * (E^2))
  
  #round up to nearest whole number
  n = ceiling(n)
  
  return(n)  
}

#' Read a csv file that lists all the stations we want to examine
#'
#' @param file_path String path to the csv file listing the stations.
#' @return Character vector of 4-digit, zero-padded station numbers.
get_station_list <- function(file_path) {
  # Read only the needed column as character
  df <- read_csv(
    file = file_path,
    col_types = cols(
      station_number = col_character()
    )
  )

  if (!"station_number" %in% names(df)) {
    stop("Column 'station_number' not found in the CSV.\n",
    "Ensure the first line in station_list.csv is 'station_number'.")
  }

  station_list <- df$station_number |> 
    trimws() |>
    # remove NA/empty rows
    (\(x) x[!is.na(x) & nzchar(x)])() |>
    # keep only digits
    (\(x) gsub("\\D", "", x))() |>
    # zero-pad to width 4
    (\(x) str_pad(x, width = 4, side = "left", pad = "0"))()

  return(station_list)
}

#' Checks if hourly volume data has already been downloaded from UDOT
#' Downloads the files if not already downloaded.
#' 
#' @return Saves the hourly volume data from UDOT to disk
dnld_google_sheet <- function() {
  # make sure the settings do NOT require a google account.
  # Since we're accessing public info, we don't need one.
  drive_deauth()

  # define files paths.
  # The names for these excel files are only defined here, they can be adjusted.
  # However, the strings "301-431" and "501-733" must be included so that
  # get_station_data() can work properly.
  local_path_1 <- file.path("data", "hourly_volumes", "2023_volumes_stations_301-431.xlsx")
  local_path_2 <- file.path("data", "hourly_volumes", "2023_volumes_stations_501-733.xlsx")

  # check if the files already exist. If not, try to download them.
  # There are common issues downloading these files, so wrap it in a try-catch.
  # The error message includes instructions on how to handle download errors.
  tryCatch({
    # file one
    if (!file.exists(local_path_1)) {
      googledrive::drive_download(
        as_id("1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw"),
        path = local_path_1,
        overwrite = TRUE,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    }
    # file two
    if (!file.exists(local_path_2)) {
      googledrive::drive_download(
        as_id("1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI"),
        path = local_path_2,
        overwrite = TRUE,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    }
  },
  error = function(e) {
    message(
      "\n*** ERROR: Failed to download UDOT's hourly volume data.***\n",
      "Either skip hourly volume estimates, or manually download the files.\n",
      "The required files can be downloaded at:\n",
      "https://docs.google.com/spreadsheets/d/1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw/edit?gid=2031380538#gid=2031380538\n",
      "https://docs.google.com/spreadsheets/d/1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI/edit?gid=1035130660#gid=1035130660\n",
      "The spreadsheets are called '2023 Station 301-431' and\n",
      "'2023 Station 501-733' respectively.\n",
      "Save these files as:\n",
      "data/hourly_volumes/2023_volumes_stations_301-431.xlsx and\n",
      "data/hourly_volumes/2023_volumes_stations_501-733.xlsx.\n",
      "Original error: ", e$message, "\n"
      )
    stop(e) # re-throw to stop the pipeline
    }
  ) # end of the try_catch funciton
  } # of the dnld_google_sheet function


#' List the excel files in a given folder
#' 
#' @param folder_path file path to a folder
#' @return a list of file paths to excel files in the given folder
list_excel_files <- function(folder_path) {
  
  files <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

  files
}


#' reads and returns the names of all the sheets in the excel files
#' 
#' @param excel_files list of file paths to excel files
#' @return a list of stations available in the excel files ( ie sheet names)
get_available_stations <- function(excel_files) {
  # Get sheet names from local Excel files
  sheet_names1 <- readxl::excel_sheets(excel_files[1])
  sheet_names2 <- readxl::excel_sheets(excel_files[2])

  # Combine and deduplicate
  sheets <- unique(c(sheet_names1, sheet_names2))

  # Keep only sheets that look like station numbers:
  #   - entirely digits
  #   - length 3 or 4
  valid_station_pattern <- "^[0-9]{3,4}$"

  # Make a list of sheet names that look like station numbers
  available_stations <- sheets[grepl(valid_station_pattern, sheets)]

  # Deduplicate and sort
  available_stations <- sort(unique(available_stations))

  return(available_stations)
}

#' Remove unavailable station numbers from station_list
#' 
#' @param station_list a list of desired station numbers
#' @param approved_stations a list of available stations
#' @return a list of desired station filtered down to only what is available.
clean_stations <- function(station_list, available_stations){
 
  # Filter station_list down to only whats on the available list
  cleaned_station_list <- station_list[station_list %in% available_stations]
  
  

  return(cleaned_station_list)
}


#' Go through a column of station numbers and pull their respective data
#' 
#' @param cleaned_station_list list station numbers
#' @param excel_files list of excel files with station data
#' @return a list of tibbles, each named with their respective station number
get_all_station_data <- function(cleaned_station_list, excel_files){
  # Iterate through the column of station numbers
  # and collect all the data into one tibble.
  map(cleaned_station_list, ~ get_individual_station_data(.x, excel_files)) |>
    set_names(cleaned_station_list) |>
    bind_rows(.id = "station_number")
}

#' Find the provided station number in the excel sheets and return a tibble
#' with all that station's hourly volume counts
#' 
#' @param station integer representing the station number
#' @param excel_files list of file paths to excel files with data
#' @return complete data frame of that station
get_individual_station_data <- function(station, excel_files) {

  sheet_name <- station
  # Convert characters to integers to determine which excel file to read
  station <- suppressWarnings(as.integer(station))

    # Pick 'pattern' based on station
    pattern <- NULL
    if (station > 300 && station < 432) {
      pattern <- "301-431"
    } else if (station > 500 && station < 734) {
      pattern <- "501-733"
    } else {
      # stop if station isn't in the provided range. Shouldn't happen if
      # clean_station_list() worked properly.
      stop(paste0("station # ", station, " not found."))
    }

    # Find the excel file whose name contains pattern.
    local_path <- excel_files[grepl(pattern, excel_files)]
    # If the excel file doesn't exist, throw an error
    if (length(local_path) == 0) {
      stop(paste0("No Excel file found for stations '", pattern, "'."))
    }
  
    # Read the sheet info from the excel file
    # If the excel files weren't downloaded properly, we'll get an error.
    # Wrap the data extraction in a tryCatch to report the error more clearly.
    tryCatch(
      {
        # Read the sheet for the station from the local Excel file
        data <- readxl::read_excel(
          path = local_path,
          sheet = sheet_name,
          col_names = TRUE
        )

        # Remove the first and last columns
        data <- data[, -c(1, ncol(data))]

        # Ensure the appropriate column names
        colnames(data) <- c(
          'date', 'route', 'MP', 'lane', as.character(0:23)
        )
      },
      error = function(e) {
          message("\n*** ERROR: Failed to load station data from Excel files. ***\n",
        "Try running the program again, or manually check the Excel files in data/hourly_volumes.\n",
        "The files may not have downloaded properly with target 'download_sheets'\n.",
        "The required files can be downloaded at:\n",
        "https://docs.google.com/spreadsheets/d/1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw/edit?gid=2031380538#gid=2031380538\n",
        "https://docs.google.com/spreadsheets/d/1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI/edit?gid=1035130660#gid=1035130660\n",
        "The spreadsheets are called '2023 Station 301-431' and\n",
        "'2023 Station 501-733' respectively.\n",
        "Save these files as:\n",
        "data/hourly_volumes/2023_volumes_stations_301-431.xlsx and\n",
        "data/hourly_volumes/2023_volumes_stations_501-733.xlsx.\n",
                  "Original error: ", e$message, "\n")
          stop(e)
        }
      )

  return(data)  
}

#' Filter the station data down to the provided start and end date
#' 
#' @param all_station_data tibble with all the stations' data
#' @param start_date start date formatted as string "YYYY-MM-DD"
#' @param end_date   end date formatted as string "YYYY-MM-DD". 
#' Defaults to start date.
#' @return tibble with average volumes per hour within given dates
get_hourly_volume <- function(
  all_station_data, start_date, end_date = start_date) {
  
  # Ensure all dates are in the appropriate format
  all_station_data$date <- as.Date(all_station_data$date)

  # Find the average hourly volumes for each site:
  avg_hourly_volumes <- all_station_data %>%

    # step 1: Filter down to only requested dates
    filter(date >= start_date & date <= end_date) %>%
    
    # step 2: summarize the lanes to get total hourly volumes each day
    # For each combination of station_number and date...
    group_by(station_number, date) %>%
    # Summarize the different lanes into a single row.
    summarize(
      # keep route and MP
      route = first(route),
      MP = first(MP),
      # This is the part that actually sums the different lanes
      # Preserves the hourly breakdown
      across(
        .cols = any_of(as.character(0:23)),
        .fns = ~ sum(.x, na.rm = TRUE) 
      ),
      .groups = "drop"
    ) %>%
    
    # step 3: average across all the days for each site
    # for each station_number...
    group_by(station_number) %>%
    summarize(
      # ...keep route and MP, and ...
      route = first(route),
      MP = first(MP),
      # ...take the mean of all the days
      across(
        .cols = any_of(as.character(0:23)),
        .fns = ~ mean(.x, na.rm = TRUE) 
      ),
      .groups = "drop"
    )

}

#' Plot hourly volumes of each site in a facted plot
#' @param hourly_volumes tibble/data.frame with columns:
#'   - station_number
#'  - "0","1",...,"23" (hourly volumes)
#' @param start_time, integer in [0, 23]; inclusive time window
#' @param end_time integer in [0, 23]; inclusive time window
#' @param n numeric scalar; horizontal reference level to display on each facet
plot_hourly_volumes <- function(hourly_volumes, start_time, end_time, n = 30) {

  # Validate inputs
  if (any(!c(start_time, end_time) %in% 0:23)) {
    stop("`start_time` and `end_time` must be integers in 0:23.")
  }
  if (length(n) != 1 || !is.numeric(n) || is.na(n)) {
    stop("`n` must be a single numeric value.")
  }

  # Ensure hour columns exist (as strings "0"..."23")
  expected_hour_cols <- as.character(0:23)
  present_hour_cols <- intersect(expected_hour_cols, names(hourly_volumes))
  if (length(present_hour_cols) == 0) {
    stop("No hour columns found. Expected some of: '0','1',...,'23'.")
  }

  # Pivot data to be longer: each row is one hour of one station number
  long <- hourly_volumes %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(present_hour_cols),
      names_to = "hour",
      values_to = "volume",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(hour = as.integer(hour))

  # Helper function to identify which hours are inside the window (inclusive)
  # Supports wrapping windows (e.g., 22 -> 5)
  in_window_vec <- function(hour, start_time, end_time) {
    if (start_time <= end_time) {
      hour >= start_time & hour <= end_time
    } else {
      hour >= start_time | hour <= end_time
    }
  }

  # Mark hours within the window and set bar colors
  long <- long %>%
    dplyr::mutate(
      in_window = in_window_vec(hour, start_time, end_time),
      fill = ifelse(in_window, "steelblue", "grey")
    )

  # Create plot
  p <- ggplot(
    long,
    aes(x = factor(hour, levels = 0:23), y = volume, fill = fill)
    ) +
    geom_col() +
    # Mark the minimum required observation with a horizontal line
    geom_hline(
      yintercept = n, 
      linetype = "dashed", 
      color = "firebrick", 
      linewidth = 0.7
    ) +
    scale_fill_identity() +
    # only label every few hours. Adjust frequency with the 'by = ' argument
    scale_x_discrete(
      breaks = as.character(seq(0, 23, by = 4))
    ) +
    labs(
      x = "Hour of Day (0-23)",
      y = "Average Traffic Volume",
      caption = paste0("Reference line at n = ", n)
    ) +
    facet_wrap(
      ~ station_number,
      scales = "free_y"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      text = element_text(family = "Times New Roman", size = 14),
      panel.border = element_blank(),
      plot.caption = element_text(hjust = 0.5) # center the caption labeling n
    )

  return(p)
}