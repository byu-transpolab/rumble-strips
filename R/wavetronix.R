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
      strip_spacing = ifelse(is.na(strip_spacing), 0, strip_spacing),
      strip_spacing = as_factor(strip_spacing),
      date = lubridate::as_date(date, format = "%m/%d/%y"))


}


### Helper functions for reading camera top files and wavetronix files

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

make_displacement_plot_data <- function(wavetronix, camera_top_data, observation_data) {

  wcum <- wavetronix |>
    # only keep w1, because that's the unit where the 
    # rumble strips are installed
    dplyr::filter(unit == "w1") |>
    # only keep lane 01 data because that's the lane where
    # the rumble strips are installed
    dplyr::filter(lane == "01") |>
    dplyr::group_by(site, date) |>
    dplyr::arrange(sensor_time) |>
    dplyr::mutate(
      cumulative = cumsum(volume)
      # TODO: make a time column that is just the time of
      # day for the observation without the date
    ) |>
    dplyr::select(site, sensor_time, cumulative)

  # test code to make sure data is working
  ggplot() + 
    geom_line(data = wcum, aes(x = sensor_time, y = cumulative, group = date)) + 
    geom_vline(data = camera_top_data, aes(xintercept = time, color = event)) +
    facet_wrap(~date, scales = "free_x")
    





}

plot_cumulative_with_camera <- function(wdf_sub, camera_meta, out_path) {
  # wdf_sub: tibble with columns time (POSIXct) and cumulative
  # camera_meta: tibble/list row with date_code and events (events is a tibble with time_str and event)
  tz <- if (!is.null(attr(wdf_sub$time, "tzone")) && nzchar(attr(wdf_sub$time, "tzone"))) {
    attr(wdf_sub$time, "tzone")
  } else {
    Sys.timezone()
  }

  # Extract camera event times (strings like "HH:MM:SS.SSS")
  cam_times_posix <- NULL
  if (!is.null(camera_meta) && !is.null(camera_meta$events) && length(camera_meta$events) >= 1) {
    cam_df <- camera_meta$events[[1]]
    if (nrow(cam_df) > 0 && "time_str" %in% names(cam_df)) {
      # compose full datetime using date_code (YYYYMMDD) + time_str and parse fractional seconds
      date_code <- camera_meta$date_code
      cam_times_posix <- as.POSIXct(paste(date_code, cam_df$time_str),
                                    format = "%Y%m%d %H:%M:%OS",
                                    tz = tz)
      cam_times_posix <- cam_times_posix[!is.na(cam_times_posix)]
    }
  }

  p <- ggplot(wdf_sub, aes(x = time, y = cumulative)) +
    geom_line(color = "steelblue") +
    theme_minimal() + labs(x = "Time", y = "Cumulative volume")


  if (!is.null(cam_times_posix) && length(cam_times_posix) > 0) {
    # normalize event values and match times
    ev <- tolower(trimws(cam_df$event))
    times_o <- cam_times_posix[ev == "o"]
    times_i <- cam_times_posix[ev == "i"]
    times_u <- cam_times_posix[ev == "u"]

    if (length(times_o) > 0) {
      p <- p + geom_vline(xintercept = as.numeric(times_o), color = "green", linetype = "dashed")
    }
    if (length(times_i) > 0) {
      p <- p + geom_vline(xintercept = as.numeric(times_i), color = "orange", linetype = "dashed")
    }
    if (length(times_u) > 0) {
      p <- p + geom_vline(xintercept = as.numeric(times_u), color = "red", linetype = "dashed")
    }
  }

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_path, plot = p, width = 8, height = 4.5)
  out_path
}

