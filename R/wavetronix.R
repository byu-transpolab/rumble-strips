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

# read simple camera_top file that you showed (time,event)
read_camera_top <- function(path) {
  df <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
  names(df) <- c("time_str", "event")
  date_code <- stringr::str_extract(basename(path), "^\\d{8}")
  df <- df %>%
    mutate(time_str = sub(":(\\d{1,3})$", ".\\1", time_str),
           datetime = as.POSIXct(paste0(date_code, " ", time_str),
                                 format = "%Y%m%d %H:%M:%OS", tz = Sys.timezone()))
  tibble(file = path, date_code = date_code, events = list(df))
}

plot_cumulative_with_camera <- function(wdf_sub, camera_meta, out_path) {
  p <- ggplot(wdf_sub, aes(x = time, y = cumulative)) +
    geom_line(color = "steelblue") +
    theme_minimal() + labs(x = "Time", y = "Cumulative volume")
  cam_times <- camera_meta$events[[1]]$datetime
  if (length(cam_times) && !all(is.na(cam_times))) {
    p <- p + geom_vline(xintercept = as.numeric(cam_times), color = "red", linetype = "dashed")
  }
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(out_path, plot = p, width = 8, height = 4.5)
  out_path
}

