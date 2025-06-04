##Description#########################################
# defines several functions used to analyze traffic counting
# data from UDOT. only analyzes 2023 data. 

##required pacakges######################################

#tidyverse
#readxl
#googledrive

##Functions##########################################

dnld_google_sheet <- function() {
  drive_deauth()

  local_path_1 <- file.path("data", "temp_data", "2023_data_301-431.xlsx")
  local_path_2 <- file.path("data", "temp_data", "2023_data_501-733.xlsx")

  # Download the files if they don't exist
  if (!file.exists(local_path_1)) {
    googledrive::drive_download(
      as_id("1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw"),
      path = local_path_1,
      overwrite = TRUE,
      type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
  }

  if (!file.exists(local_path_2)) {
    googledrive::drive_download(
      as_id("1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI"),
      path = local_path_2,
      overwrite = TRUE,
      type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
  }
}


#' returns a char list of stations available in Sheets
get_available_stations <- function() {
  # Get sheet names from local Excel files
  sheet_names1 <- readxl::excel_sheets("data/temp_data/2023_data_301-431.xlsx")
  sheet_names2 <- readxl::excel_sheets("data/temp_data/2023_data_501-733.xlsx")

  available_stations <- c(sheet_names1, sheet_names2)

  write(available_stations, file = "data/available_stations")
}


##clean_stations#####################################

#' @param station_list a tibble with the a column of station #s
#' returns a column with available stations.

clean_stations <- function(station_list){
if (file.exists("data/available_stations")) {
  approved_stations <- read.csv("data/available_stations")
} else {
  
  get_available_stations()
  approved_stations <- read.csv("data/available_stations")
  
}
 
  cleaned_station_list <- station_list %>%
    filter(station_number %in% 
             approved_stations$available_stations)
  
  # Convert the character column to integer
  cleaned_station_list$station_number <- 
    as.integer(cleaned_station_list$station_number)
  
  return(cleaned_station_list)
}

#' @param station integer, 3-digit station  number 
#' returns complete data frame of that station
#

#station = 733 #used as a debug tool

get_station_data <- function(station) {
  # Determine which local Excel file to use based on station number
  if (300 < station & station < 432) {
    local_path <- "data/temp_data/2023_data_301-431.xlsx"
  } else if (500 < station & station < 734) {
    local_path <- "data/temp_data/2023_data_501-733.xlsx"
  } else {
    stop(paste0("station # ", station, " not found."))
  }

  # Read the sheet for the station from the local Excel file
  sheet_name <- paste0("0", station)
  data <- readxl::read_excel(
    path = local_path,
    sheet = sheet_name,
    col_names = TRUE
  )

  # Remove the first and last columns
  data <- data[, -c(1, ncol(data))]

  # Ensure the appropriate column names
  colnames(data) <- c(
    'DATE', 'route', 'MP', 'lane',
    as.character(0:23)
  )

  return(data)  
}


#' @param df data frame of station data
#' @param sd start date formatted as string "YYYY-MM-DD"
#' @param ed   end date formatted as string "YYYY-MM-DD"
# return vector with average volumes per hour within
# given dates
# 

get_hourly_volume <- function(df, sd, 
                          ed = sd) {
  
# Ensure the date column is in Date format
df$DATE <- as.Date(df$DATE)

#determine which rows have the needed dates
df <- df %>%
  filter(DATE >= sd & DATE <= ed)

# Count the number of unique days
days <- length(unique(df$DATE))
 

#identify the first row with sd and last row with ed
slected_rows <- df$DATE >= sd & df$DATE <= ed
    
#isolate just the volume data (i.e. columns F to AC)
vdata <- df %>%
  select("0":"23") %>%
  mutate_all(as.integer)
  
#add all columns together into one vector
hourly_volume <- colSums(vdata[slected_rows, ], na.rm = TRUE)  

#isolate just the volume data (i.e. columns F to AC)
  df <- df %>%
    select("0":"23") %>%
    mutate_all(as.integer)
  
#average the total using the total # of days
y = rep(days, 24)
hourly_volume <- hourly_volume / y 

#rounds to whole #
hourly_volume <- round(hourly_volume, 
                       digits = 0) 
                      
return(hourly_volume)  
}

#' @param o, dbl, standard deviation 
#' @param z, dbl, z-score 
#' @param U, dbl, centrality adjustment 
#' @param E, dbl, margin of error 
# return number of required observations

get_min_obs <- function(o = 3, z = 1.959964, 
                        U = 1.04, E = 1) {
  
  #equation to find required observations
  n = ((o^2) * (z^2) * ((U^2) + 2)) / (2 * (E^2))
  
  #round up to nearest whole number
  n = ceiling(n)
  
  return(n)  
}

#' @param hv vector with hourly volume data
#' @param st start time, integer 0-23 for 24-hour format
#' @param et start time, integer 0-23 for 24-hour format
# return % AADT within time window
# 

get_aadt_perc <- function(hv, st, 
                      et = st){
  
  #find the total volume within the given time
  work_zone_volume = sum(hv[st:et])
  
  total_volume = sum(hv)
  
  #find the percentage of the AADT during specified hours
  percentage = round(
                    work_zone_volume / total_volume * 100,
              digits = 2
  )  
  
return(percentage)  
}

#' @param st  start time, integer 0-23 for 24-hour format
#' @param et  start time, integer 0-23 for 24-hour format
#' @param obs int, minimum # needed for confidence
#' @param hv  vector with hourly volume data
# calls on get_aadt_perc function
# return minimum required observation time
# 
get_obs_time <- function(st, et,
                     obs, hv){
  
  t = et - st
  if(t==0){t = 1}
  
  a = sum(hv)
  p = get_aadt_perc(hv,st,et)
  
  hours = round(
                (t * obs) / (a * p/100)
          , 5)
  
  return(hours)
  
}

#' @param hv vector, hourly volume data
#' @param st int, start time in 24-hour format
#' @param et int, end time in 24-hour format
#' return plot with AADT%, time window, min observation time

plot_station <- function(hv, st, et){
  
  #set column colors based on provided time window
  co <- rep("grey", 24)
  if (st > et) {
      day_indices <- c((st + 1):24, 1:(et + 1))
    } else {
      day_indices <- (st + 1):(et + 1)
  }
  co[day_indices] <- "steelblue"
  
  #convert a vector to a table
  data <- data.frame(hour = 0:23, volume = hv, color = co)
  
   # Create the plot
  ggplot(data, aes(x = hour, y = volume, fill = color)) +
    geom_col() + # bar chart
    scale_fill_identity() +         # color based on color col
    labs(#             hours),
      x = "Hours of the Day",   # X-axis label
      y = "Average Traffic Volume"    # Y-axis label
    ) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = "Times New Roman", 
                              size = 14),
          panel.border = element_blank()
    )                  
  
}

#' @param df expecting table with station, AADT, 
#'           and day time percentage

plot_station_summary <- function(df) {
  
  ggplot(df, aes(y = daytime_perc, x = AADT)) +
    geom_jitter(width = 0.2, 
                alpha = 0.7, 
                color = "steelblue") +
    labs(y = "Percentage of AADT between 8AM and 5PM", 
         x = "AADT") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman", 
                              size = 14),
          panel.border = element_blank()
          )
}
