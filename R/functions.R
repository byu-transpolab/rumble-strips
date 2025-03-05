##Description#########################################
# defines several functions used to analyze traffic counting
# data from UDOT. only analyzes 2023 data. 

##required pacakges######################################

#tidyverse
#googlesheets4

##get_available_stations##########################
#' returns a char list of stations available in Sheets

get_availabe_stations <- function() {
  
  options(timeout = 1000)
  
  #retrieves the available worksheets in 2023 data 301-431
  sheet_names1 <- sheet_names(
    "1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw")
  
  #retrieves the available worksheets in 2023 data 501-733
  sheet_names2 <- sheet_names(
    "1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI")
  
  return(c(sheet_names1, sheet_names2))
  
}

##clean_stations#####################################
#' @param sl a tibble with the a column of station #s
#' returns a column with avaiable stations. 

clean_stations <- function(sl){
  
  approved_stations <- get_available_stations()
  
  sl <- sl %>%
    filter(station_number %in% approved_stations)
  
  # Convert the character column to integer
  sl$station_number <- as.integer(sl$station_number)
  
  return(sl)
}

##get_station_data#########################################
#' @param station integer, 3-digit station  number 
#' returns complete data frame of that station
#

#station = 733 #used as a debug tool

get_station_data <- function(station) {
  
#determines which Google sheet to look at based on station #
if (300 < station & station < 432) {
  sheet_id = "1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw"
} else if (500 < station & station < 734) {
  sheet_id = "1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI"
} else {
  stop(paste0("station # ", station, " not found."))
}
  

#bring in volume data from Google sheets
data <- read_sheet(
  sheet_id,
  sheet = paste0("0", station),
  range = paste0("B:AC"),
  col_names = TRUE,                    
  col_types = NULL,
  trim_ws = TRUE
)

#ensure the appropriate column names
colnames(data) <- c(
                      'DATE', 'route', 'MP', 'lane',
                      '0','1','2','3','4',
                      '5','6','7','8',
                      '9','10','11','12',
                      '13','14','15','16',
                      '17','18','19','20',
                      '21','22','23'
                    )

return(data)  
}


##get_hourly_volume########################################
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
 
#isolate just the volume data (i.e. columns F to AC)
  df <- df %>%
    select("0":"23") %>%
    mutate_all(as.integer)
  
#add all columns together into one vector
hourly_volume <- colSums(df, na.rm = TRUE)  
  
  
#average the total using the total # of days
y = rep(days, 24)
hourly_volume <- hourly_volume / y 

#rounds to whole #
hourly_volume <- round(hourly_volume, 
                       digits = 0) 
                      
return(hourly_volume)  
  
}

##get_min_obs##############################################
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

##get_aadt_perc#############################################
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

##get_obs_time########################################
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

##plot_station######################################
#' @param hv vector with hourly volume data
#' @param station int, station #
#' @param sd start date formatted as string "YYYY-MM-DD"
#' @param ed end date formatted as string "YYYY-MM-DD"
#' @param obs int, # of minimum observations
#' return plot with AADT%, time window, min observation time

plot_station <- function(hv, station, 
                         sd, ed,
                         st, et,
                         obs = 54){
  
  #get the day time percentage
  p <- get_aadt_perc(hv, st, et)
  
  #get minimum hours of observation needed
  hours <- get_obs_time(st, et, obs, hv)
  
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
  
  #convert 24-hour format to am pm
  if (st < 12) {
    start_time <- paste0(st, "am")
  } else if (st == 12) {
    start_time <- paste0(st, "pm")
  } else {
    start_time <- paste0(st - 12, "pm")
  }
    
  if (et < 12) {
    end_time <- paste0(et, "am")
  } else if (et == 12) {
    end_time <- paste0(et, "pm")
  } else {
    end_time <- paste0(et - 12, "pm")
  }
  
  
  # Create the plot
  ggplot(data, aes(x = hour, y = volume, fill = color)) +
    geom_col() + # bar chart
    scale_fill_identity() +         # color based on color col
    labs(
      title = paste0('Station ', station, ', ',
                      "% AADT from ", start_time, " to ", 
                      end_time, ": ", p, "%"
                      ),        # Title of the plot
      subtitle = paste0("required hours of observation: ",
                  hours),
      x = "Hours of the Day",   # X-axis label
      y = "Average Traffic Volume"    # Y-axis label
    ) +
    theme_minimal()                   # A clean theme
  
}

##hist_summary#########################################
#' @param table expecting table with station, AADT, 
#'              and day time percentage

hist_summary <- function(table) {
  
  hist(table)
  
}
