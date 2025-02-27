##Description#########################################
# defines several functions used to analyze traffic counting
# data from UDOT. only analyzes 2023 data. 

##required pacakges######################################

#tidyverse
#googlesheets4

##Clean station list#####################################
#' @param sl a tibble with the a column of station #s

clean_stations <- function(sl){
  
  options(timeout = 1000)
  #retrieves the available worksheets in 2023 data 301-431
  sheet_names1 <- sheet_names(
    "1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw")
  
  #retrieves the available worksheets in 2023 data 501-733
  sheet_names2 <- sheet_names(
    "1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI")
  
  approved_stations <- c(sheet_names1, sheet_names2)
  
  sl <- sl %>%
    filter(station_number %in% approved_stations)
  
  # Convert the character column to integer
  sl$station_number <- as.integer(sl$station_number)
  
  return(sl)
}

##import data#############################################
#' @param station
#' returns complete data frame of that station
#

#station = 733 #used as a debug tool

station_data <- function(station) {
  
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
  trim_ws = TRUE,
  timeout = 60
)

return(data)  
}


##Avgerage volume per hour for given date(s)##############
#' @param df        data frame
#' @param start_date  date in YYYY-MM-DD format
#' @param end_date    date in YYYY-MM-DD format
# return vector with average volumes per hour within
# given dates
# 

hourly_volume <- function(df, start_date, 
                          end_date = start_date) {
  
#count unique days in the data  

  # Ensure the date column is in Date format
  df$DATE <- as.Date(df$DATE)
  
  # Count the number of unique days
  days <- length(unique(df$DATE))
 
    
#isolate just the volume data (i.e. columns F to AC)
  vdata <- df %>%
    select(H0000:H2300) %>%
    mutate_all(as.integer)
  
#add all columns together into one row
hourly_volume <- colSums(vdata, na.rm = TRUE)  
  
  
#average the total using the total # of days
y = rep(days, 24) #creates a vector 
hourly_volume <- hourly_volume / y #vector divided by vector
hourly_volume <- round(hourly_volume, 
                       digits = 0) #rounds up to whole #

return(hourly_volume)  
  
}

##Required observations###################################
#' @param standard_deviation o, dbl
#' @param z-score z, dbl
#' @param centrality_adjustment U, dbl
#' @param margin_of_error E, dbl
# return number of required observations
# 
min_obs <- function(o = 3, z = 1.959964, U = 1.04, E = 1) 
{
  
  n = ((o^2) * (z^2) * ((U^2) + 2)) / (2 * (E^2))
  n = ceiling(n)
  
  return(n)  
}

##AADT % within time window################################
#' @param hourly_volume in vector from
#' @param start_time integer 0-23 for 24 hour format
#' @param end_time integer 0-23 for 24 hour format
# return % AADT within time window
# 

AADT_perc <- function(hourly_volume, start_time, 
                      end_time = start_time){
  
  #find the total volume within the given time
  work_zone_volume = sum(hourly_volume[start_time:end_time])
  
  total_volume = sum(hourly_volume)
  
  #find the percentage of the AADT during specified hours
  percentage = round(
                    work_zone_volume / total_volume * 100,
              digits = 2
  )  
  
return(percentage)  
}

##Observation time########################################
#' @param AADT_percentage dbl 63.3% = 63.3 =/= 0.633
#' @param observations int, minimum # needed for confidence
#' @param work_time int, time period related to AADT_percent
# call on AADT and AADT% functions
# return observation time
# 
obs_time <- function(start_time, end_time,
                     obs, hourly_volume){
  
  t = end_time - start_time
  if(t==0){t = 1}
  
  a = sum(hourly_volume)
  p = AADT_perc(hourly_volume,start_time,end_time)
  
  hours = round(
                (t * obs) / (a * p/100)
          , 5)
  
  return(hours)
  
}

##plot station data######################################
#' @param data tibble with dates and volume data
#' @param start_date sd
#' @param end_date ed
#' @param start_time st
#' @param end_time et
#' @param min_observations int, # of minimum observations
# return plot with AADT%, time window, min observation time
# save it to the given folder
# 

plot_station <- function(data, 
                         sd, ed,
                         st, et,
                         obs = 54){
  
  hv <- hourly_volume(data, sd, ed)
  p <- AADT_perc(hv, st, et)
  hours <- obs_time(st, et, obs, hv)
  
    
  barplot(hv,
          beside = TRUE,
          ylim = range(pretty(c(0, hv))))
  title(main = paste0('Station ', station, ', ',
                      "% AADT from ", st, " to ", et, ": ", 
                      p, "%"
                      ),
        sub = paste("required hours of observation: ",
                     hours
                     ),
        xlab = "Hours of the Day",
        ylab = "Average Traffic Volume"
        )
    

}


