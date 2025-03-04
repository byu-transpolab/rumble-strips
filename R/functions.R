##Description#########################################
# defines several functions used to analyze traffic counting
# data from UDOT. only analyzes 2023 data. 

##required pacakges######################################

#tidyverse
#googlesheets4

##Get_approved_stations##############################
#' returns a list of available station data

get_approved_stations <- function() {
  
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
  
#count unique days in the data  

  # Ensure the date column is in Date format
  df$DATE <- as.Date(df$DATE)
  
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
#' @param sd start date formatted as string "YYYY-MM-DD"
#' @param ed end date formatted as string "YYYY-MM-DD"
#' @param obs int, # of minimum observations
#' return plot with AADT%, time window, min observation time

plot_station <- function(hv, 
                         sd, ed,
                         st, et,
                         obs = 54){
  
  p <- get_aadt_perc(hv, st, et)
  hours <- get_obs_time(st, et, obs, hv)
  
  if (st < 12) {
    st <- paste0(st, "am")
  } else if (st == 12) {
    st <- paste0(st, "pm")
  } else {
    st <- paste0(st - 12, "pm")
  }
    
  if (et < 12) {
    et <- paste0(et, "am")
  } else if (et == 12) {
    et <- paste0(et, "pm")
  } else {
    et <- paste0(et - 12, "pm")
  }
  
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


