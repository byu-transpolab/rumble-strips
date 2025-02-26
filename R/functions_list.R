##Description#########################################
# defines several functions used to analyze traffic counting
# data from UDOT. only analyzes 2023 data. 



##import data#############################################
#' @param station
#' returns complete data frame of that station
#
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
  range = paste0("0", station, "!B:AC"), #will B:AC select 
  col_names = TRUE,                      #enough rows?
  col_types = NULL,
  trim_ws = FALSE,  #this will parameter should be checked
)

return(data)  
}


##Avgerage volume per hour for given date(s)##############
#' @param data        data frame
#' @param start_date  date in YYYY-MM-DD format
#' @param end_date    date in YYYY-MM-DD format
# return vector with average volumes per hour within
# given dates
# 

hourly_volume <- function(data, start_date, 
                          end_date = start_date) {
  
#count unique days in the data  
y = 
  
    
#isolate just the volume data (i.e. columns F to AC)
vdata <-   
  
#add all columns together into one row
hourly_volume <- colSums(vdata, na.rm = TRUE)  
  
  
#average the total using the total # of days
y = rep(n, 24) 
hourly_volume <- hourly_volume / y
hourly_volume <- round(hourly_volume, digits = 0)  

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

##AADT####################################################
#' @param hourly_volume in vector form
# return AADT = total volume over 24 hours
# 

AADT <- functions(hourly_volume) {
  
AADT = sum(hourly_volume)  
  
return(AADT)  
  
}
##AADT % within time window################################
#' @param hourly_volume in vector from
#' @param start_time
#' @param end_time defaults to start time
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
#' @param hourly_volume
#' @param min_observations
# call on AADT and AADT% functions
# return observation time
# 
obs_time <- function(){
  
  
  
  
  
  
}

##plot station data######################################
#' @param hourly_volume in vector format
#' @param station
#' @param start_date
#' @param end_date
#' @param start_time
#' @param end_time
#' @param file_path
# return plot with AADT%, time window, min observation time
# save it to the given folder
# 
