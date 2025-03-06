

##Description#############################################
#' A script that takes in a csv file with a list of station 
#' numbers and averages the volume data together. 
#' Will produce individual plots for all stations as well as 
#' final plot showing the overall average.
#' Creates a tibble with a summary of each stations data 
#' to further compare each station.

##packages#################################################
library(tidyverse)
library(googlesheets4)
gs4_deauth() #prevents the need of signing into Google

#Load the functions from functions.R
source("R/functions.R")

##Define files and variables###############################

#import csv with station #s
station_list <- read_csv("~/rumble-strips/data/stations_in_region4")

#statistical info
o = 3         #standard deviation
z = 1.959964  #z-score
U = 1.04      #centrality adjustment
E = 1         #margin of error

#'start and end dates formatted as string "yyyy-mm-dd"
sd = "2023-05-01" 
ed = "2023-08-31"

#'start and end times, integer 0-23 for 24-hr format
st = 8
et = 17

##Process all the stations################################

#clean the list of stations
station_list <- clean_stations(station_list)

#calculate minimum observations on given variables
n = get_min_obs(o, z, U, E)

#initialize a total hourly volume vector
thv <- rep(0, 24)

#add columns to station_list to record AADT and AADT%
station_list <- station_list %>%
  mutate(AADT = 0, AADT_percentage = 0, hours = 0)

#initialize a counter to track the iteration of the loop
i = 1


#loop through all stations
for (station in station_list$station_number) 
  {
    #read in data from Google sheets
    df <- get_station_data(station)
  
    #add station's hourly_volume data to totals data
    hv = get_hourly_volume(df, sd, ed)
    thv = thv + hv
  
    #add station AADT to station_list
    station_list[i, "AADT"] <- 
        sum(hv)
    
    #add station AADT% to station_list
    station_list[i, "AADT_percentage"] <- 
        get_aadt_perc(hv, st, et)
    
    #add station observation time to station_list
    station_list[i, "hours"] <-
        get_obs_time(st, et, n, hv)
  
    #plot station hourly volumes
    plot_station(hv, sd, ed, st, et, n)
    
    i = i +1
  }



#average out total hourly volume
y = rep(nrow(station_list), 24)
thv = thv / y
thv <- round(thv, digits = 0)


#plot total station data
station = "Rg 4"
plot_station(thv, sd, ed, st, et, n)

#save AADT and AADT% to a csv
write_csv(station_list, 
          "~/rumble-strips/data/station_summary")




