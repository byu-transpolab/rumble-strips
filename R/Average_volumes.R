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

# Load the functions from functions_list.R
source("R/functions_list.R")

##Define files and variables###############################

#import csv with station #s
station_list <- read_csv(data/stations_in_region4)

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

#count the quantity of stations

#loop through all stations

    #read in data from Google sheets
    df <- station_data(station)
  
    #add stations hourly data to totals hourly data
  
    #add station AADT to the tibble
  
    #add station AADT% to the tibble
  
    #plot station hourly volumes

#plot total station data

#save AADT and AADT% to a csv


