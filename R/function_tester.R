# Load the functions from functions_list.R
source("R/functions_list.R")


library(tidyverse)
library(googlesheets4)
gs4_deauth() #prevents the need of signing into Google

station_data(720)
