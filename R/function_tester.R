# Load the functions from functions_list.R
source("R/functions_list.R")


library(tidyverse)
library(googlesheets4)
gs4_deauth() #prevents the need of signing into Google

options(timeout = 1000)
tib <- station_data(720)

hv <- hourly_volume(tib, "2023-05-01", "2023-08-31")

