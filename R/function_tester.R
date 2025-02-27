library(tidyverse)
library(googlesheets4)
gs4_deauth() #prevents the need of signing into Google

# Load the functions from functions_list.R
source("R/functions_list.R")

tib <- station_data(720)

hv <- hourly_volume(tib, "2023-05-01", "2023-08-31")

AADT_perc(hv, 8, 17)

n <- min_obs()

n <- min_obs(o = 5)

n <- min_obs(E = 0.5)

obs_time(8, 17, n, hv)

plot_station(tib, 
             "2023-05-01", "2023-08-31",
             8, 17)

