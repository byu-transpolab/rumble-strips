#1.
#Load the libraries by highlighting the code below and running it (Cmd + Enter)
library(targets)
library(tidyverse)
library(rstatix)
library(googledrive)
library(readxl)
library(mlogit)
library(modelsummary)
library(svglite)

#NOTE:
#Your data should already be loaded.
#However, if you're having trouble,
#try running "tar_make()" in the console to re-run the pipeline
#to generate the data files again.

#2.
#Choose a variable you want to view from the tibble
#Examples:
    #camera_back_data
    #camera_top_data
    #wavetronix
#assign a variable name and enter the following code in the console:
#variable_name <- tar_read(variable)
#Example:
    #cb <- tar_read(camera_back_data)

#NOTE:
#The variable should match what already exists in the _targets.R file

#3.
#View the tibble by entering the following code in the console:
#utils::View(variable_name)
#Example:
    #utils::View(cb)

# The tibble should now be visible in a new tab! :)

#Use this to look at the camera_back_data tibble
cb <- tar_read(camera_back_data)
utils::View(cb)

#Use this to look at the camera_top_data tibble
ct <- tar_read(camera_top_data)
utils::View(ct)

#Use this to look at the wavetronix tibble
wv <- tar_read(wavetronix)
utils::View(wv)

#Use this to look at the worker_exposure_data tibble
w_e_d <- tar_read(worker_exposure_data)
utils::View(w_e_d)