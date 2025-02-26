##Description##############################################

# This script is intended to take hourly volume data from
# UDOT and calculate what percentage of the total AADT is 
# during those specific hours and the minimum required time
# to make statistically significant observations.

#Make sure to define all variables!

#data can be found at https://drive.google.com/drive/folders/1ZYy-WkICLOp1482vwEbTc5UvLItbWs4y

##Packages################################################
library(tidyverse) 
library(googlesheets4)
  gs4_deauth() #prevents the need of signing into Google

##Define variables ########################################

  
#' @param station
#' @param date
AADT <- function(station, date)  
  
#define the station number
station = 733

start_date = '2023-05-01'
end_date = '2023-08-31'


  

#number of days within the data
#123 days From May to August
n = 123

#initial and final hour of work zone in 24-hour format
s = 8
f = 17

#Margin of Error
E = 1

#Centrality Adjustment
  #0 for median, 1.04 for 85th percentile
U = 1.04

#z-score
  #1.959964 for 95% confidence level
z = 1.959964

#Standard Deviation
o = 3

##ID what data is available################################

#determines which Google sheet to look at based on station #
if (300 < station & station < 432) {
  sheet_id = "1NroJmNFLNE_GiaSNb0lqkqSnywu_BVfIA232WEaK6xw"
} else if (500 < station & station < 734) {
  sheet_id = "1YGtU_NlKSPI5jOl8kSIeQiqb5lh5xr6431xXVYk2fSI"
} else {
  stop(paste0("station # ", station, " not found."))
}

#pulls all the dates from the data, there are repeats
dates <- read_sheet(
                    sheet_id,
                    range = paste0("0", station, "!B:B"),
                    col_names = TRUE,
                    col_types = "D",
                    trim_ws = FALSE,
                    )

#finds the first row with start_date
initial_row = dates.query('Date == "2023-05-01"').head(1)


#finds the last row with end_date 
final_row = dates.loc[dates['DATE'] == 
                        end_date].tail(1)



##Import data##############################################



#defines range of cells with volume data
rng = paste0("0", station, "!F", initial_row, ":AC", final_row)

#bring in volume data from Google sheets
data <- read_sheet(
                    sheet_id,
                    range = rng,
                    col_names = FALSE,
                    col_types = NULL,
                    trim_ws = FALSE,
                     )


#ensure we have the correct column names
colnames(data) <- c(
                    #'date', 'route', 'MP', 'lane',
                    '0','1','2','3','4',
                    '5','6','7','8',
                    '9','10','11','12',
                    '13','14','15','16',
                    '17','18','19','20',
                    '21','22','23'
                    )






##Volume during the day as % of AADT######################

#total the volume each hour for all lanes and dates
hourly_total <- colSums(data, na.rm = TRUE)

#average the totals per day
y = rep(n, 24) 
hourly_total <- hourly_total / y
hourly_total <- round(hourly_total, digits = 0)

#find the AADT for the given time frame we're looking at
AADT = sum(hourly_total)

#find the total volume within the specified hours
work_zone_volume = sum(hourly_total[s:f])

#find the percentage of the AADT during specified hours
work_zone_percentage = round(
                      work_zone_volume / AADT * 100,
                      digits = 2
                      )

##Minimum Observation Time#######################

#calculates the minimum required observations
observations = ((o^2) * (z^2) * ((U^2) + 2)) / (2 * (E^2))
observations = ceiling(observations)

#calculates the minimum required observation time
t = f - s
minimum_hours = round(
  (t * observations) / (AADT * work_zone_percentage/100)
                , 5)

##Print Results###############################

sub_title = bquote(.(minimum_hours) ,
                   " hours of required observation time")


barplot(hourly_total,
        beside = TRUE,
        ylim = range(pretty(c(0, hourly_total))))
title(main = paste0('Station ', station, ', ',
                   "% AADT from ", s, " to ", f, ": ", 
                   work_zone_percentage, "%"
                  ),
      sub = paste("required hours of observation: ",
                  minimum_hours
                  ),
      xlab = "Hours of the Day",
      ylab = "Average Traffic Volume"
      )


