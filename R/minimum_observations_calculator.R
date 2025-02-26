# Intended to take in various settings and calculate the
# minimum time required to make statistically significant
# observations. 

#Define Variables########################################

#Margin of Error
E = 1

#Centrality Adjustment
 #0 for median, 1.04 for 85th percentile
U = 1.04

#z-score
z = 1.959964

#Standard Deviation
o = 3

#AADT
AADT = 500

# % AADT during the work zone
  #estimated with script AADT_daylight_percentage
p = 0.6630

#initial and final hours of work zone in 24-hour format
s = 8
f = 17

##Math#################################################

#calculates the minimum required observations
n = ((o^2) * (z^2) * ((U^2) + 2)) / (2 * (E^2))

n = ceiling(n)
#calculates the minimum required observation time
t = f - s

h = (t * n) / (AADT * p)

##Print Results#######################################

print(h)
