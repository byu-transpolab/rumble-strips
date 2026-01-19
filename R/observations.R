# Helper Functions related to observations_data.csv

library(tidyverse)

# Pivot observation data to show trailer spacing
pivot_trailer_spacing <- function(observations) {
  trailer_spacing <- observations %>%
    select( site, spacing_type, trailer_spacing) %>%
    pivot_wider(names_from = spacing_type, 
                values_from = trailer_spacing,
                values_fn = ~ paste(.x, collapse = "/"))

write.csv(trailer_spacing, "output/trailer_spacing.csv", row.names = FALSE)
return(trailer_spacing)
}


# Pivot observation data to show camera spacing data
pivot_camera_spacing <- function(observations) {
  camera_spacing <- observations %>%
    select( site, spacing_type, gopro_spacing) %>%
    pivot_wider(names_from = spacing_type, 
                values_from = gopro_spacing,
                values_fn = ~ paste(.x, collapse = "/"))

write.csv(camera_spacing, "output/camera_spacing.csv", row.names = FALSE)
return(camera_spacing)
}