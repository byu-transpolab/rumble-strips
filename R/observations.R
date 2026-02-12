# Helper Functions related to observations_data.csv

library(tidyverse)

#' Pivot observation data to show trailer spacing
#'
#' @param observations data.frame or tibble. Input observations containing at
#'   minimum the columns `site`, `spacing_type`, and `trailer_spacing`.
#' @return A tibble (wide) with one row per `site` and a column for each
#'   `spacing_type`. Each cell contains the concatenated `trailer_spacing`
#'   values for that site/spacing (character). Side effect: writes
#'   "output/trailer_spacing.csv" with the resulting tibble.
pivot_trailer_spacing <- function(observations) {
  trailer_spacing <- observations %>%
    select( site, spacing_type, trailer_spacing) %>%
    pivot_wider(names_from = spacing_type, 
                values_from = trailer_spacing,
                values_fn = ~ paste(.x, collapse = "/"))

  write.csv(trailer_spacing, "output/trailer_spacing.csv", row.names = FALSE)
  return(trailer_spacing)
}


#' Pivot observation data to show camera spacing data
#'
#' @param observations data.frame or tibble. Input observations containing at
#'   minimum the columns `site`, `spacing_type`, and `gopro_spacing`.
#' @return A tibble (wide) with one row per `site` and a column for each
#'   `spacing_type`. Each cell contains the concatenated `gopro_spacing`
#'   values for that site/spacing (character). Side effect: writes
#'   "output/camera_spacing.csv" with the resulting tibble.
pivot_camera_spacing <- function(observations) {
  camera_spacing <- observations %>%
    select( site, spacing_type, gopro_spacing) %>%
    pivot_wider(names_from = spacing_type, 
                values_from = gopro_spacing,
                values_fn = ~ paste(.x, collapse = "/"))

  write.csv(camera_spacing, "output/camera_spacing.csv", row.names = FALSE)
  return(camera_spacing)
}