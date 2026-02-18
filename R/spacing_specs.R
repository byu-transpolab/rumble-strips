# R/spacing_specs.R
# Helper functions related to plotting the various spacing specifications
# from other states and what we used in the methodology

library(tidyverse)
library(readxl)

#' Read in test_spacing and prep it for plotting
#' 
#' @param file_path str pointing to the csv file
#' @return tibble with the approriate info ready for plotting
get_test_spacing <- function(file_path) {
  test_spacing <- read_csv(file_path) %>%
    select(specifications, speed, spacing) %>%
    mutate_if(is.character, as.factor) %>%
    filter(!specifications %in% c("Recommended"))
}


#' Read in state_spacing and prep it for plotting
#' 
#' @param file_path str pointing to the csv file
#' @return tibble with the approriate info ready for plotting
get_state_spacing <- function(file_path) {
  state_spacing <- read_csv(file_path) %>%
    select(state, speed, spacing) %>%
    mutate_if(is.character, as.factor) %>% 
    # including these states makes plot facetting awkward and don't show
    # any different spacing strategy that isn't already represented
    filter(!state %in% c("California", "North Dakota"))
}


#' Plot test spacing data
#' 
#' @param state_spacing tibble with the state spacing specs
#' @return ggplot object organizing all the state specifications
plot_test_spacing <- function(state_spacing) {

  p <- ggplot(
    state_spacing, 
    aes(x = speed, y = spacing, color = specifications)
    ) + 
    geom_line() + 
    labs(
      x = "Speed [mph]",
      y = "Strip spacing [ft]",
      color = "Specifications"
    ) + 
    theme_minimal() +
    theme(
      text = element_text(
        size = 14, 
        family = "Times New Roman")
    )
    
  p
}


#' Plot state spacing specs faceted by state
#' 
#' @param state_spacing tibble with all the state spacing data
#' @return ggplot object state spacing plot
plot_state_spacing <- function(state_spacing) {
  p <- ggplot(
    state_spacing, 
    aes(x = speed, y = spacing)
    ) + 
    geom_line() + 
    xlab("Speed [mph]") + 
    ylab("Strip spacing [ft]") +
    theme_minimal() + 
    #axis labels in 14 pt Times New Roman font
    theme(
      text = element_text(size = 14, 
      family = "Times New Roman")
    ) +
    facet_wrap(
      ~factor(state,
        # These states are put into a very particular order
        # The final plot shows clear patterns and the states are grouped
        # to best show that pattern.
        levels = c("Wisconsin",   "Florida",  "Colorado",
                    "Maryland",    "Virginia", "Minnesota",
                    "New York",    "Texas",    "Missouri", 
                    "Recommended", "Utah",     "Linear Spacing")),
        drop = TRUE, # removes any NA plots
        ncol = 3 # sets number of columns
    ) + 
    #pane titles in 14 pt Times New Roman font
    theme(
      strip.text = element_text(size = 14,
      family = "Times New Roman"))
  
  p
}


#' Plot old_test_spacing data
#' 
#' @param old_test_spacing tibble with the older version of the testing specs
#' @return ggplot object displaying all the possible test specs
plot_old_test_spacing <- function(old_test_spacing) {

  p <- ggplot(
    old_test_spacing, 
    aes(x = speed, y = spacing, color = specifications)
    ) +
    geom_line() +
    labs(
      x = "Speed (mph)",
      y = "Strip Spacing (ft)",
      color = "Specifications") +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman", size = 20),
      plot.title = element_text(hjust = 1.0, size = 20),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
  
  p
}