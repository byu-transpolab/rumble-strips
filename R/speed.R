# This file contains helper functions related to driver speed analysis.

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

# prepare tibble for statistical tests
prepare_speed_data <- function(wavetronix, observations) {
  wavetronix %>%
    filter(lane == "01") %>%
    select(site, unit, date, time = sensor_time, speed_85) %>%
    mutate(unit = as.factor(as.character(unit)),
           speed_85 = as.numeric(speed_85)) %>%
    left_join(observations %>% select(site, date, spacing_type), by = c("site", "date")) %>%
    filter(!is.na(speed_85), !is.na(spacing_type))
}


# perform t-tests on speed_85 grouped by site, unit, spacing_type
paired_test <- function(speed_data) {
  grouped <- speed_data %>%
    group_by(site, spacing_type) %>%
    nest()
  
  results <- grouped %>%
    mutate(
      t_test = map(data, ~ {
        wide <- .x %>%
          select(time, unit, speed_85) %>%
          pivot_wider(names_from = unit, values_from = speed_85) %>%
          filter(!is.na(w1), !is.na(w2))
        
        if ("w1" %in% names(wide) && "w2" %in% names(wide) &&
            nrow(wide) >= 2) {
          test <- tryCatch(
            t.test(wide$w2, wide$w1, paired = TRUE),
            error = function(e) NULL
          )
          
          if (!is.null(test)) {
            tibble(
              statistic = test$statistic,
              p_value = test$p.value,
              mean_diff = test$estimate,
              conf_low = test$conf.int[1],
              conf_high = test$conf.int[2]
            )
          } else {
            tibble(statistic = NA, p_value = NA, mean_diff = NA,
                   conf_low = NA, conf_high = NA)
          }
        } else {
          tibble(statistic = NA, p_value = NA, mean_diff = NA,
                 conf_low = NA, conf_high = NA)
        }
      })
    ) %>%
    unnest(t_test)

  results
}

plot_confidence_bounds <- function(paired_t_test) {
  # Prepare data
  plot_data <- paired_t_test %>%
    filter(!is.na(conf_low), !is.na(conf_high))

  # Build plot
  p <- ggplot(plot_data, aes(y = site, 
                             x = mean_diff, 
                             color = spacing_type)) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                  orientation = "y",
                  position = position_dodge(width = 0.6),
                  height = 0.4,
                  linewidth = 1.2) +
    geom_point(position = position_dodge(width = 0.6), size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c(
      "NO TPRS" = "#2E4756",
      "UDOT" = "#3C7A89",
      "1:2" = "#F49D37",
      "LONG" = "#D81159" ),
      breaks = c("LONG", "1:2", "UDOT", "NO TPRS")) +
    labs(
      x = "Speed Difference",
      y = "Site",
      color = "Spacing Type"
    )

  # Save and return
  ggsave("output/change-in-speeds.svg", plot = p, width = 10, height = 8)
  p
}

# run t-tests on speed data for only the given unit.
run_single_unit_t_test <- function(speed_data, unit = "w1") {
  grouped <- speed_data %>%
    group_by(site, spacing_type) %>%
    nest()
  
  single_unit_t_test <- grouped %>%
    mutate(
      t_test = map(data, ~ {
        unit_data <- .x %>%
          filter(unit == !!unit) %>%
          select(speed_85)
        
        if (nrow(unit_data) >= 2) {
          test <- tryCatch(
            t.test(unit_data$speed_85, mu = 0),
            error = function(e) NULL
          )
          
          if (!is.null(test)) {
            tibble(
              statistic = test$statistic,
              p_value = test$p.value,
              mean = test$estimate,
              conf_low = test$conf.int[1],
              conf_high = test$conf.int[2]
            )
          } else {
            tibble(statistic = NA, p_value = NA, mean = NA,
                   conf_low = NA, conf_high = NA)
          }
        } else {
          tibble(statistic = NA, p_value = NA, mean = NA,
                 conf_low = NA, conf_high = NA)
        }
      })
    ) %>%
    unnest(t_test)

  single_unit_t_test
}

# Plot the confidence bounds for single unit t-test results
plot_single_unit_confidence_bounds <- function(
  single_unit_t_test, unit = "w1") {

  # create the file name now with the shortened unit label ie w1 or w2
  file_name <- paste0("output/single-unit-speed-", unit, ".svg")

  # Prepare data
  plot_data <- single_unit_t_test %>%
    # Filter out any rows with NA confidence intervals
    filter(!is.na(conf_low), !is.na(conf_high)) %>%
    # mutate unit label for plotting
    mutate(
      unit = case_when(
        unit == "w1" ~ "Wavetronix Unit 1",
        unit == "w2" ~ "Wavetronix Unit 2"
      )
    )

  # Build plot
  p <- ggplot(plot_data, aes(y = site, 
                             x = mean, 
                             color = spacing_type)) +
    geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                  orientation = "y",
                  position = position_dodge(width = 0.6),
                  height = 0.4,
                  linewidth = 1.2) +
    geom_point(position = position_dodge(width = 0.6), size = 2) +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = c(
      "NO TPRS" = "#2E4756",
      "UDOT" = "#3C7A89",
      "1:2" = "#F49D37",
      "LONG" = "#D81159" ),
      breaks = c("LONG", "1:2", "UDOT", "NO TPRS")) +
    scale_x_continuous(expand = expansion(mult = 0.05)) +
    labs(
      x = paste("Mean Speed for", unit),
      y = "Site",
      color = "Spacing Type"
    )

  # Save and return
  ggsave(
    file_name,
    plot = p,
    width = 10,
    height = 8)
  p
}