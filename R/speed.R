# R/speed.R
# Helper functions related to driver speed analysis.

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

#' Prepare tibble for statistical tests
#'
#' @param wavetronix data.frame or tibble. Wavetronix measurements containing at
#'   minimum the columns `lane`, `site`, `unit`, `date`, `time`, and
#'   `speed_85`.
#' @param observations data.frame or tibble. Observations tibble containing at
#'   minimum the columns `site`, `date`, and `spacing_type` used to join with
#'   wavetronix rows.
#' @return A tibble filtered to lane "01" with columns `site`, `unit` (factor),
#'   `date`, `time` (from `time`), `speed_85` (numeric) and `spacing_type`.
#'   Rows with missing `speed_85` or missing `spacing_type` are removed.
prepare_speed_data <- function(wavetronix, observations) {
  wavetronix %>%
    filter(lane == "01") %>%
    select(site, unit, date,time, speed_85) %>%
    mutate(unit = as.factor(as.character(unit)),
           speed_85 = as.numeric(speed_85)) %>%
    left_join(observations %>% select(site, date, spacing_type), by = c("site", "date")) %>%
    filter(!is.na(speed_85), !is.na(spacing_type))
}


#' Perform paired t-tests on speed_85 grouped by site and spacing_type
#'
#' @param speed_data data.frame or tibble. Prepared speed data (see
#'   prepare_speed_data) containing `site`, `unit`, `time`, `speed_85`, and
#'   `spacing_type`. Units should be named consistently (e.g., "w1", "w2").
#' @return A tibble with one row per `site` x `spacing_type` containing the
#'   t-test results: `statistic`, `p_value`, `mean_diff` (w2 - w1 estimate),
#'   `conf_low`, and `conf_high`. If a valid paired test cannot be calculated,
#'   numeric columns contain NA.
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

#' Plot confidence bounds for paired t-test results
#'
#' @param paired_t_test data.frame or tibble. Output of paired_test containing
#'   `site`, `spacing_type`, `mean_diff`, `conf_low`, and `conf_high`.
#' @return A ggplot object showing mean differences with horizontal 95% confidence
#'   intervals per `site`, colored by `spacing_type`. The plot object is returned
#'   (not written to disk).
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
  # return the plot object so it can be saved in a separate target
  p
}

#' Run one-sample t-tests on a single unit grouped by site and spacing_type
#'
#' @param speed_data data.frame or tibble. Prepared speed data (see
#'   prepare_speed_data) containing `site`, `unit`, `speed_85`, and
#'   `spacing_type`.
#' @param unit character. Unit identifier to test (e.g., "w1" or "w2"). Default
#'   is "w1".
#' @return A tibble with one row per `site` x `spacing_type` containing the
#'   one-sample t-test results for the specified unit: `statistic`, `p_value`,
#'   `mean`, `conf_low`, and `conf_high`. If the test cannot be performed, the
#'   numeric columns contain NA.
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

#' Plot confidence bounds for single-unit t-test results
#'
#' @param single_unit_t_test data.frame or tibble. Output of
#'   run_single_unit_t_test containing `site`, `spacing_type`, `mean`,
#'   `conf_low`, and `conf_high`.
#' @param unit character. Unit identifier used to create the file name and axis
#'   label (e.g., "w1"). Default is "w1".
#' @return A ggplot object showing mean speeds with 95% confidence intervals per
#'   `site`, colored by `spacing_type`. The plot object is returned (and the
#'   function also computes a filename but does not save the plot).
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
  # return the plot object so it can be saved in a separate target
  p
}