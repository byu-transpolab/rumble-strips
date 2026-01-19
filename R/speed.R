# This file contains helper functions related to driver speed analysis.

library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

# prepare tibble for statistical tests
prepare_speed_data <- function(wavetronix, observations) {
  wavetronix %>%
    dplyr::filter(lane == "01") %>%
    select(site, unit, date, time = sensor_time, speed_85) %>%
    mutate(unit = as.factor(as.character(unit)),
           speed_85 = as.numeric(speed_85)) %>%
    left_join(observations %>% select(site, date, spacing_type), by = c("site", "date")) %>%
    filter(!is.na(speed_85), !is.na(spacing_type)) %>%
    mutate(spacing_type = fct_relevel(spacing_type, "NO TPRS", "UDOT", "PSS", "LONG"))
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
    mutate(spacing_type = as.factor(spacing_type),
    site = case_when(
      site == "sr12" ~ "SR-12",
      site == "us6" ~ "US-6",
      site == "i70" ~ "I-70",
      site == "us191" ~ "US-191",
      TRUE ~ site)
    ) %>%
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
      "PSS" = "#F49D37",
      "LONG" = "#D81159" ),
      breaks = c("LONG", "PSS", "UDOT", "NO TPRS")) +
    labs(
      x = "Speed Difference",
      y = "Site",
      color = "Spacing Type"
    )

  # Save and return
  ggsave("output/change-in-speeds.svg", plot = p, width = 10, height = 8)
  p
}