# Helper functions related to Driver braking and TPRS avoidance rates.

library(tidyverse)
library(svglite)
library(scales)
library(gridExtra)
library(grid)

# ==== Combine camera_back_data with observations ======= #
compile_brake_and_departure <- function(camera_back_data, observations) {

    brake_and_departure <- camera_back_data %>%
        left_join(observations %>% select(date, site, spacing_type),
            by = c("date", "site")) %>%
        # Capitalize the factors for better presentation
        mutate(
            brake = str_to_title(brake),
            departure = str_to_title(departure),
            class = str_to_title(class)
        ) %>%
        # Only keep the necessary rows
        select(site, date, spacing_type, class, brake, departure)
}

# ==== Plot Braking data ==== #
plot_braking <- function(brake_and_departure) {

    # Generate the plot
    p <- brake_and_departure %>%
        ggplot(aes(x = brake)) +
        geom_bar(fill = "steelblue") +
        facet_grid(
            rows = vars(class),
            cols = vars(spacing_type),
            scales = "free_y"
        ) +
        labs(x = "Brake", y = "Count") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        )

    # Save the plot to output/ and return the plot for _targets.R
    ggsave("output/braking_plot.svg", 
        plot = p, width = 10, height = 13)
    p
}

# ==== Plot Departure data ==== #
plot_departure <- function(brake_and_departure) {

    departure_plot <- brake_and_departure %>%
        # Remove the "NO TPRS" data as vehicles cannot avoid what is not there.
        filter(spacing_type != "NO TPRS") %>%
        ggplot(aes(x = departure)) +
        geom_bar(fill = "darkorange") +
        facet_grid(
            rows = vars(class),
            cols = vars(spacing_type),
            scales = "free_y"
        ) +
        labs(x = "Departure", y = "Count") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        )

    # Save the plot to output/ and return the plot for _targets.R
    ggsave("output/departure_plot.svg", 
        plot = p, width = 10, height = 13)
    p
}