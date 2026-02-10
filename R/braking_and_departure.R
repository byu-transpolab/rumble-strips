# Helper functions related to Driver braking and TPRS avoidance rates.

library(tidyverse)

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

    # Return plot for future use in _targets.R and save file in separate target
    p
}

# ==== Plot Departure data ==== #
plot_departure <- function(brake_and_departure) {

    p <- brake_and_departure %>%
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

    # Return plot for future use in _targets.R and save file in separate target
    p
}


#' Estimate models of TPRS avoidance
#' 
#' @param brake_and_departure Tibble with braking and departure data
#' @return List of models, one pooled as well as segmentations by site
estimate_avoid_models <- function(brake_and_departure) {
    df <- brake_and_departure |>
        mutate(
            departure = ifelse(departure == "Avoided", 1, 0),
            class = factor(class, levels = c("Motorcycle", "Passenger", "Truck")),
            class = fct_relevel(class, "Motorcycle", after = Inf) # move motorcycle to the end
        )

    # pooled model
    avoidance_model <- glm(departure ~ class + spacing_type + site, data = df, family = binomial)

    # site-specific models
    avoidance_sites <- df |>
        group_by(site) |>
        nest(data = -site) |>
        mutate(
            model = map(data, ~glm(departure ~ class + spacing_type, data = .x, family = binomial))
        )

    models <- c(
        list(avoidance_model),
          avoidance_sites$model
        ) |>
        set_names(c("Pooled", avoidance_sites$site))
}


#' Estimate models of TPRS avoidance
#' 
#' @param brake_and_departure Tibble with braking and departure data
#' @return List of models, one pooled as well as segmentations by site
estimate_brake_models <- function(brake_and_departure) {
    df <- brake_and_departure |>
        mutate(
            brake = factor(brake, levels = c("After", "Before", "No Brake")),
            brake = fct_relevel(brake, c("No Brake", "Before", "After")), 
            class = factor(class, levels = c("Motorcycle", "Passenger", "Truck")),
            class = fct_relevel(class, "Motorcycle", after = Inf) # move motorcycle to the end
        )

    # pooled model
    avoidance_model <- nnet::multinom(brake ~ class + spacing_type + site, data = df, family = multinomial, Hess = TRUE)

    # site-specific models
    avoidance_sites <- df |>
        group_by(site) |>
        nest(data = -site) |>
        mutate(
            model = map(data, ~nnet::multinom(brake ~ class + spacing_type, data = .x, family = multinomial, Hess = TRUE))
        )

    models <- c(
        list(avoidance_model),
          avoidance_sites$model
        ) |>
        set_names(c("Pooled", avoidance_sites$site))
}