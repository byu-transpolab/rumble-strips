
# Stacked bar charts by spacing type
# Shows departure (avoided/not avoided) and brake (before/after/no brake) behavior
# by vehicle class, grouped by spacing type (UDOT, NO TPRS, PSS, LONG)

suppressPackageStartupMessages({
  library(tidyverse)
  library(svglite)
  library(scales)
  library(gridExtra)
  library(grid)
})

# ==== VALIDATE DATA ====
if (!exists("cb")) {
  stop("Object `cb` not found. Make sure `cb <- tar_read(camera_back_data)` has been run and is in the environment.")
}

# Load observation data to get spacing_type
obs_data_path <- "/Users/benjaminhailstone/Documents/GitHub/rumble-strips/data/observation_data.csv"
if (!file.exists(obs_data_path)) {
  stop("observation_data.csv not found at: ", obs_data_path)
}

obs_data <- read_csv(obs_data_path, show_col_types = FALSE) %>%
  mutate(
    site = tolower(as.character(site)),
    date = as.Date(date, format = "%m/%d/%Y"),
    spacing_type = as.character(spacing_type)
  )

# ==== CLEAN AND PREPARE CB DATA ====
# Detect columns
candidate_cols <- names(cb)

detect_brake_col <- function(df, cols) {
  for (nm in cols) {
    vals <- tolower(as.character(df[[nm]]))
    has_before    <- any(grepl("^\\s*before\\s*$",    vals))
    has_no_brake  <- any(grepl("^\\s*no\\s+brake\\s*$", vals))
    has_after     <- any(grepl("^\\s*after\\s*$",     vals))
    if (has_before || has_no_brake || has_after) return(nm)
  }
  NA_character_
}

detect_avoid_col <- function(df, cols) {
  for (nm in cols) {
    vals <- tolower(as.character(df[[nm]]))
    has_avoided <- any(grepl("^\\s*avoided\\s*$", vals))
    has_not_avoided <- any(grepl("^\\s*not\\s+avoided\\s*$", vals))
    if (has_avoided || has_not_avoided) return(nm)
  }
  NA_character_
}

brake_col <- detect_brake_col(cb, candidate_cols)
avoid_col <- detect_avoid_col(cb, candidate_cols)

if (is.na(brake_col)) {
  stop("Could not auto-detect the brake column in `cb`. Expected values include 'before', 'no brake', and 'after'.")
}
if (is.na(avoid_col)) {
  stop("Could not auto-detect the avoidance/departure column in `cb`. Expected values include 'avoided' and 'not avoided'.")
}

# Clean cb data
cb_clean <- cb %>%
  mutate(
    class_raw = tolower(as.character(.data[["class"]])),
    class = case_when(
      class_raw %in% c("motorcycle", "motercycle") ~ "motorcycle",
      class_raw %in% c("passenger", "truck") ~ class_raw,
      TRUE ~ class_raw
    ),
    site = tolower(as.character(.data[["site"]])),
    brake = tolower(as.character(.data[[brake_col]])),
    departure = tolower(as.character(.data[[avoid_col]])),
    date = as.Date(.data[["date"]])
  ) %>%
  filter(
    class %in% c("passenger", "motorcycle", "truck"),
    brake %in% c("before", "no brake", "after"),
    departure %in% c("avoided", "not avoided")
  )

# ==== JOIN WITH OBSERVATION DATA ====
# Join cb_clean with obs_data to get spacing_type for each observation
cb_with_spacing <- cb_clean %>%
  left_join(obs_data %>% select(date, site, spacing_type), 
            by = c("date", "site")) %>%
  filter(!is.na(spacing_type))

# ==== PREPARE DATA FOR PLOTTING ====
# Create separate datasets for departure and brake

# Departure data (2 categories: avoided, not avoided)
departure_data <- cb_with_spacing %>%
  count(spacing_type, departure, class) %>%
  mutate(
    behavior_type = "Departure",
    behavior = departure
  )

# Brake data (3 categories: before, after, no brake)
brake_data <- cb_with_spacing %>%
  count(spacing_type, brake, class) %>%
  mutate(
    behavior_type = "Brake",
    behavior = brake
  )

# Combine both datasets
plot_data <- bind_rows(departure_data, brake_data) %>%
  mutate(
    spacing_type = factor(spacing_type, levels = c("UDOT", "NO TPRS", "PSS", "LONG")),
    class = factor(class, levels = c("motorcycle", "truck", "passenger")),
    behavior = factor(behavior, levels = c("avoided", "not avoided", "after", "before", "no brake")),
    behavior_label = case_when(
      behavior == "avoided" ~ "Avoided",
      behavior == "not avoided" ~ "Not Avoided",
      behavior == "before" ~ "Before",
      behavior == "no brake" ~ "No Brake",
      behavior == "after" ~ "After"
    ),
    behavior_label = factor(behavior_label, levels = c("Avoided", "Not Avoided", "After", "Before", "No Brake"))
  )

# ==== DEFINE COLOR PALETTE ====
# Different colors for each vehicle class
class_colors <- c(
  "passenger"  = "#2C7FB8",  # blue
  "motorcycle" = "#66C2A4",  # green
  "truck"      = "#D7301F"   # red
)

# ==== CREATE SEPARATE PLOTS ====
# Configure output directory (portable)
output_dir <- file.path(path.expand("~"), "CloudStorage/Box-Box/2024-tprs/output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Common theme for both plots
common_theme <- theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 15),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14, color = "grey40")
  )

# Plot 1: Departure behavior (avoided/not avoided)
departure_plot_data <- plot_data %>% 
  filter(behavior %in% c("avoided", "not avoided"))

p_departure <- ggplot(departure_plot_data, aes(x = behavior_label, y = n, fill = class)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~ spacing_type, scales = "free_x", nrow = 1) +
  scale_fill_manual(
    values = class_colors,
    labels = c("Motorcycle", "Truck", "Passenger"),
    name = "Vehicle Class"
  ) +
  labs(
    title = "Vehicle Departure Behavior by Spacing Type",
    subtitle = "Stacked bars showing vehicle class distribution for avoided vs. not avoided",
    x = "Departure Behavior",
    y = "Count"
  ) +
  common_theme

# Plot 2: Brake behavior (before/after/no brake)
brake_plot_data <- plot_data %>% 
  filter(behavior %in% c("before", "after", "no brake"))

p_brake <- ggplot(brake_plot_data, aes(x = behavior_label, y = n, fill = class)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~ spacing_type, scales = "free_x", nrow = 1) +
  scale_fill_manual(
    values = class_colors,
    labels = c("Motorcycle", "Truck", "Passenger"),
    name = "Vehicle Class"
  ) +
  labs(
    title = "Vehicle Braking Behavior by Spacing Type",
    subtitle = "Stacked bars showing vehicle class distribution for braking timing",
    x = "Braking Behavior",
    y = "Count"
  ) +
  common_theme

# Display both plots
print(p_departure)
print(p_brake)

# Save plots as separate SVGs
cat("\nSaving departure bar chart as SVG...\n")
departure_path <- file.path(output_dir, "departure_behavior_bar_chart.svg")
ggsave(departure_path, plot = p_departure, width = 16, height = 8, units = "in", device = "svg")
cat("Departure chart saved to:", departure_path, "\n")

cat("\nSaving brake bar chart as SVG...\n")
brake_path <- file.path(output_dir, "brake_behavior_bar_chart.svg")
ggsave(brake_path, plot = p_brake, width = 16, height = 8, units = "in", device = "svg")
cat("Brake chart saved to:", brake_path, "\n")

# ==== CREATE DATA TABLE ====
# Prepare table data with proper formatting
table_data <- plot_data %>%
  select(spacing_type, behavior_label, class, n) %>%
  arrange(spacing_type, behavior_label, desc(class)) %>%
  mutate(
    class_label = case_when(
      class == "passenger" ~ "Passenger",
      class == "truck" ~ "Truck",
      class == "motorcycle" ~ "Motorcycle"
    )
  ) %>%
  select(spacing_type, behavior_label, class_label, n)

# Create a wide format table for better visualization
table_wide <- table_data %>%
  pivot_wider(
    names_from = spacing_type,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(behavior_label, desc(class_label))

# Display the data table in a viewer tab (like cb, ct, wv, w_e_d)
utils::View(table_wide, title = "Vehicle Counts by Spacing Type and Behavior")

# Format the table
table_grob <- tableGrob(
  table_wide,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontsize = 12),
      bg_params = list(fill = c(rep(c("grey95", "white"), length.out = nrow(table_wide))))
    ),
    colhead = list(
      fg_params = list(fontsize = 13, fontface = "bold"),
      bg_params = list(fill = "grey80")
    )
  )
)

# Add title to table
title <- textGrob("Vehicle Counts by Spacing Type and Behavior",
                  gp = gpar(fontsize = 16, fontface = "bold"))
table_with_title <- arrangeGrob(title, table_grob, 
                                heights = unit.c(unit(0.5, "inches"), unit(1, "npc") - unit(0.5, "inches")))

# Display the table
grid.newpage()
grid.draw(table_with_title)

# Save table as SVG
cat("\nSaving data table as SVG...\n")
table_path <- file.path(output_dir, "stacked_bar_data_table.svg")
ggsave(table_path, plot = table_with_title, 
       width = 12, height = 10, units = "in", device = "svg")
cat("Data table saved to:", table_path, "\n")

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n\n")
cat("Total observations by spacing type:\n")
print(cb_with_spacing %>% count(spacing_type))

cat("\n\nBreakdown by spacing type and behavior:\n")
print(plot_data %>% 
  group_by(spacing_type, behavior) %>% 
  summarise(total = sum(n), .groups = "drop"))

cat("\n\nBreakdown by spacing type, behavior, and class:\n")
print(plot_data %>% arrange(spacing_type, behavior, class))

