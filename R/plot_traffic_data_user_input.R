# Suppress warning messages
options(readr.num_columns = 0)

# Set working directory
setwd("~/Documents/GitHub/rumble-strips/Wavetronix Data/Test Data")

# Load libraries
library(tidyverse)
library(lubridate)

# Load the CSV as character columns to avoid parsing issues
raw_data <- read_csv("WavetronixUnitData - 5-14-2025.csv", skip = 3, col_names = FALSE, col_types = cols(.default = "c"))

# Preview first few rows
cat("Preview of raw data:\n")
print(head(raw_data, 5))

# Prompt user for row range
cat("Enter the starting row number to analyze: ")
start_row <- as.integer(readline())

cat("Enter the ending row number to analyze: ")
end_row <- as.integer(readline())

# Subset the data to the specified row range
wvx_data <- raw_data[start_row:end_row, ]

# Check column count
if (ncol(wvx_data) < 16) {
  stop("Selected rows do not contain enough columns. Please choose a different row range.")
}

# Assign column names manually (first 16 shown here)
colnames(wvx_data)[1:16] <- c("Lane", "Volume", "Occupancy", "Average Speed", "85th Percentile Speed", 
                              "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13", 
                              "Headway", "Gap", "Timestamp")

# Convert relevant columns to numeric
numeric_cols <- c("Volume", "Occupancy", "Average Speed", "85th Percentile Speed", "Headway", "Gap")
wvx_data[numeric_cols] <- lapply(wvx_data[numeric_cols], as.numeric)

# Filter out malformed rows
wvx_data <- wvx_data %>%
  filter(Lane %in% c("LANE_01", "LANE_02"))

# Convert Timestamp to datetime
wvx_data <- wvx_data %>%
  mutate(Timestamp = parse_date_time(Timestamp, orders = c("mdy HMS", "mdy HM")))

# Check timestamp parsing
cat("Timestamp summary:\n")
print(summary(wvx_data$Timestamp))

# Prompt user for date
cat("Enter the date you want to view (YYYY-MM-DD): ")
user_date <- as.Date(readline())

# Prompt user for variable
variables <- c("Volume", "Occupancy", "Average Speed", "85th Percentile Speed", "Headway", "Gap")
cat("Choose a variable to plot:\n")
for (i in seq_along(variables)) {
  cat(i, ":", variables[i], "\n")
}

repeat {
  var_choice <- as.integer(readline("Enter the number of your choice: "))
  if (!is.na(var_choice) && var_choice >= 1 && var_choice <= length(variables)) {
    y_var <- variables[var_choice]
    break
  } else {
    cat("Invalid input. Please enter a number between 1 and", length(variables), "\n")
  }
}

# Filter data for selected date
filtered_data <- wvx_data %>%
  filter(date(Timestamp) == user_date)

# Check if data exists for selected date
cat("Number of rows for selected date:", nrow(filtered_data), "\n")
if (nrow(filtered_data) == 0) {
  stop("No data found for the selected date. Please check your input or row range.")
}

# Plot
print(
  ggplot(filtered_data, aes(x = Timestamp, y = as.numeric(.data[[y_var]]), color = Lane)) +
    geom_line() +
    scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M") +
    labs(title = paste(y_var, "by Lane on", user_date),
         x = "Time of Day",
         y = y_var,
         color = "Lane") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
