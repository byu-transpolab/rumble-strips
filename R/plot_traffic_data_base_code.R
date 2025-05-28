library(tidyverse)
library(lubridate)

# Load the CSV, skipping the first 3 rows
raw_data <- read_csv("WavetronixUnitData - 5-14-2025.csv", skip = 3, col_names = FALSE)

# Assign column names manually (only the relevant ones shown here)
colnames(raw_data)[1:16] <- c("Lane", "Volume", "Occupancy", "AvgSpeed", "Speed85", 
                              "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13", 
                              "Headway", "Gap", "Timestamp")

# Convert Timestamp to datetime
raw_data <- raw_data %>%
  mutate(Timestamp = parse_date_time(Timestamp, orders = c("mdy HMS", "mdy HM")))

# Filter for a specific date (e.g., April 14, 2025)
filtered_data <- raw_data %>%
  filter(date(Timestamp) == as.Date("2025-04-14"))

# Plot volume vs. time for both lanes
ggplot(filtered_data, aes(x = Timestamp, y = as.numeric(Volume), color = Lane)) +
  geom_line() +
  scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M") +
  labs(title = "Traffic Volume by Lane on April 14, 2025",
       x = "Time of Day",
       y = "Vehicle Volume",
       color = "Lane") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
