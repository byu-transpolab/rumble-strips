# Install and load the necessary packages
install.packages("ggplot2")
install.packages("extrafont")
install.packages("Cairo")
install.packages("svglite")
library(ggplot2)
library(extrafont)
library(svglite)

# Import fonts
font_import()
loadfonts(device = "win")

# Read the .csv file
data_for_chart <- read.csv("~/Documents/GitHub/rumble-strips/data/test_spacing_update_1.csv")

# Create the line graph with updated title and font settings
plot <- ggplot(data_for_chart, aes(x = speed, y = spacing, color = specifications)) +
  geom_line() +
  labs(title = "Spacing Specifications For Posted Speed Limits",
       x = "Speed (mph)",
       y = "Strip Spacing (ft)",
       color = "Specifications") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 20),
        plot.title = element_text(hjust = 1.0, size = 20),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

# Print the plot to view it in RStudio
print(plot)

#svg("~/Documents/GitHub/rumble-strips/0717")
#hist(your_data)
#dev.off()
