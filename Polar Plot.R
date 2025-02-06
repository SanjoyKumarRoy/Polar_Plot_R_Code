# Packages Installed
install.packages("dplyr")
install.packages("ggplot2")

# Libraries
library(dplyr)
library(ggplot2)

# Average Monthly Maximum Temperature Data set
T_max <- read.csv("Maximum_Temperature.csv") # Maximum Temperature (Dhaka)
View(T_max)
names(T_max)
unique(T_max$Station)

# Select only the years 2019-2023
T_max1 <- T_max |> 
  filter(Year >= 2019 & Year <= 2023) 
View(T_max1)
min(T_max1$T.max)
max(T_max1$T.max)
sum(is.na(T_max1))

# Polar Plot for Average Monthly Maximum Temperature (2019-2023)
ggplot(T_max1, aes(x = Month, y = T.max, group = Year, color = Year)) +
  geom_line(size = 1, alpha = 0.8) +           # Adjust line thickness and transparency
  coord_polar(start = 0, direction = 1) +     # Set the circular direction of the plot
  scale_x_continuous(
    breaks = 1:12, 
    labels = month.abb,                       # Use abbreviated month names (Jan, Feb, etc.)
    expand = c(0, 0),                         # Remove padding around the axis
    limits = c(1, 13)                         # Ensure the full range of months (1 to 12) is used
  ) +
  scale_y_continuous(
    expand = c(0.05, 0),                      # Reduce the lower gap
    limits = c(25, 40),                       # Set radial axis range from 25°C to 40°C
    breaks = seq(25, 40, by = 5),             # Add breaks every 5°C
    labels = seq(25, 40, by = 5), # Label the radial axis with temperature levels
  ) +
  scale_color_gradient(
    low = "turquoise3", high = "violetred",   # Gradient from turquoise3 to violetred
    name = "Year"
  ) +
  labs(
    title = "Average Monthly Maximum Temperature (°C)",
    subtitle = "Dhaka City (2019-2023)",
    x = NULL, 
    y = NULL                                  # Remove y-axis label
  ) +
  annotate("text", x = 4, y = 25, label = "25", size = 3, color = "black") +
  annotate("text", x = 4, y = 30, label = "30", size = 3, color = "black") +
  annotate("text", x = 4, y = 31, label = "|", size = 2, color = "black") +
  annotate("text", x = 4, y = 32, label = "|", size = 2, color = "black") +
  annotate("text", x = 4, y = 33, label = "|", size = 2, color = "black") +
  annotate("text", x = 4, y = 34, label = "|", size = 2, color = "black") +
  annotate("text", x = 4, y = 35, label = "35", size = 3, color = "black") +
  annotate("text", x = 4, y = 36, label = "|", size = 2, color = "black") +
  annotate("text", x = 4, y = 37, label = "|", size = 2, color = "black") +
  annotate("text", x = 4, y = 38, label = "|", size = 2, color = "tomato") +
  annotate("text", x = 4, y = 39, label = "|", size = 2, color = "red") +
  annotate("text", x = 4, y = 40, label = "40", size = 3, color = "black") +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),  # Keep month labels horizontal
    axis.text.y = element_blank(),                                   # Remove radial axis labels
    axis.ticks = element_blank(),                                    # Remove all axis ticks
    panel.grid.minor = element_blank(),                              # Remove minor grid lines
    plot.title = element_text(hjust = 0.5, vjust = 1),               # Align title
    plot.subtitle = element_text(hjust = 0.5),                       # Align subtitle
    plot.margin = margin(15, 100, 15, 100)                           # Add margins to expand space around the plot
  )



# Average Monthly Minimum Temperature Data Set 
T_min <- read.csv("Minimum_Temperature.csv") # Minimum Temperature (Dhaka)
View(T_min)

# Select only the years 2019-2023
T_min1 <- T_min |> 
  filter( Year >= 2019 & Year <= 2023) 
View(T_min1)
min(T_min1$T.min) # Show the minimum value
max(T_min1$T.min) # Show the maximum value

# Polar Plot for Average Monthly Minimum Temperature (2019-2023)
ggplot(T_min1, aes(x = Month, y = T.min, group = Year, color = Year)) +
  geom_line(size = 1, alpha = 0.8) +           # Adjust line thickness and transparency
  coord_polar(start = 0, direction = 1) +     # Set the circular direction of the plot
  scale_x_continuous(
    breaks = 1:12, 
    labels = month.abb,                       # Use abbreviated month names (Jan, Feb, etc.)
    expand = c(0, 0),                         # Remove padding around the axis
    limits = c(1, 13)                         # Ensure the full range of months (1 to 12) is used
  ) +
  scale_y_continuous(
    expand = c(0.05, 0),                      # Reduce the lower gap
    limits = c(13, 30),                       # Set radial axis range from 13°C to 30°C
    breaks = seq(13, 30, by = 5),             # Add breaks every 5°C
    labels = seq(13, 30, by = 5), # Label the radial axis with temperature levels
  ) +
  scale_color_gradient(
    low = "turquoise3", high = "violetred",   # Gradient from turquoise3 to violetred
    name = "Year"
  ) +
  labs(
    title = "Average Monthly Minimum Temperature (°C)",
    subtitle = "Dhaka City (2019-2023)",
    x = NULL, 
    y = NULL                                  # Remove y-axis label
  ) +
  annotate("text", x = 4, y = 15, label = "15", size = 2, color = "black") +
  annotate("text", x = 4, y = 20, label = "20", size = 2, color = "black") +
  annotate("text", x = 4, y = 30, label = "30", size = 2, color = "black") +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),  # Keep month labels horizontal
    axis.text.y = element_blank(),                                   # Remove radial axis labels
    axis.ticks = element_blank(),                                    # Remove all axis ticks
    panel.grid.minor = element_blank(),                              # Remove minor grid lines
    plot.title = element_text(hjust = 0.5, vjust = 1),               # Align title
    plot.subtitle = element_text(hjust = 0.5),                       # Align subtitle
    plot.margin = margin(15, 100, 15, 100)                           # Add margins to expand space around the plot
  )



# Average Monthly Rain Fall Data Set
Rain <- read.csv("Rainfall.csv")
View(Rain)

# Select only the years 2019-2023
Rain1 <- Rain |> 
  filter( Year >= 2019 & Year <= 2023)
View(Rain1)


# Polar Plot of Average Rain Fall of Dhaka (2019-2023)
ggplot(Rain1, aes(x = Month, y = Rainfall, group = Year, color = Year)) +
  geom_line(size = 1, alpha = 0.8) +           # Adjust line thickness and transparency
  coord_polar(start = 0, direction = 1) +     # Set the circular direction of the plot
  scale_x_continuous(
    breaks = 1:12, 
    labels = month.abb,                       # Use abbreviated month names (Jan, Feb, etc.)
    expand = c(0, 0),                         # Remove padding around the axis
    limits = c(1, 13)                         # Ensure the full range of months (1 to 12) is used
  ) +
  scale_y_continuous(
    expand = c(0.05, 0),                      # Reduce the lower gap
    limits = c(0, 600),                       # Set radial axis range from 0 mm to 600 mm
    breaks = seq(0, 600, by = 100),             # Add breaks every 100 mm
    labels = seq(0, 600, by = 100), # Label the radial axis with temperature levels
  ) +
  scale_color_gradient(
    low = "turquoise3", high = "violetred",   # Gradient from turquoise3 to violetred
    name = "Year"
  ) +
  labs(
    title = "Average Monthly Rainfall (mm)",
    subtitle = "Dhaka City (2019-2023)",
    x = NULL, 
    y = NULL                                  # Remove y-axis label
  ) +
  annotate("text", x = 7, y = 0, label = "0", size = 2, color = "black") +
  annotate("text", x = 7, y = 50, label = "50", size = 2, color = "black") +
  annotate("text", x = 7, y = 100, label = "100", size = 2, color = "black") +
  annotate("text", x = 7, y = 200, label = "200", size = 2, color = "black") +
  annotate("text", x = 7, y = 300, label = "300", size = 2, color = "black") +
  annotate("text", x = 7, y = 400, label = "400", size = 2, color = "black") +
  annotate("text", x = 7, y = 500, label = "500", size = 2, color = "black") +
  annotate("text", x = 7, y = 600, label = "600", size = 2, color = "black") +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),  # Keep month labels horizontal
    axis.text.y = element_blank(),                                   # Remove radial axis labels
    axis.ticks = element_blank(),                                    # Remove all axis ticks
    panel.grid.minor = element_blank(),                              # Remove minor grid lines
    plot.title = element_text(hjust = 0.5, vjust = 1),               # Align title
    plot.subtitle = element_text(hjust = 0.5),                       # Align subtitle
    plot.margin = margin(15, 100, 15, 100)                           # Add margins to expand space around the plot
  )


# Average Monthly Air Quality Index (AQI) Data Set (2020-2024)
AQI <- read.csv("AQI.csv")
View(AQI)
sum(is.na(AQI))

min(AQI$AQI)
max(AQI$AQI)

# Polar Plot of Average Monthly AQI of Dhaka (2020-2024)
ggplot(AQI, aes(x = Month, y = AQI, group = Year, color = Year)) +
  geom_line(size = 1, alpha = 0.8) +           # Adjust line thickness and transparency
  coord_polar(start = 0, direction = 1) +     # Set the circular direction of the plot
  scale_x_continuous(
    breaks = 1:12, 
    labels = month.abb,                       # Use abbreviated month names (Jan, Feb, etc.)
    expand = c(0, 0),                         # Remove padding around the axis
    limits = c(1, 13)                         # Ensure the full range of months (1 to 12) is used
  ) +
  scale_y_continuous(
    expand = c(0.05, 0),                      # Reduce the lower gap
    limits = c(50, 350),                       # Set radial axis range from 50 to 350
    breaks = seq(50, 350, by = 100),             # Add breaks every 100
    labels = seq(50, 350, by = 100), # Label the radial axis with temperature levels
  ) +
  scale_color_gradient(
    low = "turquoise3", high = "violetred",   # Gradient from turquoise3 to violetred
    name = "Year"
  ) +
  labs(
    title = "Average Monthly AQI",
    subtitle = "Dhaka City (2020-2024)",
    x = NULL, 
    y = NULL                                  # Remove y-axis label
  ) +
  annotate("text", x = 1, y = 50, label = "50", size = 2, color = "black") +
  annotate("text", x = 1, y = 150, label = "150", size = 2, color = "black") +
  annotate("text", x = 1, y = 100, label = "100", size = 2, color = "black") +
  annotate("text", x = 1, y = 200, label = "200", size = 2, color = "black") +
  annotate("text", x = 1, y = 250, label = "250", size = 2, color = "black") +
  annotate("text", x = 1, y = 300, label = "300", size = 2, color = "black") +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", angle = 0),  # Keep month labels horizontal
    axis.text.y = element_blank(),                                   # Remove radial axis labels
    axis.ticks = element_blank(),                                    # Remove all axis ticks
    panel.grid.minor = element_blank(),                              # Remove minor grid lines
    plot.title = element_text(hjust = 0.5, vjust = 1),               # Align title
    plot.subtitle = element_text(hjust = 0.5),                       # Align subtitle
    plot.margin = margin(15, 100, 15, 100)                           # Add margins to expand space around the plot
  )
