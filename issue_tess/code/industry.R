### Plotting industry effects ####
library(dplyr)
# Load necessary libraries
library(ggplot2)
library(ggrepel)

gps_data <- fread("src/gps_data_02082021.csv")
truck <- fread("out/truck.csv")
industries <- fread("src/firms_transport.csv")

# Group by vehicle_id and summarize unique names
vehicle_names <- gps_data %>%
  group_by(vehicle_id) %>%
  summarise(name = paste(unique(name), collapse = ", ")) %>%
  ungroup()

# View the results
print(vehicle_names)

truck <- merge(vehicle_names, truck, by = "vehicle_id")
truck <- merge(truck,industries, by = "name")

ratio_by_industry <- truck %>%
  group_by(industry) %>%
  summarise(
    total_npeople = sum(npeople, na.rm = TRUE),
    total_distance_km = sum(total_distance_km, na.rm = TRUE),
    total_stops = sum(total_stops, na.rm = TRUE),
    ratio_pk = total_npeople / total_distance_km,
    ratio_ps = total_npeople / total_stops,
    ratio_sk = total_stops / total_distance_km
  )

print(ratio_by_industry)

# Create the scatter plot
ggplot(ratio_by_industry, aes(x = total_distance_km, y = total_npeople, label = industry)) +
  geom_point(color = "blue", size = 4) +  # Plot points
  geom_text_repel(aes(label = industry), size = 5, box.padding = 0.35, point.padding = 0.5) +  # Add labels
  labs(title = "Total People vs. Total Distance by Industry",
       x = "Total Distance (km)",
       y = "Total People") +
  theme_minimal()

# Create the scatter plot
ggplot(ratio_by_industry, aes(x = total_stops, y = total_npeople, label = industry)) +
  geom_point(color = "blue", size = 4) +  # Plot points
  geom_text_repel(aes(label = industry), size = 5, box.padding = 0.35, point.padding = 0.5) +  # Add labels
  labs(title = "Total People vs. Total Stops",
       x = "Total Stops",
       y = "Total People") +
  theme_minimal()
