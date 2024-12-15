library(geosphere)  # For calculating distances
library(dplyr)
library(lubridate)
library(data.table)
library(leaflet)  # For plotting

# Load ggplot2
library(ggplot2)

setwd("C:/Users/Francisco/Desktop/Research/transport")

gps_data <- fread("src/gps_data_02082021.csv")

alpha <- 4*60
maxdist <- 0.25

# Ensure there are no missing latitude or longitude values
gps_data <- gps_data %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Ensure there are no 0 lat and long
gps_data <- gps_data %>%
  filter(!(latitude == 0) | !(longitude == 0))

# Define the bounding box for Santiago
lat_min <- -33.7
lat_max <- -33.2
lon_min <- -70.9
lon_max <- -70.4


# Sort data by vehicle_id and timestamp
gps_data <- gps_data %>%
  arrange(vehicle_id, timestamp)

# Function to calculate Haversine distance with vectorized check for NA values
calculate_haversine <- function(lat1, lon1, lat2, lon2) {
  # Using ifelse to handle NA values
  ifelse(
    is.na(lat1) | is.na(lon1) | is.na(lat2) | is.na(lon2),
    NA,  # Return NA if any coordinate is missing
    distHaversine(cbind(lon1, lat1), cbind(lon2, lat2)) / 1000  # Convert meters to kilometers
  )
}

# Calculate distances between consecutive points for each truck
gps_data <- gps_data %>%
  group_by(vehicle_id) %>%
  mutate(
    # Lag the lat/lon values to calculate distance between consecutive points
    prev_latitude = lag(latitude),
    prev_longitude = lag(longitude),
    # Calculate the distance between consecutive points
    distance_km = calculate_haversine(latitude, longitude, prev_latitude, prev_longitude),
    
    # Calculate the time difference between consecutive points in seconds
    prev_timestamp = lag(timestamp),
    time_diff_sec = as.numeric(difftime(timestamp, prev_timestamp, units = "secs"))
  ) %>%
  ungroup()

source("issue_tess/code/stops.R")

# Filter GPS points within the Santiago bounding box
gps_data <- gps_data %>%
  filter(latitude >= lat_min & latitude <= lat_max & longitude >= lon_min & longitude <= lon_max)

# Replace NA values in distance (first row of each group) with 0
gps_data$distance_km[is.na(gps_data$distance_km)] <- 0

# Replace NA values in time(first row of each group) with 0
gps_data$time_diff_sec[is.na(gps_data$time_diff_sec)] <- 0

# Summing total distance per vehicle
total_distance_per_truck <- gps_data %>%
  group_by(vehicle_id) %>%
  summarise(total_distance_km = sum(distance_km), total_stops = sum(stop))


# View the results
setorder(total_distance_per_truck, -"total_distance_km")
print(total_distance_per_truck)

# Firms and trucks
# Summarize data to get each unique vehicle_id for each name
vehicle_by_name <- gps_data %>%
  group_by(name) %>%
  summarise(vehicle_ids = paste(unique(vehicle_id), collapse = ", "))

# View the results
print(vehicle_by_name)

# Filter for the specific truck with vehicle_id
truck_data <- gps_data %>% filter(vehicle_id == 38575)

# Create a leaflet map to plot the route
leaflet(truck_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(
    lng = ~longitude,
    lat = ~latitude,
    color = "blue",
    weight = 2,
    opacity = 1
  ) %>%
  # Adding green markers only for stop points
  addCircleMarkers(
    data = truck_data %>% filter(stop == 1),  # Filter for stop points
    lng = ~longitude,
    lat = ~latitude,
    color = "green",
    radius = 2,
    popup = ~paste("Time:", timestamp, "<br>Distance (km):", distance_km)
  )  %>%
  addLegend(
    "bottomright",
    colors = c("blue", "red", "green"),
    labels = c("Route", "Points", "Stops"),
    title = "Truck Route"
  )

# Preparing coca cola data

cocacola <- c(38909,38913,38908,38915,38576,41895,41889,38575)
cocacola_gps_data <- gps_data %>% filter(vehicle_id %in% cocacola)

fwrite(cocacola_gps_data,"out/gps_data_cocacola_02082019.csv")

# Preparing la fete data

lafete <- c(943, 944, 2938, 6279, 49805)
lafete_gps_data <- gps_data %>% filter(vehicle_id %in% lafete)

fwrite(lafete_gps_data,"out/gps_data_lafete_02082021.csv")

# Filter data for each truck
truck_data_1 <- gps_data %>% filter(vehicle_id == 945)
truck_data_2 <- gps_data %>% filter(vehicle_id == 944)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Add polyline for the first truck
  addPolylines(
    data = truck_data_1,
    lng = ~longitude,
    lat = ~latitude,
    color = "blue",
    weight = 2,
    opacity = 1
  ) %>%
  addCircleMarkers(
    data = truck_data_1 %>% filter(stop == 1),
    lng = ~longitude,
    lat = ~latitude,
    color = "green",
    radius = 3,
    popup = ~paste("Time:", timestamp, "<br>Distance (km):", distance_km, "<br>Stop: Yes")
  ) %>%
  
  # Add polyline for the second truck
  addPolylines(
    data = truck_data_2,
    lng = ~longitude,
    lat = ~latitude,
    color = "orange",
    weight = 2,
    opacity = 1
  ) %>%
  addCircleMarkers(
    data = truck_data_2 %>% filter(stop == 1),
    lng = ~longitude,
    lat = ~latitude,
    color = "green",
    radius = 3,
    popup = ~paste("Time:", timestamp, "<br>Distance (km):", distance_km, "<br>Stop: Yes")
  ) %>%
  
  # Add legend
  addLegend(
    "bottomright",
    colors = c("blue", "orange", "green"),
    labels = c("Truck 1 Route", "Truck 2 Route", "Stops"),
    title = "Truck Stops"
  )




# Create a leaflet map to plot the route
leaflet(truck_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(
    lng = ~longitude,
    lat = ~latitude,
    color = "blue",
    weight = 2,
    opacity = 1
  ) %>%
  addLegend(
    "bottomright",
    colors = c("blue", "red"),
    labels = c("Route", "Points"),
    title = "Truck Route"
  )

truck_people <- fread("out/truck_people.csv")
truck <- merge(total_distance_per_truck, truck_people, by = "vehicle_id")
truck <- truck[truck$total_distance_km > 0,]

fwrite(truck, "out/truck.csv")

# Assuming `truck` is your data frame with total_stops and total_distance_km
ggplot(truck, aes(x = total_stops, y = total_distance_km)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +   # Plot points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a linear trend line
  labs(title = "Total Stops vs. Total Distance Travelled",
       x = "Total Stops",
       y = "Total Distance (km)") +  # Custom titles and labels
  theme_minimal(base_size = 15) +  # Use a minimalistic theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title

# Assuming `truck` is your data frame with total_stops and total_distance_km
ggplot(truck, aes(x = total_distance_km , y = npeople)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +   # Plot points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a linear trend line
  labs(title = "Total Distance Travelled vs. People",
       x = "Total Distance (km)",
       y = "People") +  # Custom titles and labels
  theme_minimal(base_size = 15) +  # Use a minimalistic theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title

# Assuming `truck` is your data frame with total_stops and total_distance_km
ggplot(truck, aes(x = total_stops , y = npeople)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +   # Plot points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a linear trend line
  labs(title = "Stops vs. People",
       x = "Stops",
       y = "People") +  # Custom titles and labels
  theme_minimal(base_size = 15) +  # Use a minimalistic theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title
