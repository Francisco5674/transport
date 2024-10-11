library(geosphere)  # For calculating distances
library(dplyr)
library(lubridate)
library(data.table)
library(leaflet)  # For plotting

setwd("C:/Users/Francisco/Desktop/Research/transport/issue_tess")

gps_data <- fread("src/gps_data_02082021.csv")

# Ensure there are no missing latitude or longitude values
gps_data <- gps_data %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Ensure there are no 0 lat and long
gps_data <- gps_data %>%
  filter(!(latitude == 0) | !(longitude == 0))

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
    distance_km = calculate_haversine(latitude, longitude, prev_latitude, prev_longitude)
  ) %>%
  ungroup()

# Replace NA values in distance (first row of each group) with 0
gps_data$distance_km[is.na(gps_data$distance_km)] <- 0

# Summing total distance per vehicle
total_distance_per_truck <- gps_data %>%
  group_by(vehicle_id) %>%
  summarise(total_distance_km = sum(distance_km))

# View the results
setorder(total_distance_per_truck, -"total_distance_km")
print(total_distance_per_truck)

# Filter for the specific truck with vehicle_id
truck_data <- gps_data %>% filter(vehicle_id == 11338)

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
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = "red",
    radius = 3,
    popup = ~paste("Time:", timestamp, "<br>Distance (km):", distance_km)
  ) %>%
  addLegend(
    "bottomright",
    colors = c("blue", "red"),
    labels = c("Route", "Points"),
    title = "Truck Route"
  )

truck_people <- fread("out/truck_people.csv")
truck <- merge(total_distance_per_truck, truck_people, by = "vehicle_id")

plot(truck$total_distance_km,truck$npeople)
