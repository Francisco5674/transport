##### La fete case study #########
library(geosphere)  # For calculating distances
library(dplyr)
library(lubridate)
library(data.table)
library(leaflet)  # For plotting
library(h3jsr) 
library(sf)

# Load ggplot2
library(ggplot2)

setwd("C:/Users/Francisco/Desktop/Research/transport")

# raad files
shops_data <- fread("src/La_fete_shops.csv")
gps_data <- fread("out/gps_data_lafete_02082021.csv")

# Add a new ID column to gps_data
gps_data[, id := .I]

# Check the result
print(gps_data)

shops_data <- shops_data[, `:=`(
  longitude = as.numeric(sub("POINT \\((-?[0-9.]+) (-?[0-9.]+)\\)", "\\1", WKT)),
  latitude = as.numeric(sub("POINT \\((-?[0-9.]+) (-?[0-9.]+)\\)", "\\2", WKT))
)]

# Filter the gps_data to only include rows where stop == 1
stops_data <- gps_data[stop == 1]

# Create a new column `delivery` initialized to 0
stops_data[, delivery := 0]

# Loop through each stop and calculate distances to all shops
for (i in 1:nrow(stops_data)) {
  stop_lat <- stops_data$latitude[i]
  stop_lon <- stops_data$longitude[i]
  
  # Calculate distances from this stop to all shops
  distances <- distHaversine(
    matrix(c(stop_lon, stop_lat), ncol = 2),
    matrix(c(shops_data$longitude, shops_data$latitude), ncol = 2)
  )
  
  # If any distance is less than 500 meters, mark as a delivery
  if (any(distances < 500)) {
    stops_data$delivery[i] <- 1
  }
}

# Check the result
print(stops_data)

# Merge stops_data with gps_data
gps_data <- merge(gps_data, stops_data[, .(id, delivery)], by = "id", all.x = TRUE)

# Replace NA in the delivery column with 0 (for non-delivery stops)
gps_data[is.na(delivery), delivery := 0]

# Check the result
print(gps_data)

# Filter truck data for vehicle ID 943 and only keep delivery points
truck_data <- gps_data[vehicle_id == 6279]

# Create the leaflet map with a white background
leaflet(data = shops_data) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Use a white background
  # Add markers for each shop
  addCircleMarkers(
    ~longitude, ~latitude,
    color = "blue",
    fill = TRUE,
    fillColor = "blue",
    radius = 5,
    label = ~name,
    labelOptions = labelOptions(noHide = TRUE, direction = 'top')
  ) %>%
  # Add truck stops only for deliveries
  addCircleMarkers(
    data = truck_data[delivery == 1],
    ~longitude, ~latitude,
    color = "green",  # Green for deliveries
    fill = TRUE,
    radius = 5,
    labelOptions = labelOptions(noHide = TRUE, direction = 'top'),
    popup = ~paste("Delivery Timestamp:", timestamp) # Show timestamp in popup
  ) %>%
  # Add truck stops only for deliveries
  addCircleMarkers(
    data = truck_data[stop == 1 & !(delivery == 1)],
    ~longitude, ~latitude,
    color = "red",  # Green for deliveries
    fill = TRUE,
    radius = 5,
    labelOptions = labelOptions(noHide = TRUE, direction = 'top'),
    popup = ~paste("Delivery Timestamp:", timestamp) # Show timestamp in popup
  ) %>%
  # Add 500m circular buffer around each shop
  addCircles(
    ~longitude, ~latitude,
    radius = 500,  # 500 meters
    color = "blue",
    fillColor = "lightblue",
    fillOpacity = 0.3
  ) %>%
  # Add line for the truck's route using all GPS points
  addPolylines(
    data = truck_data[, .(longitude, latitude)],  # Filter for the specific truck
    lng = ~longitude, lat = ~latitude,
    color = "orange",  # Color for the truck's route
    weight = 2
  ) %>%
  # Set initial map view
  setView(lng = mean(shops_data$longitude), lat = mean(shops_data$latitude), zoom = 12)

