##### CocaCola case study #########
library(geosphere)  # For calculating distances
library(dplyr)
library(lubridate)
library(data.table)
library(leaflet)  # For plotting
library(h3jsr) 
library(sf)
library(rgdal)  # For reading shapefiles

# Ignore truck 41887,38583,41896

# Load ggplot2
library(ggplot2)

setwd("C:/Users/Francisco/Desktop/Research/transport")

# raad files
shops_data <- fread("src/Coca_cola_points.csv")
gps_data <- fread("out/gps_data_cocacola_02082019.csv")

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
truck_data <- gps_data[vehicle_id == 38575]


# Replace "path_to_commune_file" with the path to your file
commune_boundaries <- st_read("src/comunas.geojson")

# Ensure CRS (Coordinate Reference System) matches
commune_boundaries <- st_transform(commune_boundaries, crs = 4326)  # WGS 84 for Leaflet

# Create the leaflet map with a white background
leaflet(data = shops_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
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
    fill = FALSE
  ) %>%
  # Add line for the truck's route using all GPS points
  addPolylines(
    data = truck_data[, .(longitude, latitude)],  # Filter for the specific truck
    lng = ~longitude, lat = ~latitude,
    color = "orange",  # Color for the truck's route
    weight = 2
  ) %>%  # Use a white background
  # Add markers for each shop
  addCircleMarkers(
    ~longitude, ~latitude,
    color = "blue",
    fill = TRUE,
    fillColor = "blue",
    radius = 5,
    #label = ~name,
    labelOptions = labelOptions(noHide = TRUE, direction = 'top')
  ) %>%
  # Set initial map view
  setView(lng = mean(shops_data$longitude), lat = mean(shops_data$latitude), zoom = 12)


leaflet() %>%
  # Add base map
  addProviderTiles("CartoDB.Positron") %>%
  
  # Add hexagonal heatmap polygons
  addPolygons(
    data = hex_polygons,
    fillColor = ~colorNumeric("YlOrRd", log_count)(log_count),  # Logarithmic color scale
    weight = 0.5,
    opacity = 1,
    color = 'white',
    fillOpacity = 0.3,
    smoothFactor = 0.2
  ) %>%
  
  # Add heatmap legend
  addLegend(
    "bottomright",
    pal = colorNumeric("YlOrRd", hex_polygons$log_count),
    values = hex_polygons$log_count,
    title = paste("Log Count per Hex -", filter_time),
    opacity = 1
  ) %>%
  
  # Add truck stops for deliveries
  addCircleMarkers(
    data = truck_data[delivery == 1],
    ~longitude, ~latitude,
    color = "green",  # Green for deliveries
    fill = TRUE,
    radius = 5,
    labelOptions = labelOptions(noHide = TRUE, direction = 'top'),
    popup = ~paste("Delivery Timestamp:", timestamp) # Show timestamp in popup
  ) %>%
  
  # Add truck stops for non-deliveries
  addCircleMarkers(
    data = truck_data[stop == 1 & !(delivery == 1)],
    ~longitude, ~latitude,
    color = "red",  # Red for non-deliveries
    fill = TRUE,
    radius = 5,
    labelOptions = labelOptions(noHide = TRUE, direction = 'top'),
    popup = ~paste("Stop Timestamp:", timestamp) # Show timestamp in popup
  ) %>%
  
  # Add 500m circular buffer around each shop
  addCircles(
    data = shops_data,
    ~longitude, ~latitude,
    radius = 500,  # 500 meters
    color = "blue",
    fill = FALSE
  ) %>%
  
  # Add line for the truck's route using all GPS points
  addPolylines(
    data = truck_data[, .(longitude, latitude)],  # Filter for the specific truck
    lng = ~longitude, lat = ~latitude,
    color = "purple",  # Color for the truck's route
    weight = 2
  ) %>%
  
  # Add markers for each shop
  addCircleMarkers(
    data = shops_data,
    ~longitude, ~latitude,
    color = "blue",
    fill = TRUE,
    fillColor = "blue",
    radius = 5,
    labelOptions = labelOptions(noHide = TRUE, direction = 'top')
  ) %>%
  
  # Set initial map view
  setView(
    lng = mean(shops_data$longitude),
    lat = mean(shops_data$latitude),
    zoom = 12
  )
