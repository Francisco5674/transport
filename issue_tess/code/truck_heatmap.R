library(geosphere)  # For calculating distances
library(dplyr)
library(lubridate)
library(data.table)
library(leaflet)  # For plotting
library(h3jsr) 
library(sf)

setwd("C:/Users/Francisco/Desktop/Research/transport/issue_tess")

# Step 1: Load GPS data and prepare the truck route
gps_data <- fread("src/gps_data_02082021.csv")

# Clean GPS data
gps_data <- gps_data %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  filter(!(latitude == 0) | !(longitude == 0)) %>%
  arrange(vehicle_id, timestamp)

# Define a bounding box for Santiago (approximate)
bbox_santiago <- list(
  lat_min = -33.75,  # Southern boundary
  lat_max = -33.25,  # Northern boundary
  lon_min = -70.95,  # Western boundary
  lon_max = -70.45   # Eastern boundary
)

# Function to calculate Haversine distance
calculate_haversine <- function(lat1, lon1, lat2, lon2) {
  ifelse(
    is.na(lat1) | is.na(lon1) | is.na(lat2) | is.na(lon2),
    NA,
    distHaversine(cbind(lon1, lat1), cbind(lon2, lat2)) / 1000
  )
}

# Add distance to GPS data
gps_data <- gps_data %>%
  group_by(vehicle_id) %>%
  mutate(
    prev_latitude = lag(latitude),
    prev_longitude = lag(longitude),
    distance_km = calculate_haversine(latitude, longitude, prev_latitude, prev_longitude)
  ) %>%
  ungroup()

gps_data$distance_km[is.na(gps_data$distance_km)] <- 0

# Filter the truck with vehicle_id
truck_data <- gps_data %>% filter(vehicle_id == 11338)

# Step 2: Load mobile data and prepare the heatmap
mobile_data <- fread("src/data_filtrada_04082021.csv")
hex_resolution <- 8

# Extract latitude and longitude from 'end_geo' column
mobile_data$end_geo <- gsub("POINT\\(|\\)", "", mobile_data$end_geo)
coords <- do.call(rbind, strsplit(mobile_data$end_geo, " "))
mobile_data$longitude <- as.numeric(coords[, 1])
mobile_data$latitude <- as.numeric(coords[, 2])
mobile_data[,end_geo := NULL]

# Extract hour from timestamp
mobile_data$hour <- hour(mobile_data$end_absolutetime_ts)

# Assign H3 hexagons
mobile_data$hex <- point_to_cell(
  matrix(c(mobile_data$longitude, mobile_data$latitude), ncol = 2),
  res = hex_resolution
)

# Filter for specific timeframe (e.g., 21:00)
filter_time <- 17
mobile_data_filter <- mobile_data %>% filter(hour == filter_time)

# Aggregate data by hexagon
hex_count <- mobile_data_filter %>%
  group_by(hex) %>%
  summarise(count = n())

# Convert H3 hexagons to polygons
hex_polygons <- cell_to_polygon(hex_count$hex, simple = FALSE) %>%
  st_as_sf()

# Join the count data with the hex polygons
hex_polygons$count <- hex_count$count
hex_polygons$log_count <- log1p(hex_polygons$count)

# Step 3: Combine truck route and heatmap on the same leaflet map
leaflet() %>%
  # Set view to Santiago (centered at -33.45, -70.65)
  setView(lng = -70.65, lat = -33.45, zoom = 12) %>%
  
  # Add base map
  addProviderTiles("CartoDB.Positron") %>%
  
  # Add the hexagon heatmap
  addPolygons(
    data = hex_polygons,
    fillColor = ~colorNumeric("YlOrRd", log_count)(log_count),
    weight = 0.5,
    opacity = 0.6,
    color = 'white',
    fillOpacity = 0.4,
    smoothFactor = 0.2
  ) %>%
  
  # Add truck route as a polyline
  addPolylines(
    data = truck_data,
    lng = ~longitude,
    lat = ~latitude,
    color = "blue",
    weight = 2,
    opacity = 1,
    popup = ~paste("Time:", timestamp, "<br>Distance (km):", distance_km)
  ) %>%
  
  # Add individual points for the truck's GPS data
  addCircleMarkers(
    data = truck_data,
    lng = ~longitude,
    lat = ~latitude,
    color = "red",
    radius = 3,
    popup = ~paste("Time:", timestamp, "<br>Distance (km):", distance_km)
  ) %>%
  
  # Add legends
  addLegend(
    "bottomright",
    pal = colorNumeric("YlOrRd", hex_polygons$log_count),
    values = hex_polygons$log_count,
    title = paste("Log Count per Hex -", filter_time),
    opacity = 1
  ) %>%
  addLegend(
    "bottomleft",
    colors = c("blue", "red"),
    labels = c("Route", "Points"),
    title = "Truck Route"
  )
