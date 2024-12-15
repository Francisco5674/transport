library(data.table)
library(h3jsr) 
library(sf)
library(lubridate)
library(dplyr)
library(leaflet)  # For plotting

setwd("C:/Users/Francisco/Desktop/Research/transport/issue_tess")

# Opening data
mobile_data <- fread("src/data_filtrada_04082021.csv")
hex_resolution <- 8

# Getting lat and long
mobile_data$end_geo <- gsub("POINT\\(|\\)", "", mobile_data$end_geo)
coords <- do.call(rbind, strsplit(mobile_data$end_geo, " "))
mobile_data$longitude <- as.numeric(coords[, 1])
mobile_data$latitude <- as.numeric(coords[, 2])
mobile_data[,end_geo:=NULL]

# Getting hours
mobile_data$hour <- hour(mobile_data$end_absolutetime_ts)

#### Assign H3 hexagons
mobile_data$hex <- point_to_cell(
  matrix(c(mobile_data$longitude,mobile_data$latitude),ncol=2),
  res=hex_resolution)

# Filter for the "peak_early" timeframe
filter_time = 21
mobile_data_filter <- mobile_data %>% filter(hour == filter_time)

# Aggregating data by hexagon for "peak_early"
hex_count <- mobile_data_filter %>%
  group_by(hex) %>%
  summarise(count = n())

# Convert H3 hexagons to polygons (GeoJSON format) for mapping
hex_polygons <- cell_to_polygon(hex_count$hex, simple = FALSE) %>%
  st_as_sf()

# Join the count data with the hex polygons
hex_polygons$count <- hex_count$count

# Apply logarithmic scaling to the counts (using log1p to handle zero counts)
hex_polygons$log_count <- log1p(hex_polygons$count)

# Create a leaflet heatmap for "peak_early" with logarithmic scale
leaflet(hex_polygons) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Base map
  addPolygons(
    fillColor = ~colorNumeric("YlOrRd", log_count)(log_count),  # Logarithmic color scale
    weight = 0.5,
    opacity = 1,
    color = 'white',
    fillOpacity = 0.2,
    smoothFactor = 0.2
  ) %>%
  addLegend(
    "bottomright",
    pal = colorNumeric("YlOrRd", hex_polygons$log_count),
    values = hex_polygons$log_count,
    title = paste("Log Count per Hex -",filter_time),
    opacity = 1
  )

