##### case study (Noise) #########
library(geosphere)  # For calculating distances
library(dplyr)
library(lubridate)
library(data.table)
library(leaflet)  # For plotting
library(h3jsr) 
library(sf)

# Load ggplot2
library(ggplot2)

# Step 1: Load gps data
gps_data <- fread("out/gps_data_lafete_02082021.csv")

# Step 2: Load mobile data and prepare the heatmap
mobile_data <- fread("src/data_filtrada_04082021.csv")
# 2225 meters of radio 
hex_resolution <- 7

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
gps_data$hex <- point_to_cell(
  matrix(c(gps_data$longitude,gps_data$latitude),
         ncol=2),
  res=hex_resolution)
# adding time to the gps_data
gps_data$hour <- hour(gps_data$timestamp)

# big merge
joined_data <- gps_data[mobile_data, on = .(hex,hour), nomatch = 0,allow.cartesian=TRUE]
head(joined_data)

# distance from each truck
# Calculate the distance and add as a new column
joined_data$distance_to_i_coords <- mapply(function(lat, lon, i_lat, i_lon) {
  distHaversine(c(lon, lat), c(i_lon, i_lat))
}, joined_data$latitude, joined_data$longitude, joined_data$i.latitude, joined_data$i.longitude)

# Check the updated data with the new distance column
head(joined_data)

# sound measurement
joined_data$db <- 80 - 6*log2(joined_data$distance_to_i_coords)

# Statistics
# La fete effects on people

# how many people has lafete affected?
uniqueN(joined_data$hashed_imsi)

# people average effects
# Calculate the average db for each hashed_imsi
avg_db_by_imsi <- joined_data %>%
  group_by(hashed_imsi) %>%
  summarise(avg_db_to_i = mean(db, na.rm = TRUE))

# Calculate histogram with relative frequencies
hist_data <- hist(avg_db_by_imsi$avg_db_to_i, breaks = 50, plot = FALSE)  # Set plot to FALSE to get data first
hist_data$counts <- hist_data$counts / sum(hist_data$counts)  # Normalize counts to relative frequencies

# Plot histogram
plot(hist_data, 
     main = "People affected by sound ",
     xlab = "Sound perceived (db)", 
     ylab = "Relative Frequency", 
     col = "skyblue", 
     freq = FALSE)
