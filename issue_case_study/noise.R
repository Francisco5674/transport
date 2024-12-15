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
gps_data <- fread("out/gps_data_cocacola_02082019.csv")

# Step 2: Load mobile data
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

# Adding id to mobile data
mobile_data[, mobpoint_id := .I]

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
joined_data$db <- 110 - 6*log2(joined_data$distance_to_i_coords)

# Statistics
# La fete effects on people

# how many people has CocaCola affected?
uniqueN(joined_data$hashed_imsi)

# people average effects
# Calculate the average db for each hashed_imsi
avg_db_by_imsi <- joined_data %>%
  group_by(hashed_imsi) %>%
  summarise(avg_db_to_i = max(db, na.rm = TRUE))

# Calculate the average db for each hashed_imsi
sum_db_by_imsi_point <- joined_data %>%
  group_by(hashed_imsi,mobpoint_id) %>%
  summarise(max_db_to_i = max(db, na.rm = TRUE))

sum_db_by_imsi <- sum_db_by_imsi_point %>%
  group_by(hashed_imsi) %>%
  summarise(sum_db_to_i = sum(max_db_to_i, na.rm = TRUE))

sum_db_by_imsi <- sum_db_by_imsi[sum_db_by_imsi$sum_db_to_i>0,]
sum_db_by_imsi$log_sum <- log(sum_db_by_imsi$sum_db_to_i)

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

# Calculate histogram with relative frequencies
hist_data <- hist(sum_db_by_imsi$log_sum, breaks = 50, plot = FALSE)  # Set plot to FALSE to get data first
hist_data$counts <- hist_data$counts / sum(hist_data$counts)  # Normalize counts to relative frequencies

# Plot histogram
plot(hist_data, 
     main = "People affected by sound ",
     xlab = "Sound perceived (db)", 
     ylab = "Relative Frequency", 
     col = "skyblue", 
     freq = FALSE)


