##### case study (Noise) #########
library(geosphere)  # For calculating distances
library(dplyr)
library(lubridate)
library(data.table)
library(leaflet)  # For plotting
library(h3jsr) 
library(sf)
# Use knitr to create LaTeX table
library(knitr)
library(kableExtra)

# Load ggplot2
library(ggplot2)

setwd("C:/Users/Francisco/Desktop/Research/transport")

# Step 1: Load gps data
gps_data <- fread("src/gps_data_02082021.csv")

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

# Correct column selection for gps_data
gps_data <- gps_data[, .(name, vehicle_id, latitude, longitude, timestamp)]

# Correct column selection for mobile_data
mobile_data <- mobile_data[, .(hashed_imsi, latitude, longitude, end_absolutetime_ts)]

# Extract 5 rows from each dataset
gps_sample <- gps_data[1:5, ]
mobile_sample <- mobile_data[1:5, ]

# GPS Data Table
gps_latex <- kable(gps_sample, format = "latex", booktabs = TRUE, caption = "Extract of GPS Data") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Print LaTeX Code
cat(gps_latex, "\n\n")

# Limit 'hashed_imsi' to first 5 characters and add ellipses
mobile_sample$hashed_imsi <- paste0(substr(mobile_sample$hashed_imsi, 1, 5), "...")

mobile_latex <- kable(mobile_sample, format = "latex", booktabs = TRUE, caption = "Extract of Mobile Data") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Print LaTeX code
cat(mobile_latex)


