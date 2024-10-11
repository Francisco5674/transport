library(data.table)
library(h3jsr) 
library(sf)
library(lubridate)
library(dplyr)

# Set the seed for reproducibility
set.seed(123)  # You can use any number
setwd("C:/Users/Francisco/Desktop/Research/transport/issue_tess")

# Data from Mati
gps_mati <- fread("src/one_driver_september.csv")
colnames(gps_mati) <- c("timestamp1","timestamp2","observation_id",
                        "timestamp3","latitude","longitude","truck_id","x1","x2")

# Set parameters and data
gps_data <- fread("src/ArchivoCompleto.csv")
gps_data$month <- format(gps_data$timestamp, "%m-%d")
gps_data <- gps_data[gps_data$month == "08-02"]
fwrite(gps_data, "src/gps_data_02082021.csv")
gps_data <- fread("src/gps_data_02082021.csv")
mobile_data <- fread("src/data_filtrada_02082021.csv")
sample_size <- 300000
hex_resolution <- 8

# The program

# sample
#gps_ac <- gps_ac %>% slice_sample(n = sample_size)
#gps_data <- gps_data %>% slice_sample(n = sample_size)
gps_data[,reference_id:=NULL]
mobile_data <- mobile_data %>% slice_sample(n = sample_size)

# points in mobile data
mobile_data$end_geo <- gsub("POINT\\(|\\)", "", mobile_data$end_geo)
coords <- do.call(rbind, strsplit(mobile_data$end_geo, " "))
mobile_data$longitude <- as.numeric(coords[, 1])
mobile_data$latitude <- as.numeric(coords[, 2])
mobile_data[,end_geo:=NULL]

# Tess
gps_data$hex <- point_to_cell(matrix(c(gps_data$longitude,gps_data$latitude),ncol=2),res=hex_resolution)
mobile_data$hex <- point_to_cell(matrix(c(mobile_data$longitude,mobile_data$latitude),ncol=2),res=hex_resolution)

#gps_data$date <- format(gps_data$timestamp, "%m-%d")
gps_data$hour <- hour(gps_data$timestamp)

#mobile_data$date <- format(mobile_data$end_absolutetime_ts, "%m-%d")
mobile_data$hour <- hour(mobile_data$end_absolutetime_ts)

joined_data <- gps_data[mobile_data, on = .(hex,hour), nomatch = 0,allow.cartesian=TRUE]
head(joined_data)
#contar personas, luego se puede segmentar por horarios o algo
#ademas, se puede contar km por personas tambien
conteo <- joined_data[,.(npeople = uniqueN(c(hour,hashed_imsi))),by=vehicle_id]
fwrite(conteo,"out/truck_people.csv")
