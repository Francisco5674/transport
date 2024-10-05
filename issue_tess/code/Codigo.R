library(data.table)
library(h3jsr) 
library(sf)
library(lubridate)

gps_data <- fread("src/ArchivoCompleto.csv")
mobile_data <- fread("src/data_filtrada_02082021.csv")
mobile_data$end_geo <- gsub("POINT\\(|\\)", "", mobile_data$end_geo)
coords <- do.call(rbind, strsplit(mobile_data$end_geo, " "))
mobile_data$longitude <- as.numeric(coords[, 1])
mobile_data$latitude <- as.numeric(coords[, 2])
mobile_data[,end_geo:=NULL]

gps_data <- gps_data[1:100000]
gps_data[,reference_id:=NULL]
mobile_data <- mobile_data[1:1000000]

hex_resolution <- 8
gps_data$hex <- point_to_cell(matrix(c(gps_data$longitude,gps_data$latitude),ncol=2),res=hex_resolution)
mobile_data$hex <- point_to_cell(matrix(c(mobile_data$longitude,mobile_data$latitude),ncol=2),res=hex_resolution)
gps_data[,hora:=hour(timestamp)]
mobile_data[,hora:=hour(end_absolutetime_ts)]

joined_data <- gps_data[mobile_data, on = .(hex,hora), nomatch = 0,allow.cartesian=TRUE]
head(joined_data)
#contar personas, luego se puede segmentar por horarios o algo
#ademas, se puede contar km por personas tambien
conteo <- joined_data[,.N,by=vehicle_id]
conteo <- joined_data[,.(length(unique(hashed_imsi))),by=vehicle_id]
