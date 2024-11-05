

# Identify stops
#  <- gps_data[gps_data$vehicle_id == 943,]
gps_data$stop <- 0
r <- 1
R <- gps_data[r,]
S <- R
while (r < nrow(gps_data)){
  if (R$vehicle_id == S$vehicle_id){
    while (calculate_haversine(R$latitude,R$longitude,S$latitude,S$longitude) < maxdist){
      r <- r + 1 
      if (r > nrow(gps_data)){
        break
      }
      S <- gps_data[r,]
      time <- as.numeric(difftime(S$timestamp, R$timestamp, units = "secs"))
    }
    if (time > alpha ){
      pS <- gps_data[r,]
      if (!(calculate_haversine(R$latitude,R$longitude,pS$latitude,pS$longitude)) == 0){
        gps_data[r, "stop"] <- 1
      }
    }
    R <- gps_data[r,]
    S <- R
  }
  else {
    R = S
  }
}


