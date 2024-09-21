########### R file to explore SimpliRoute data ############
library(data.table)
library(geosphere)

data <- fread("src/ArchivoCompleto.csv")


for (colum in colnames(data)){
    print(paste("###############", colum, "###############"))
    uv <- nrow(unique(data[,colum, with = FALSE]))
    print(paste("Unique values:", uv))
}

print(table(data$name))

firms <- data[,.(vehicles = length(unique(vehicle_id))), by = "name"]

print(firms)
print(paste("Total =", sum(firms$vehicles)))

##### one vehicle ###############

vehicle <- data[vehicle_id == 12047]
setorder(vehicle, "timestamp")

total_distance <- 0
for (r in 2:nrow(vehicle)){
    spoint <- c(vehicle$longitude[r - 1], vehicle$latitude[r - 1])
    fpoint <- c(vehicle$longitude[r], vehicle$latitude[r])
    distance <- distm(spoint, fpoint, fun = distHaversine)
    distance <- distance[1]
    total_distance <- total_distance + distance
}
print(paste("Vehicle", 12047, "has traveled", total_distance, "m"))

