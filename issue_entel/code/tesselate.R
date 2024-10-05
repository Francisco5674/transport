########### R file to explore SimpliRoute data ############
library(data.table)
library(geosphere)
library(ggplot2)
library(sf)
library(dplyr)  # For sampling
library(maps)  # For enhanced mapping

# WD
setwd("C:/Users/Francisco/Desktop/Research/transport/issue_entel")

# Load the dataset
data <- fread("src/data_filtrada_03082021.csv")

# Extract longitude and latitude from the end_geo column
data[, `:=` (
  longitude = as.numeric(sub("POINT\\((-?[0-9.]+) .*", "\\1", end_geo)),
  latitude = as.numeric(sub("POINT\\(.* (-?[0-9.]+)\\)", "\\1", end_geo))
)]

# Randomly sample 1,000 points from the large dataset
sample_data <- data %>% sample_n(10000)

# Convert the sampled data into a spatial object using sf
data_sf <- st_as_sf(sample_data, coords = c("longitude", "latitude"), crs = 4326)

# Define the number of grid cells
n_rows <- 30
n_cols <- 30

# Define the boundaries for Santiago based on your previous plot
min_longitude <- -70.85
max_longitude <- -70.45
min_latitude <- -33.7
max_latitude <- -33.3

# Create the grid
lat_seq <- seq(min_latitude, max_latitude, length.out = n_rows + 1)
long_seq <- seq(min_longitude, max_longitude, length.out = n_cols + 1)

# Assign each point to a grid zone
sample_data[, grid_x := findInterval(longitude, long_seq)]
sample_data[, grid_y := findInterval(latitude, lat_seq)]

# Calculate the width and height of each grid cell in meters
lon_range_meters <- abs((max_longitude - min_longitude) / n_cols) * 111320  # Approximate conversion for longitude
lat_range_meters <- abs((max_latitude - min_latitude) / n_rows) * 111139  # Approximate conversion for latitude

# Assign grid ID based on the grid_x and grid_y
sample_data[, grid_id := (grid_y - 1) * n_cols + grid_x]

# Calculate the area of one quadrilateral
area <- lon_range_meters * lat_range_meters  # Area in square meters
print(paste("Area of each quadrilateral:", area, "square meters"))

# Calculate the density of points per square meter for each grid_id
density_data <- sample_data %>%
  group_by(grid_id) %>%
  summarise(
    count = n(),  # Number of points in each grid
    density = count / area  # Density: points per square meter
  )

# Add grid positions for plotting
density_data <- density_data %>%
  mutate(
    grid_x = (grid_id - 1) %% n_cols + 1,
    grid_y = (grid_id - 1) %/% n_cols + 1,
    center_longitude = long_seq[grid_x] + diff(long_seq)[1] / 2,
    center_latitude = lat_seq[grid_y] + diff(lat_seq)[1] / 2
  )

# Print the first few rows of the density data
print(density_data)


# Print the calculated area and first few rows of updated data
print(paste("Area of each quadrilateral:", area, "square meters"))
print(sample_data[, .(hashed_imsi, end_absolutetime_ts, longitude, latitude, grid_id)])

# Convert the sampled data into a spatial object using sf
data_sf <- st_as_sf(sample_data, coords = c("longitude", "latitude"), crs = 4326)

# Assuming you downloaded a shapefile/GeoJSON of Santiago boundaries
santiago_boundaries <- st_read("src/PRC Región Metropolitana Límite Urbano.geojson")

# Plot the points on a map of Santiago, Chile with grid lines
ggplot() +
  geom_sf(data = santiago_boundaries, fill = "gray80", color = "black") +  # Add Santiago boundaries
  geom_sf(data = data_sf, color = "red", size = 0.5) +  # Plot points in red
  geom_hline(yintercept = lat_seq, color = "white", linewidth = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = long_seq, color = "white", linewidth = 0.5) +  # Add vertical grid lines
  coord_sf(xlim = c(min_longitude, max_longitude), ylim = c(min_latitude, max_latitude), expand = TRUE) +  # Focus on Santiago region
  theme_minimal() +
  labs(title = "Sampled Geographic Plot on Santiago, Chile", 
       x = "Longitude", 
       y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels to prevent overlap

# Plot the heat map of the density with Santiago boundaries
ggplot() +
  geom_sf(data = santiago_boundaries, fill = "gray80", color = "black") +  # Add Santiago boundaries
  geom_tile(data = density_data, aes(x = center_longitude, y = center_latitude, fill = density), alpha = 0.7) +  # Set transparency for heatmap
  scale_fill_gradient(low = "white", high = "red") +  # Color scale for density
  geom_hline(yintercept = lat_seq, color = "white", linewidth = 0.5) +  # Add horizontal grid lines
  geom_vline(xintercept = long_seq, color = "white", linewidth = 0.5) +  # Add vertical grid lines
  coord_sf(xlim = c(min_longitude, max_longitude), ylim = c(min_latitude, max_latitude), expand = TRUE) +  # Focus on Santiago region
  theme_minimal() +  # Use a minimal theme
  labs(title = "Heat Map of Point Density in Santiago, Chile", 
       x = "Longitude", 
       y = "Latitude", 
       fill = "Density (points/m²)") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
