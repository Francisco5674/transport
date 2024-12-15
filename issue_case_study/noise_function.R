# Load necessary library
library(ggplot2)

# Define parameters
S1 <- 110  # Updated S1 value

# Create a data frame with a range of x values
x_values <- seq(1, 1000, by = 0.1)  # Distance values from 1 to 100 with small increments
S_values <- pmax(S1 - 6 * log2(x_values), 0)  # Compute S values

# Create a data frame for plotting
data <- data.frame(x = x_values, S = S_values)

# Specific points of interest for S = 100, 70, and 50
points_of_interest <- data.frame(
  x = c(3.17, 101.59, 322.54),  # Distances that correspond to S = 100, 70, and 50 with S1 = 110
  S = c(100, 70, 60)
)

# Plot using ggplot2
ggplot(data, aes(x = x, y = S)) +
  geom_line(color = "blue") +
  geom_point(data = points_of_interest, aes(x = x, y = S), color = "red", size = 3) +  # Markers
  geom_text(data = points_of_interest, aes(x = x, y = S, label = paste0("(", round(x, 2), ", ", S, ")")), 
            vjust = -1, color = "red", size = 4) +  # Labels for the points
  labs(title = "Sound Level S as a Function of Distance x (S1 = 110)",
       x = "Distance (x)",
       y = "Sound Level (S)") +
  theme_minimal()
