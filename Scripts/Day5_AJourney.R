# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(showtext)

# Enable showtext for custom fonts and load a fun font (optional)
showtext_auto()
font_add_google("Pacifico", "pacifico")

# Load country boundaries for the world map in an Orthographic projection
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = "+proj=ortho +lon_0=-45 +lat_0=50")

# Define the places data
places_data <- data.frame(
  name = c("Newburgh, NY", "Harrisonburg, VA", "Buffalo, NY", "Moscow, ID", "Davidson, NC", "The Hague, NL", "Philadelphia, PA"),
  x = c(-74.0087, -78.8686, -78.8146, -116.9927, -80.8606, 4.2982, -75.1893),
  y = c(41.5076, 38.4508, 42.9845, 46.7333, 35.4996, 52.0832, 40.0147)
)

# Reproject places_data to the Orthographic projection
places_data_sf <- st_as_sf(places_data, coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = "+proj=ortho +lon_0=-45 +lat_0=50")

# Convert coordinates back to data frame
places_data <- as.data.frame(st_coordinates(places_data_sf)) %>%
  bind_cols(places_data)

# Create flow data with calculated midpoints and rotation angles for labels
flow_data <- data.frame(
  id_a = head(places_data$name, -1),
  id_b = tail(places_data$name, -1),
  xa = head(places_data$X, -1),
  ya = head(places_data$Y, -1),
  xb = tail(places_data$X, -1),
  yb = tail(places_data$Y, -1),
  flow_ab = 1
)

# Calculate midpoint coordinates and rotation angles for labels
flow_data <- flow_data %>%
  mutate(
    mid_x = (xa + xb) / 2,
    mid_y = (ya + yb) / 2,
    angle = atan2(yb - ya, xb - xa) * 180 / pi  # Convert angle to degrees
  )


p <- ggplot() +
  # Add a large blue circle to simulate a circular earth boundary
  geom_point(aes(x = 0, y = 0), color = "lightblue", size = 180, shape = 21, fill = "lightblue") +
  geom_sf(data = world, fill = "grey80", color = "white") +  # Light grey for land
  coord_sf(expand = FALSE) +  # No extra space around the map
  theme_void() +  # Remove x and y axis, labels, and grid
  theme(panel.background = element_rect(fill = "transparent", color = NA), legend.position = "none")  # Transparent panel background
# Add curved lines for flows
p <- p + 
  geom_curve(data = flow_data,
             aes(x = xa, y = ya, xend = xb, yend = yb),
             color = "deeppink3", size = 0.7, curvature = 0.2,
             alpha = 0.8, arrow = arrow(type = "closed", length = unit(0.1, "inches")))

# Add small points for each city location
p <- p + 
  geom_point(data = places_data, aes(x = X, y = Y), color = "black", size = 1.5)



# Add title and caption
p <- p + 
  labs(
    title = "My Life's Journey So Far",
    caption = "#30DayMappingChallenge, Day 5: A Journey, By Elizabeth Delmelle"
  ) +
  theme(
    plot.title = element_text(family = "pacifico", size = 18, face = "bold", hjust = 0.5, color = "deeppink3"),
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 10))
  )

# Display the map
print(p)


