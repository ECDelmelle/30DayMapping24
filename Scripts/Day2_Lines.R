library(FITfileR)
library(dplyr)
library(sf)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(ggspatial)
library(osmdata)

#I followed this tutorial for importing the FIT data: https://msmith.de/FITfileR/articles/FITfileR.html

# Load FIT data
gps <- readFitFile("Data/Pre_game_Rocky_run_LGM.fit")
allrecords <- records(gps) %>%
  bind_rows() %>%
  arrange(timestamp)

# Extract coordinates and speed
coords <- allrecords %>%
  select(position_long, position_lat, heart_rate) %>%
  filter(!is.na(position_long) & !is.na(position_lat))

# Convert to `sf` format with coordinates and CRS
coords_sf <- st_as_sf(coords, coords = c("position_long", "position_lat"), crs = 4326, remove = FALSE)


# Download OSM data within the bounding box
osm_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Plot with ggplot2
ggplot() +
  geom_sf(data = osm_data$osm_lines, color = "gray80", size = 0.3) +  # Highways in light gray
  geom_sf(data = coords_sf, aes(color = heart_rate), size = 1) +
  scale_color_viridis_c(option = "inferno", name = "Heart Rate", direction = -1) +
  annotation_scale(
    location = "tl",
    unit_category = "imperial",  # Scale in miles
    pad_x = unit(0, "cm"), pad_y = unit(0.2, "cm")  # Adjusts position relative to bottom-left
  )  + 
  theme_void() +
  labs(title = "Run Along the Schyulkill River Trail, Philadelphia", 
       subtitle = "GPS Track with Heart Rate",
       caption = "#30DayMappingChallenge Day 2: Lines, Author: Elizabeth Delmelle")+
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 10, hjust = 0) 
  )

ggsave("Outputs/02-Delmelle-Lines.png", width = 12, height = 11)