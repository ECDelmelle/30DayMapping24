#The plan is to make cute circle facted maps for each city I've lived in comparing some census variable
library(sf)
library(tigris)
library(tidycensus)
library(ggplot2)
library(dplyr)
library(patchwork)
library(RColorBrewer)
library(grid)

options(scipen = 999)

# Define coordinates and years lived for each location
places_data <- data.frame(
  name = c("Newburgh, NY", "Harrisonburgh, VA", "Buffalo, NY", "Moscow, ID", "Davidson, NC", "Philadelphia, PA"),
  years = c(18, 4, 2, 2, 13, 2), # Number of years lived at each location
  x = c(-74.0087, -78.8686, -78.8146, -116.9927, -80.8606, -75.1893),
  y = c(41.5076, 38.4508, 42.9845, 46.7333, 35.4996, 40.0147)
)

# Create an sf object with the point geometries and transform to EPSG:5070
places_lived <- st_as_sf(places_data, coords = c("x", "y"), crs = "EPSG:4326") %>%
  st_transform(crs = 'EPSG:5070')

tract_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = c("Idaho", "Pennsylvania", "New York", "North Carolina", "Virginia", "Washington", "New Jersey"),
  year = 2020,
  key = "42bf8a20a3df1def380f330cf7edad0dd5842ce6",
  geometry = TRUE
)%>%
  st_transform(crs = 'EPSG:5070')%>%
  mutate(estimate = as.numeric(estimate)) %>% erase_water(area_threshold =0.75)

# Coordinates of the cities as per the 'places_lived' file (in EPSG:5070 - NAD83 / Conus Albers)
city_coords <- data.frame(
  name = c("Newburgh, NY", "Harrisonburg, VA", "Buffalo, NY", "Moscow, ID", "Davidson, NC", "Philadelphia, PA"),
  x = c(1805675, 1472995, 1386291, -1597455, 1355717, 1746657),
  y = c(2265676, 1846480, 2346659, 2815324, 1490928, 2080637)
)

# Download highways data using `tigris`
states <- c("ID", "VA", "NY", "NC", "PA")  # State abbreviations for each city
highways <- do.call(rbind, lapply(states, function(state) {
  primary_secondary_roads(state = state) %>%
    st_transform(crs = st_crs(5070))
}))

# Optionally, filter for interstates only
highways <- highways %>%
  filter(RTTYP == "I")  # `RTTYP == "I"` selects interstates


# Define buffer radius
buffer_radius <- 20000  # 20,000 meters

# Prepare standardized income breaks and colors
all_estimates <- na.omit(tract_income$estimate)  # Remove any NA values
breaks <- scales::pretty_breaks(n = 5)(range(all_estimates))

# Generate labels for each interval with dollar signs
labels <- paste0("$", scales::label_comma()(breaks[-length(breaks)]), " - $", scales::label_comma()(breaks[-1]))
colors <- c("#e0f3db", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c", "#005a32") 

#colors <- brewer.pal(6, "YlOrRd") 

# Convert `estimate` to a factor with fixed levels across all maps
tract_income <- tract_income %>%
  mutate(
    income_group = cut(estimate, breaks = breaks, include.lowest = TRUE, labels = labels)
  )

# Create a list to hold plots
city_maps <- lapply(1:nrow(city_coords), function(i) {
  city_name <- city_coords$name[i]
  center_point <- st_sfc(st_point(c(city_coords$x[i], city_coords$y[i])), crs = st_crs(tract_income))
  city_circle <- st_buffer(center_point, dist = buffer_radius)
  
  # Crop to the circular area and add city name
  city_area <- st_intersection(tract_income, city_circle) %>%
    mutate(City = city_name)
  city_highways <- st_intersection(highways, city_circle)
  # Plot each city
  ggplot(city_area) +
    geom_sf(data = city_circle, fill = "blue", color = "blue", alpha = 0.7) +
    geom_sf(aes(fill = income_group), color = "white", size = 0.1, show.legend = i == 1) + 
    geom_sf(data = city_highways, color = "red", size = 0.2) +  # Only show legend on first plot
    scale_fill_manual(
      values = colors, 
      name = "Median Income (2020)", 
      drop = FALSE,  # Keeps all levels in the legend
      labels = c(labels, "NA"),
      guide = guide_legend(nrow = 1, byrow = TRUE)  # Spread legend across bottom
    ) +
    labs(title = city_name) +
    theme_void() +
    theme(legend.position = if (i == 1) "bottom" else "none")  # Position legend only on the first plot
})


# Combine plots in a 3x2 grid with a single, centered legend at the bottom of the layout
final_plot <- (wrap_plots(city_maps, ncol = 3) + plot_layout(guides = "collect")) & theme(legend.position = "bottom")

final_plot +
  annotation_custom(
    grob = textGrob(
      "Day 3: Polygons\nMap by Elizabeth Delmelle",
      x = unit(1, "npc"), y = unit(0, "npc"),  # Adjust x and y for fine-tuning
      hjust = 0, vjust = 0, rot = 90, gp = gpar(col = "black", fontsize = 6, fontfamily = "sans")
    )
  )

