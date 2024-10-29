#Map number 1 is a map of places I've lived in the USA.

library(sf)
library(tigris)
library(tidycensus)
library(ggplot2)
library(dplyr)

# Get USA states boundaries
us <- get_decennial(
  geography = "state", 
  year = 2010,
  variables = "P001001",
  key = "42bf8a20a3df1def380f330cf7edad0dd5842ce6",
  geometry = TRUE,
  progress = FALSE
)

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

# Transform US boundaries and filter continental US
us <- st_transform(us, crs = 'EPSG:5070')
cont_us <- us %>% filter(NAME != 'Alaska' & NAME != 'Hawaii' & NAME != 'Puerto Rico')

# Highlight states lived
states_lived <- cont_us %>% filter(NAME %in% c("New York", "Virginia", "Idaho", "North Carolina", "Pennsylvania"))

# Map creation
q <- ggplot() +
  # Map of continental US with light grey fill
  geom_sf(data = cont_us, fill = "lightgrey", color = "white") +
  # Highlighted states 
  geom_sf(data = states_lived, fill = "darkslategray3", color = "white") +
  # Plot points with size proportional to years lived
  geom_sf(data = places_lived, aes(size = years), color = "maroon3") +
  scale_size_continuous(range = c(3, 8), guide = "none") + # Adjust size range as needed
  labs(title = "Oh, The Places I've Lived", 
       subtitle = "In the Continental US") +
  theme_void() +
  theme(plot.title = element_text(size = 20, face = "bold"), plot.subtitle = element_text(size = 15, face = "italic"))

# Adding annotations
q <- q +
  annotate(
    geom = "text",
    x = 1927000,
    y = 2265676,
    size = 4,
    label = "Newburgh, NY\n1982-2000",
    color = "black",
    fontface = "bold",
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 1830000,
    y = 1819000,
    size = 4,
    label = "Harrisonburgh, VA\n2000-2004",
    color = "black",
    fontface = "bold",
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 1100000,
    y = 2406000,
    size = 4,
    label = "Buffalo, NY\n2004-2006",
    color = "black",
    fontface = "bold",
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = -1527456,
    y = 2786000,
    size = 4,
    label = "Moscow, ID\n2006-2008",
    color = "black",
    fontface = "bold",
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 1800000,
    y = 1500000,
    size = 4,
    label = "Davidson, NC\n2008-2021",
    color = "black",
    fontface = "bold",
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 1858000,
    y = 2055900,
    size = 4,
    label = "Philadelphia, PA\n2022-Present",
    color = "black",
    fontface = "bold",
    hjust = 0
  )+
  
  # Dashed line from point to annotation
  geom_segment(
    aes(x = 1805675, y = 2265676,
        xend = 1805675+100000, yend = 2265676),
    linetype = "dashed",
    color = "grey28"
  )+
  geom_segment(
    aes(x = 1746657, y = 2080637,
        xend = 1746657+100000, yend = 2080637),
    linetype = "dashed",
    color = "grey28"
  )+
  geom_segment(
    aes(x = 1472995, y = 1846480,
        xend = 1830000, yend = 1855480),
    linetype = "dashed",
    color = "grey28"
  )+
  geom_segment(
    aes(x = 1355717, y = 1490928,
        xend = 1800000, yend = 1530928),
    linetype = "dashed",
    color = "grey28"
  )+
  geom_segment(
    aes(x = -1597455, y = 2815324,
        xend = -1527456, yend = 2815324),
    linetype = "dashed",
    color = "grey28"
  )+
  
  annotate(
    geom = "text",
    x = -2100000, # Adjust x and y as needed to fit in the bottom left corner
    y = 1000000,
    label = "#30DayMappingChallenge\nDay 1: Points\nAuthor: Elizabeth Delmelle\nMaster of Urban Spatial Analytics Program (MUSA) at Penn\n@Weitzman_musa",
    color = "grey2",
    hjust = 0,
    vjust = 1,
    size = 3
  )


# Print the updated map
q


#save output
ggsave("Outputs/01-Delmelle-Points.png", width = 15, height = 11)
