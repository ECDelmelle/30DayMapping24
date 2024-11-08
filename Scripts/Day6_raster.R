library(rayshader)
library(elevatr)
library(magrittr)
library(tidycensus)
library(sf)
library(tigris)
library(raster)
library(osmdata)

##There is a lot of trial and error in this code!! I wanted to overlay the roads onto the 3D viewer...but did not succeed in a reasonable amount of time!

# Get Orange County boundary from the Census
orange_county <- get_decennial(
  geography = "county",
  variables = "P1_001N",  # Population variable for demonstration
  state = "NY",
  county = "Orange",
  key = "42bf8a20a3df1def380f330cf7edad0dd5842ce6",
  geometry = TRUE
)

orange_county%>% st_transform(orange_county, crs = 4269)

# Get roads data from TIGER/Line shapefiles
roads <- roads(state = "NY", county = "Orange", class = "sf")

# Filter for primary highways only (you can adjust the filter as needed)
highways_1 <- roads %>% dplyr::filter(RTTYP == "I")


#%>%st_transform(EPSG:4269)  # S1100 is the MTFCC code for primary roads

# Define bounding box for Orange County, NY
bbox <- st_bbox(c(xmin = -74.7, xmax = -73.94, ymin = 41.14, ymax = 41.63), crs = 4269)

# Query OSM for highways in this bounding box
highways_osm <- opq(bbox = bbox) %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

highways <- st_transform(highways_osm$osm_lines, crs = 4269)

# Retrieve elevation data with 30-meter resolution
dem_data <- get_elev_raster(orange_county, z = 10)

# Convert DEM raster to matrix for use in rayshader
elmat <- raster_to_matrix(dem_data)

# Generate a shaded texture and plot the 3D map
shaded_texture <- sphere_shade(elmat, texture = "imhof1")


# define label as sf feature
# Step 1: Create an sf object for "West Point" in WGS84
west_point_wgs84 <- st_sfc(st_point(c(-73.95575536230268, 41.39684891057594)), crs = 4326)
west_point_sf <- st_sf(name = "West Point", geometry = west_point_wgs84)

wp = c(41.33684891057594, -73.90575536230268) 

# Step 2: Reproject the point to NAD83
west_point_nad83 <- st_transform(west_point_sf, crs = 4269)

# Check the output
print(west_point_nad83)

##This is the map on the 2D viewer with the roads
basemap <- elmat %>%
  sphere_shade(texture = "imhof1") %>%
  add_water(detect_water(elmat), color = "imhof1") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>% 
  add_overlay(sphere_shade(elmat, texture = "imhof1", 
                           zscale=4, colorintensity = 5), alphalayer=0.5)%>% 
  add_shadow(texture_shade(elmat,detail=8/10,contrast=9,brightness = 11), 0.1)

basemap <- basemap%>%
  add_overlay(generate_line_overlay(highways_1,orange_county, linewidth = 4, color="white", heightmap = elmat))

basemap <- basemap%>% add_overlay(generate_label_overlay(west_point_sf, extent = orange_county,
                                   text_size = 2, point_size = 1, 
                                   halo_color = "white",halo_expand = 5, 
                                   seed=1,
                                   heightmap = elmat, data_label_column = "name"))
                                
##this is where working in the 3D starts
#coordinates of west point
wp = c(41.33478853742079, -74.058858972455) 

map3d <- elmat %>% 
  sphere_shade(zscale =5, texture = "imhof1") %>% 
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 5, fov = 0, theta = -30, phi = 30, windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, waterlinecolor = "white", waterlinealpha = 0.5,
          wateralpha = 0.5, watercolor = "lightblue")
render_label(elmat, lat = wp[1], long = wp[2], extent = orange_county, altitude = 13000, zscale = 50,
             text = "West Point", textsize = 2, linewidth = 5)
render_snapshot(title_text = "Elevation: Orange County, New York",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "W", y=50,
                scale_length = c(0.33,1))
render_compass(position = "E")


