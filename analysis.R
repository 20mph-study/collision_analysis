# Load libraries
library(rgdal)
library(leaflet)

# Load data from the network drive for Belfast road network
belfast_road_data <- readOGR('V:/Studies/MOVED/HealthImpact/data/20mph study collisions/20mph Speed Limit Streets/20mph_Speed_Limit_Streets.shp')

# Tranform the data by applying a projection
belfast_road_data <- spTransform(belfast_road_data, "+init=epsg:4326")

# Visualize using leaflet
leaflet(belfast_road_data) %>% addTiles() %>% addPolygons()