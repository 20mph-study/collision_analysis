# Load libraries
library(rgdal)
library(leaflet)
library(tidyverse)
library(rgeos)


### Visualize collisions data - 2015 data
source_rd <- read_csv("V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\collisions\\collisions 2015.csv")

rd <- source_rd %>% filter(`Local_Authority_(District)` == 923)

rd <- rd[1:100,]

#add coordinates to make it spatial data
coordinates(rd) <- ~Longitude + Latitude

#### Edinburgh visualizations
## Read geodatabase
gdb_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\20mph.gdb"

gdb_layers <- ogrListLayers(gdb_path)

edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
# Transform the data by applying a projection
edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")

# Visualize using leaflet
# leaflet(edin_impl_zones) %>% addTiles() %>% addPolygons()

edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")
# Transform the data by applying a projection
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")

#add a buffer around the point
points_polygons <- gBuffer(rd, width=.0005, byid = TRUE)

#Identify subset of roads where these points (with buffer) exist
intersect_lines <- gIntersection(edin_cons_streets, points_polygons, byid = T)

# Visualize using leaflet
leaflet() %>% addTiles() %>% addPolygons(data = points_polygons) %>% addPolygons(data = intersect_lines, color = 'Black')

# plot(gIntersects(edin_cons_streets, gBuffer(rd, width=.0005, byid = TRUE), byid = T))


