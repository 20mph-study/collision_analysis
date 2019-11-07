# Load libraries
library(rgdal)
library(leaflet)
library(tidyverse)

# Load data from the network drive for Belfast road network
belfast_road_data <- readOGR('V:/Studies/MOVED/HealthImpact/data/20mph study collisions/20mph Speed Limit Streets/20mph_Speed_Limit_Streets.shp')

# Tranform the data by applying a projection
belfast_road_data <- spTransform(belfast_road_data, "+init=epsg:4326")

# Visualize using leaflet
leaflet(belfast_road_data) %>% addTiles() %>% addPolygons()

#### Edinburgh visualizations
## Read geodatabase
gdb_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\20mph.gdb"

gdb_layers <- ogrListLayers(gdb_path)
print(gdb_layers)

edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
# Transform the data by applying a projection
edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")

# Visualize using leaflet
leaflet(edin_impl_zones) %>% addTiles() %>% addPolygons()

edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")
# Transform the data by applying a projection
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")

# Visualize using leaflet
leaflet(edin_cons_streets) %>% addTiles() %>% addPolygons()

### Visualize collisions data - 2015 data
rd <- read_csv("V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\collisions\\collisions 2015.csv")

m <- leaflet(rd) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
# Setview to city of Edinburgh
m1<- m %>% setView(-3.188267, 55.953251, zoom = 12)
# Add circles
m1 %>% addCircles(~Longitude, ~Latitude, weight = 3, radius=40, color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 


