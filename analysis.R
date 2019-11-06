# Load libraries
library(rgdal)
library(leaflet)

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

edin_impl_zones <- readOGR(dsn=fp,layer="ImplementationZones")
# Transform the data by applying a projection
edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")

# Visualize using leaflet
leaflet(edin_impl_zones) %>% addTiles() %>% addPolygons()

edin_cons_streets <- readOGR(dsn=fp,layer="Consultation20mphStreets")
# Transform the data by applying a projection
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")

# Visualize using leaflet
leaflet(edin_cons_streets) %>% addTiles() %>% addPolygons()