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

class(edin_cons_streets)
class(edin_impl_zones)
#data frames of road and zone maps
list_roads <- data.frame(edin_cons_streets)
list_zones <- data.frame(edin_impl_zones)
summary(list_roads)
summary(list_zones)


#data frames with (long,lat) of road and zones
df_road <- fortify(edin_cons_streets)
df_zones <- fortify(edin_impl_zones)
summary(df_road)
summary(df_zones)

library(ggplot2)
#ggplot the (long,lat) of roads
ggplot(data = df_road, aes(x = long, y = lat, group=group)) + geom_path() 

#ggplot the (long,lat) of zones
ggplot(data = df_zones, aes(x = long, y = lat, group=group)) + geom_path() 

head(edin_cons_streets@data)
head(edin_impl_zones@data)
head(edin_cons_streets@lines)
head(edin_impl_zones@polygons)

#compare (long,lat) bettween data and Edinburgh roads
apply(df_road[1:2],1, "==",rd[4:5] )

print(df_road[1:2])
print(rd[4:5])
gc()

#Maps the attributes of shapefiles
library(sp)
summary(edin_cons_streets)
spplot(edin_cons_streets,'Shape_Length')
spplot(edin_cons_streets,'LAYER')
spplot(edin_cons_streets,"FEATID")

summary(edin_impl_zones)
spplot(edin_impl_zones,'Shape_Area')
spplot(edin_impl_zones,'Shape_Length')
spplot(edin_impl_zones,"ImplementationZone")

glimpse(edin_cons_streets)
library(shapefiles)
conver <- convert.to.simple(edin_cons_streets)
