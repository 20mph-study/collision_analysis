# Load libraries
library(rgdal)
library(leaflet)
library(tidyverse)
library(rgeos)
library(raster)
library(ggplot2)
library(sf)
library(mapview)
library(maptools)
library(sp)

dir_path <- "C:\\Users\\Kyriaki Kokka\\Desktop\\"
data <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2015.csv"))
data <- data %>% filter(data$Speed_limit %in% c(20,30,40)) 
data <- data %>% filter(data$`Local_Authority_(District)` == 923)
data <- na.omit(data[1:31])#remove NA obs

gdb_path <- paste0(dir_path, "20mph study collisions\\20mph.gdb")
gdb_layers <- ogrListLayers(gdb_path)
edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")

#add coordinates to make it spatial data
#coordinates(data) <- ~Longitude + Latitude

#Create planar(cartesian) projection 
crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat

#Convert data to planar projection  
edin_impl_zones <- spTransform(edin_impl_zones,crs)
utm_data <-  spTransform(SpatialPointsDataFrame(coords = data[4:5],proj4string = wgs84,data = data), crs)

proj4string(utm_data) <- proj4string(edin_impl_zones)

#add a buffer around the point
points_polygons <- gBuffer(utm_data, width=5, byid = TRUE )
count_zones <- over(points_polygons,edin_impl_zones,returnList = FALSE)
count_zones <- rowid_to_column(count_zones,"ID")
data <- rowid_to_column(data,"index_ID")
total_zones <- merge(data,count_zones,by.x="index_ID",by.y="ID")
df_zones <-data.frame(total_zones[2],total_zones[5],total_zones[6],total_zones[35])

#Convert data projections back to lat/long to plot with leaflet
edin_impl_zones <- spTransform(edin_impl_zones,"+init=epsg:4326")
#total_zones <- spTransform(total_zones, "+init=epsg:4326")
utm_data <- spTransform(utm_data, "+init=epsg:4326")


df_na <- na.omit(df_zones[1:4]) # check how many are missing

edin_impl_zones <- st_as_sf(edin_impl_zones)
#Leaflet maps of the zones & accidents
zone_1 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 1)
zone_2 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 2)
zone_3 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 3)
zone_4 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 4)
zone_5 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 5)
zone_6 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 6)

#Visualize
map_zones <- leaflet() %>% addTiles() %>% 
  addPolygons(data = zone_1, color= "green")%>%
  addPolygons(data = zone_2,color= "red")%>%
  addPolygons(data = zone_3,color= "purple")%>%
  addPolygons(data = zone_4,color= "cyan")%>%
  addPolygons(data = zone_5,color= "blue")%>%
  addPolygons(data = zone_6,color= "orange")
map_zones

map_zones %>% addMarkers(lng =df_zones$Longitude, lat = df_zones$Latitude, 
                         popup =paste("Accident index: ", df_zones$Accident_Index, "<br>",
                                      "Zone: ", df_zones$ImplementationZone))
