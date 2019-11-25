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

### Replace V:\\Studies\\MOVED\\HealthImpact\\Data\\ with C:
dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"
rd_1 <-read_csv("V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\collisions\\collisions 2005 to 2014.csv")  

#Keep Edinburgh data
rd_13a <- subset(rd_1,rd_1$`Local_Authority_(District)`==923)
#keep edin roads with speed limit 20 or 30
rd_13aa <- subset(rd_13a,rd_13a$Speed_limit==20)
aa <- subset(rd_13a,rd_13a$Speed_limit==30)
bb <- subset(rd_13a,rd_13a$Speed_limit==40)
rd_13a<- rbind(rd_13aa,aa)
rd_13a <- rbind(rd_13a,bb)

#Keep 2013 data from Edinburgh
rd_13a$Date <- trimws(rd_13a$Date)
rd_13a$Date <- as.Date(rd_13a$Date, "%m/%d/%Y")
rd <- rd_13a[rd_13a$Date >="2013-01-01" & rd_13a$Date <= "2013-12-31", ] %>% filter(!is.na(Date))

#clean NA
rd <- na.omit(rd[1:31])
rd <- tibble::rowid_to_column(rd, "ID")
#Keep a copy in data frame
rd1 <- rd

#add coordinates to make it spatial data
coordinates(rd) <- ~Longitude + Latitude

#### Edinburgh visualizations
## Read geodatabase
gdb_path <- paste0(dir_path, "20mph study collisions\\20mph.gdb")
gdb_layers <- ogrListLayers(gdb_path)
edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")

# Transform the data by applying a projection
edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")

# Visualize using leaflet
leaflet(edin_impl_zones) %>% addTiles() %>% addPolygons()

# Transform the data by applying a projection
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")

#same projections
proj4string(points_polygons) <- proj4string(edin_cons_streets)
proj4string(rd) <- proj4string(edin_cons_streets)

#add a buffer around the point
points_polygons <- gBuffer(rd, width=0.00032, byid = TRUE )

# keeps intersect lines in df
inter <- intersect(edin_cons_streets,points_polygons)

#keep nearest line with a projected point
new_point_sp <- snapPointsToLines(rd, edin_cons_streets, maxDist = 0.1)

proj4string(new_point_sp) <- proj4string(edin_cons_streets)
df_nearest <- data.frame(new_point_sp)

#Visualize with leaflet
map <- leaflet() %>% addTiles() %>% addPolygons(data = edin_cons_streets)
map_near <- map %>% addMarkers(lng =df_nearest$X, lat = df_nearest$Y, popup =paste("Accident index: ", df_nearest$Accident_Index, "<br>",
                                                                                   "Speed limit: ", df_nearest$Speed_limit))
map3 <- map_near %>% addCircles(lng =rd1$Longitude , lat = rd1$Latitude, popup = rd1$Accident_Index,radius = 5, color = 'black')
map3

#Visualize with leaflet (nearest+buffer)
map <- leaflet() %>% addTiles() %>% addPolygons(data = inter, color = 'Black')
map_near <- map %>% addMarkers(lng =df_nearest$X, lat = df_nearest$Y, popup = df_nearest$Accident_Index)
map3a <- map_near %>% addCircles(lng =rd1$Longitude , lat = rd1$Latitude, popup = rd1$Accident_Index,radius = 2, color = '#ff0000')
map3a

#Maps with every speed limit
#Visualize with leaflet
road_existing_20 <-edin_cons_streets%>% filter(edin_cons_streets$LAYER == "20mph existing streets")
road_local_20 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "20mph local streets")
road_main_20 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "20mph main streets")
road_30 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "30mph")
road_40 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "40mph ")
road_part_time_20 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "Part time 20mph ")

map3a %>% 
  addPolygons(data = road_existing_20, color= "green")%>%
  addPolygons(data = road_local_20,color= "red" )%>%
  addPolygons(data = road_main_20,color= "yellow" )%>%
  addPolygons(data = road_30,color= "blue" )%>%
  addPolygons(data = road_40,color= "black" )%>%
  addPolygons(data = road_part_time_20,color= "brown" )
  
