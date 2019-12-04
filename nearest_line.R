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

#~~~~~~~~~~~~~~Path~~~~~~~~~~~~~~~~~~~~~~~~~

### Replace V:\\Studies\\MOVED\\HealthImpact\\Data\\ with C:
dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2013~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rd_1 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2005 to 2014.csv"))  

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2014~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rd_1 <-read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2005 to 2014.csv")) 

rd_14a <- subset(rd_1,rd_1$`Local_Authority_(District)`==923)
#keep edin roads with speed limit 20 or 30
rd_14aa <- subset(rd_14a,rd_14a$Speed_limit==20)
aa <- subset(rd_14a,rd_14a$Speed_limit==30)
bb <- subset(rd_14a,rd_14a$Speed_limit==40)
rd_14a<- rbind(rd_14aa,aa)
rd_14a <- rbind(rd_14a,bb)

#Keep 2013 data from Edinburgh
rd_14a$Date <- trimws(rd_14a$Date)
rd_14a$Date <- as.Date(rd_14a$Date, "%m/%d/%Y")
rd <- rd_14a[rd_14a$Date >="2014-01-01" & rd_14a$Date <= "2014-12-31", ] %>% filter(!is.na(Date))


#Read second csv file with 2014 data
rd_b <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2014.csv"))
rd_b <- subset(rd_b,rd_b$`Local_Authority_(District)`==923)


#keep edin roads with speed limit 20 or 30
rd_2 <- subset(rd_b,rd_b$Speed_limit==20)
aa <- subset(rd_b,rd_b$Speed_limit==30)
bb <- subset(rd_b,rd_b$Speed_limit==40)
rd_14a<- rbind(rd_2,aa)
rd_f <- rbind(rd_14a,bb)

rd <- rbind(rd,rd_f)

#clean NA
rd <- na.omit(rd[1:31])
#rd <- tibble::rowid_to_column(rd, "ID")
#Keep a copy in data frame
rd1 <- rd

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2015-2018~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rd_18 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2015.csv"))

#Keep Edinburgh data
rd_13a <- subset(rd_18,rd_18$`Local_Authority_(District)`==923)
#keep edin roads with speed limit 20/30/40
rd_13aa <- subset(rd_13a,rd_13a$Speed_limit==20)
aa <- subset(rd_13a,rd_13a$Speed_limit==30)
bb <- subset(rd_13a,rd_13a$Speed_limit==40)
rd_13a<- rbind(rd_13aa,aa)
rd_13a <- rbind(rd_13a,bb)
rd <- rd_13a
rd <- na.omit(rd[1:31]) #clean NA
rd1 <- rd #Keep a copy in data frame

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Edinburgh visualizations
## Read geodatabase
gdb_path <- paste0(dir_path, "20mph study collisions\\20mph.gdb")
gdb_layers <- ogrListLayers(gdb_path)
edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Nearest road~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Planar(cartesian) projection 
crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat

#Convert to planar 
edin_cons_streets <- spTransform(edin_cons_streets,crs)
rd_final <-  spTransform(SpatialPointsDataFrame(coords = rd1[4:5],proj4string = wgs84,data = rd1), crs)

#Applying the projection
proj4string(rd_final) <- proj4string(edin_cons_streets)

#Find nearest line maxDist ?
new_point_sp <- snapPointsToLines(rd_final,edin_cons_streets,maxDist=20)

#keep data in df
edin_cons_streets@data <-rowid_to_column(edin_cons_streets@data, "ID")
df_road1 <- edin_cons_streets@data
df_nearest1 <- data.frame(new_point_sp)

#Merge
total <- merge(df_nearest1,edin_cons_streets@data,by.x = "nearest_line_id" ,by.y = "ID")
sunset <- data.frame(total[1],total[2],total[19],total[38])

#Count 
count1 <- aggregate(Accident_Index~LAYER,sunset,length)%>% view()
count2 <- aggregate(Accident_Index~Speed_limit,sunset,length) %>%view()

#Transform back to lat/long to plot with leaflet
edin_cons_streets <- spTransform(edin_cons_streets,"+init=epsg:4326")
rd_final <- spTransform(rd_final, "+init=epsg:4326")
new_point_sp <- spTransform(new_point_sp, "+init=epsg:4326")

#keep a copy in df (check if different projections change count results)
df_road <- fortify(edin_cons_streets)
df_nearest <- data.frame(new_point_sp)

#Merge df
total1 <- merge(df_nearest,edin_cons_streets@data,by.x = "nearest_line_id" ,by.y = "ID" ,all.x = T)
sunset1 <- data.frame(total1[1],total1[2],total1[19],total1[38]) %>% view
#count 
count11 <- aggregate(Accident_Index~LAYER,sunset1,length)%>% view()
count22 <- aggregate(Accident_Index~Speed_limit,sunset1,length) %>%view()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Buffer~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#add coordinates to make it spatial data
coordinates(rd) <- ~Longitude + Latitude

#add a buffer around the point
points_polygons <- gBuffer(rd, width=0.0005, byid = TRUE )

#same projections
#proj4string(points_polygons) <- proj4string(edin_cons_streets)
#proj4string(rd) <- proj4string(edin_cons_streets)

# keeps intersect lines in df
#inter <- intersect(edin_cons_streets,points_polygons)

#Visualize with leaflet
#map0 <- leaflet() %>% addTiles() %>% addPolygons(data = edin_cons_streets)
#map_near0 <- map0 %>% addMarkers(lng =df_nearest$X, lat = df_nearest$Y, popup =paste("Accident index: ", df_nearest$Accident_Index, "<br>",
#                                                                                "Speed limit: ", df_nearest$Speed_limit))
#map30 <- map_near0 %>% addCircles(lng =rd1$Longitude , lat = rd1$Latitude, popup = rd1$Accident_Index,radius = 5, color = 'black')
#map30

#Visualize with leaflet (nearest+buffer)
#map2 <- leaflet() %>% addTiles() %>% addPolygons(data = inter, color = 'Black')
#map_near2 <- map2 %>% addMarkers(lng =df_nearest$X, lat = df_nearest$Y, popup = df_nearest$Accident_Index)
#map3a <- map_near2 %>% addCircles(lng =rd1$Longitude , lat = rd1$Latitude, popup = rd1$Accident_Index,radius = 2, color = '#ff0000')
#map3a

#Maps with all the information
map <- leaflet() %>% addTiles() %>% addPolygons(data = edin_cons_streets)
map_near <-map %>% leaflet() %>% addTiles() %>% addMarkers(lng =total1$X, lat = total1$Y,
                                                    popup =paste("Accident index: ", total1$Accident_Index, "<br>",
                                                                 "Speed limit: ", total1$Speed_limit ,"<br>",
                                                                 "Layer: ",total1$LAYER,"<br>",
                                                                 "1st road class: ",total1$X1st_Road_Class, "<br>",
                                                                 "2nd road class: ", total1$X2nd_Road_Class,"<br>",
                                                                 "Road Type: ",  total1$Road_Type))

map3 <- map_near %>% addCircleMarkers(lng =rd_final@data$Longitude , lat = rd_final@data$Latitude,
                                      popup =paste("Accident index: ", rd_final@data$Accident_Index, "<br>",
                                                   "Speed limit: ", rd_final@data$Speed_limit ,"<br>",
                                                   "1st road class: ",rd_final@data$`1st_Road_Class`, "<br>",
                                                   "2nd road class: ", rd_final@data$`2nd_Road_Class`,"<br>",
                                                   "Road Type: ", rd_final@data$Road_Type ))

#Visualize with leaflet
road_existing_20 <-edin_cons_streets%>% filter(edin_cons_streets$LAYER == "20mph existing streets")
road_local_20 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "20mph local streets")
road_main_20 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "20mph main streets")
road_30 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "30mph")
road_40 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "40mph")
road_part <- filter(edin_cons_streets, edin_cons_streets$LAYER == "Part time 20mph")
road_50 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "50, 60 or 70mph")
road_trunk <- filter(edin_cons_streets, edin_cons_streets$LAYER == "Trunk roads")

map3 %>% 
  addPolylines(data = road_existing_20, color= "green")%>%
  addPolylines(data = road_local_20,color= "red")%>%
  addPolylines(data = road_main_20,color= "yellow")%>%
  addPolylines(data = road_30,color= "cyan")%>%
  addPolylines(data = road_40,color= "blue")%>%
  addPolylines(data = road_part,color= "black")%>%
  addPolylines(data = road_50,color= "purple")%>%
  addPolylines(data = road_trunk,color= "orange")
  
#~~~~~~~~~~~~~~~~~~~ Edin_impl_zones ~~~~~~~~~~~~~~~~~~~~

coordinates(rd1) <- ~ Longitude + Latitude
points_polygons <- gBuffer(rd1,width = .000005,byid=TRUE)
proj4string(points_polygons) <- proj4string(edin_impl_zones)
count_zones <- over(points_polygons,edin_impl_zones,returnList = FALSE)
count_zones <- rowid_to_column(count_zones,"ID")
rd <- rowid_to_column(rd,"index_ID")
total_zones <- merge(rd,count_zones,by.x="index_ID",by.y="ID")
df_zones <-data.frame(total_zones[2],total_zones[5],total_zones[6],total_zones[35])

edin_impl_zones <- st_as_sf(edin_impl_zones)
#Leaflet
zone_1 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 1)
zone_2 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 2)
zone_3 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 3)
zone_4 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 4)
zone_5 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 5)
zone_6 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 6)

map_zones <- leaflet() %>% addTiles() %>% 
  addPolygons(data = zone_1, color= "green")%>%
  addPolygons(data = zone_2,color= "red")%>%
  addPolygons(data = zone_3,color= "purple")%>%
  addPolygons(data = zone_4,color= "cyan")%>%
  addPolygons(data = zone_5,color= "blue")%>%
  addPolygons(data = zone_6,color= "orange")

map_zones %>% addMarkers(lng =df_zones$Longitude, lat = df_zones$Latitude, 
                         popup =paste("Accident index: ", df_zones$Accident_Index, "<br>",
                                      "Zone: ", df_zones$ImplementationZone))