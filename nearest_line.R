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
library(readxl)
library(readr)

#Functions for the analysis
#Function for the nearest line
nearest_line <-function(df,road_net){
  #Create planar(cartesian) projection 
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  #Convert data to planar projection  
  road_net <- spTransform(road_net,crs)
  df <-  spTransform(SpatialPointsDataFrame(coords = df[4:5],proj4string = wgs84,data = df), crs)
  
  #Make sure our data have same projections
  proj4string(df) <- proj4string(road_net)
  
  #Find nearest line with maxDist=20m 
  nearest_line_sp <- snapPointsToLines(df,road_net,maxDist=20)
  
  return(nearest_line_sp)
}

#Function for leaflet visualisations
leaflet_map <- function(final_df,road_df,edin_streets){
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  #Convert data to planar projection  
  edin_streets <- spTransform(edin_streets,crs)
  coordinates(road_df) <- ~ Longitude + Latitude
  coordinates(final_df) <- ~ X + Y
  
  #Convert data projections back to lat/long to plot with leaflet
  edin_streets <- spTransform(edin_streets,"+init=epsg:4326")

  #Same projections
  proj4string(final_df) <- proj4string(edin_streets)
  proj4string(road_df) <- proj4string(edin_streets)
  
  edin_streets <- st_as_sf(edin_streets)
  #Leaflet maps of the road network , accidents and their projections
  road_existing_20 <-edin_streets%>% filter(edin_streets$LAYER == "20mph existing streets")
  road_local_20 <- filter(edin_streets, edin_streets$LAYER == "20mph local streets")
  road_main_20 <- filter(edin_streets, edin_streets$LAYER == "20mph main streets")
  road_30 <- filter(edin_streets, edin_streets$LAYER == "30mph")
  road_40 <- filter(edin_streets, edin_streets$LAYER == "40mph")
  road_part <- filter(edin_streets, edin_streets$LAYER == "Part time 20mph")
  road_50 <- filter(edin_streets, edin_streets$LAYER == "50, 60 or 70mph")
  road_trunk <- filter(edin_streets, edin_streets$LAYER == "Trunk roads")
  
  
  #Visualize
  map <- leaflet() %>% addTiles() %>% addPolygons(data = edin_streets)
  map_near <-map %>% leaflet() %>% addTiles() %>% addMarkers(lng =final_df$X, lat = final_df$Y,
                                                             popup =paste("Accident index: ", final_df$Accident_Index, "<br>",
                                                                          "Speed limit: ", final_df$Speed_limit ,"<br>",
                                                                          "Layer: ",final_df$LAYER,"<br>",
                                                                          "1st road class: ",final_df$X1st_Road_Class, "<br>",
                                                                          "2nd road class: ", final_df$X2nd_Road_Class,"<br>",
                                                                          "Road Type: ",  final_df$Road_Type))
  road_df <- (data.frame(road_df))
  map3 <- map_near %>% addCircleMarkers(lng =road_df$Longitude , lat = road_df$Latitude,
                                        popup =paste("Accident index: ", road_df$Accident_Index, "<br>",
                                                     "Speed limit: ", road_df$Speed_limit ,"<br>",
                                                     "1st road class: ",road_df$`1st_Road_Class`, "<br>",
                                                     "2nd road class: ", road_df$`2nd_Road_Class`,"<br>",
                                                     "Road Type: ", road_df$Road_Type ))
  
  
  map3 %>% 
    addPolylines(data = road_existing_20, color= "green")%>%
    addPolylines(data = road_local_20,color= "red")%>%
    addPolylines(data = road_main_20,color= "yellow")%>%
    addPolylines(data = road_30,color= "cyan")%>%
    addPolylines(data = road_40,color= "blue")%>%
    addPolylines(data = road_part,color= "black")%>%
    addPolylines(data = road_50,color= "purple")%>%
    addPolylines(data = road_trunk,color= "orange")
}


#Function to count how many accidents happen per layer and per speed limit
count <- function(data_df){
  count1 <- aggregate(Accident_Index~LAYER,data_df,length)
  count2 <- aggregate(Accident_Index~Speed_limit,data_df,length)
  print(count1)
  print(count2)
}

#Data inputs
##1.Data path 
#dir_path <- "C:\\Users\\Kyriaki Kokka\\Desktop\\"
dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"

#2.Gis 
#Read geodatabase for Edinburgh
#Path
gdb_path <- paste0(dir_path, "20mph study collisions\\20mph.gdb")
gdb_layers <- ogrListLayers(gdb_path)

#Read shapefiles
edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")

#Put ID column as variable to help us identify the id of each road
edin_cons_streets@data <-rowid_to_column(edin_cons_streets@data, "ID")

#Transform to long/lat
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")
edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")

#Read filtered data for Edinburgh 20/30/40mph
edin_road_data <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\Edin_Data"))

#pre 20mph (bind 2013-2015)
pre_20 <- edin_road_data[edin_road_data$Date >="2013-01-01" & edin_road_data$Date <= "2015-12-31", ] %>% filter(!is.na(Date))

#post 20mph (2018)
post_20 <-  edin_road_data[edin_road_data$Date >="2018-01-01" & edin_road_data$Date <= "2018-12-31", ] %>% filter(!is.na(Date))

#Data manipulation
#Get nearest line for pre and post 20mph using the nearest_line function 
nearest_pre <- nearest_line(pre_20,edin_cons_streets)
nearest_pre <- spTransform(nearest_pre,"+init=epsg:4326")

nearest_post <- nearest_line(post_20,edin_cons_streets)
nearest_post <- spTransform(nearest_post,"+init=epsg:4326")

#Merge the dataframes based on the ID of the nearest line in order to connect geoinformation with stasts19 
merged_pre <- merge(data.frame(nearest_pre),edin_cons_streets@data,by.x = "nearest_line_id" ,by.y = "ID")
merged_post <- merge(data.frame(nearest_post),edin_cons_streets@data,by.x = "nearest_line_id" ,by.y = "ID")

#Visualize
leaflet_map(merged_data,edin_road_data[edin_road_data$Date >="2013-01-01" & edin_road_data$Date <= "2015-12-31", ],edin_cons_streets)
leaflet_map(merged_data,edin_road_data[edin_road_data$Date >="2018-01-01" & edin_road_data$Date <= "2018-12-31", ],edin_cons_streets)
