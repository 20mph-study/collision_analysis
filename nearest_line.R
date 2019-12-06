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

#1.Function for keeping data of Edinburgh with speed limits 20/30/40
filter_data <- function(data){
  data <- data %>% filter(data$Speed_limit %in% c(20,30,40)) 
  data <- data %>% filter(data$`Local_Authority_(District)` == 923)
  #data$Date <- as.Date(data$Date,format="%d/%m/%Y")
  #data <- na.omit(data[1:31])#remove NA obs
  return(data)
}

#2.Function for deleting Na points
delete_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Data inputs
##1.Data path 
dir_path <- "C:\\Users\\Kyriaki Kokka\\Desktop\\"
#dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"

#2.Gis 
#Read geodatabase for Edinburgh
#Path
gdb_path <- paste0(dir_path, "20mph study collisions\\20mph.gdb")
gdb_layers <- ogrListLayers(gdb_path)

#Read shapefiles
edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")

#Put ID column as variable 
edin_cons_streets@data <-rowid_to_column(edin_cons_streets@data, "ID")

#Transform to long/lat
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")
edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")

#3.Read csv files
#read all at once
files <- list.files(path = "C:\\Users\\Kyriaki Kokka\\Desktop\\20mph study collisions\\collisions\\", pattern = "*.csv", full.names = T)
source_data <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id")

#Read every year seperately
#Read csv file from 2005 to 2014
rd_source <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2005 to 2014.csv"))  
rd_source$Date <- as.Date(rd_source$Date,format="%d/%m/%Y")

#2013 data
rd_2013 <- rd_source[rd_source$Date >="2013-01-01" & rd_source$Date <= "2013-12-31", ] %>% filter(!is.na(Date))

#2014 data
rd_2014 <- rd_source[rd_source$Date >="2014-01-01" & rd_source$Date <= "2014-12-31", ] %>% filter(!is.na(Date))
rd_2014b <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2014.csv"))
rd_2014 <- rbind(rd_2014,rd_2014b)

#2015 data
rd_2015 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2015.csv"))
rd_2015$Date <- as.Date(rd_2015$Date,format="%d/%m/%Y")

#2018 data
rd_2018 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2018.csv"))
rd_2018$Date <- as.Date(rd_2018$Date,format="%d/%m/%Y")

#2019 data
#rd_2019 <- read_excel("C:\\Users\\Kyriaki Kokka\\Desktop\\20mph study collisions\\collisions\\collisions 2019 Jan to May Edinburgh only.xls")
#rd_2019 <- rd_2019%>%filter(rd_2019$`Speed Limit`  %in% c(20,30,40)) 

edin_road_data <-rbind(rd_2013,rd_2014,rd_2015,rd_2018) 
edin_road_data <- filter_data(edin_road_data)
edin_road_data <-delete_na(edin_road_data,c("Longitude","Latitude"))

write.csv(edin_road_data, file = "Edin_Data.csv",row.names=FALSE)

#Df for pre 20mph (bind 2013-2015)
pre_20 <- edin_road_data[edin_road_data$Date >="2013-01-01" & edin_road_data$Date <= "2015-12-31", ] %>% filter(!is.na(Date))

#Df for post 20mph (2018)
post_20 <-  edin_road_data[edin_road_data$Date >="2018-01-01" & edin_road_data$Date <= "2018-12-31", ] %>% filter(!is.na(Date))

#Data manipulation 
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

#Get nearest line for pre and post 20mph 
nearest_pre <- nearest_line(pre_20,edin_cons_streets)
nearest_post <- nearest_line(post_20,edin_cons_streets)

#Keep nearest line in df
nearest_pre_line <- data.frame(nearest_pre)
nearest_post_line <- data.frame(nearest_post)

#Merge the dataframes based on the ID of the nearest line in order to connect geoinformation with stasts19 
merged_data <- merge(nearest_pre_line,edin_cons_streets@data,by.x = "nearest_line_id" ,by.y = "ID")

#Keep the columns we need to extract information ( Accident_index,nearest_line_id ,Layer, Speed_limit )
result_pre <- data.frame(merged_data[1],merged_data[2],merged_data[19],merged_data[39])

#Count how many accidents happen per layer and per speed limit
count1 <- aggregate(Accident_Index~LAYER,result_pre,length)
count2 <- aggregate(Accident_Index~Speed_limit,result_pre,length) 

#Function for leaflet visualisations

#Convert data projections back to lat/long to plot with leaflet
edin_cons_streets <- spTransform(edin_cons_streets,"+init=epsg:4326")
nearest_pre <- spTransform(nearest_pre, "+init=epsg:4326")

#Leaflet maps of the road network , accidents and their projections
road_existing_20 <-edin_cons_streets%>% filter(edin_cons_streets$LAYER == "20mph existing streets")
road_local_20 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "20mph local streets")
road_main_20 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "20mph main streets")
road_30 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "30mph")
road_40 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "40mph")
road_part <- filter(edin_cons_streets, edin_cons_streets$LAYER == "Part time 20mph")
road_50 <- filter(edin_cons_streets, edin_cons_streets$LAYER == "50, 60 or 70mph")
road_trunk <- filter(edin_cons_streets, edin_cons_streets$LAYER == "Trunk roads")

#Visualize
map <- leaflet() %>% addTiles() %>% addPolygons(data = edin_cons_streets)

map_near <-map %>% leaflet() %>% addTiles() %>% addMarkers(lng =merged_data$X, lat = merged_data$Y,
                                                    popup =paste("Accident index: ", merged_data$Accident_Index, "<br>",
                                                                 "Speed limit: ", merged_data$Speed_limit ,"<br>",
                                                                 "Layer: ",merged_data$LAYER,"<br>",
                                                                 "1st road class: ",merged_data$X1st_Road_Class, "<br>",
                                                                 "2nd road class: ", merged_data$X2nd_Road_Class,"<br>",
                                                                 "Road Type: ",  merged_data$Road_Type))

map3 <- map_near %>% addCircleMarkers(lng =rd_final@data$Longitude , lat = rd_final@data$Latitude,
                                      popup =paste("Accident index: ", rd_final@data$Accident_Index, "<br>",
                                                   "Speed limit: ", rd_final@data$Speed_limit ,"<br>",
                                                   "1st road class: ",rd_final@data$`1st_Road_Class`, "<br>",
                                                   "2nd road class: ", rd_final@data$`2nd_Road_Class`,"<br>",
                                                   "Road Type: ", rd_final@data$Road_Type ))

map3 %>% 
  addPolylines(data = road_existing_20, color= "green")%>%
  addPolylines(data = road_local_20,color= "red")%>%
  addPolylines(data = road_main_20,color= "yellow")%>%
  addPolylines(data = road_30,color= "cyan")%>%
  addPolylines(data = road_40,color= "blue")%>%
  addPolylines(data = road_part,color= "black")%>%
  addPolylines(data = road_50,color= "purple")%>%
  addPolylines(data = road_trunk,color= "orange")
  

