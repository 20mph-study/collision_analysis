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

##Functions
#1.Function for keeping data of Edinburgh with speed limits 20/30/40
filter_data <- function(data){
  data <- data %>% filter(data$Speed_limit %in% c(20,30,40)) 
  data <- data %>% filter(data$`Local_Authority_(District)` == 923)
  data <- na.omit(data[1:31])#remove NA obs
  return(data)
}

#Data inputs
##1.Data path
dir_path <- "C:\\Users\\Kyriaki Kokka\\Desktop\\"
#dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"

#2.Gis 
#Read geodatabase for Edinburgh
gdb_path <- paste0(dir_path, "20mph study collisions\\20mph.gdb")
gdb_layers <- ogrListLayers(gdb_path)
edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")


#3.Read csv files
#Read csv file from 2005 to 2014
rd_source <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2005 to 2014.csv"))  
rd_source$Date <- as.Date(rd_source$Date,format="%d/%m/%Y")

#2013 data
rd_2013 <- rd_source[rd_source$Date >="2013-01-01" & rd_source$Date <= "2013-12-31", ] %>% filter(!is.na(Date))
rd_2013 <- filter_data(rd_2013)

#2014 data
rd_2014 <- rd_source[rd_source$Date >="2014-01-01" & rd_source$Date <= "2014-12-31", ] %>% filter(!is.na(Date))
rd_2014b <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2014.csv"))
rd_2014 <- rbind(rd_2014,rd_2014b)
rd_2014 <- filter_data(rd_2014)

#2015 data
rd_2015 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2015.csv"))
rd_2015 <- filter_data(rd_2015)

#2018 data
rd_2018 <-read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2018.csv"))
rd_2018 <- filter_data(rd_2018)

#Df for pre 20mph (bind 2013-2015)
pre_20 <-rbind(rd_2013,rd_2014,rd_2015)

#Df for post 20mph (2018)
post_20 <-rd_2018

#Data manipulation 
#Function for the nearest line
nearest_line <-function(data1,road_network){
  #Create planar(cartesian) projection 
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  #Convert data to planar projection  
  road_network <- spTransform(road_network,crs)
  utm_data <-  spTransform(SpatialPointsDataFrame(coords = data1[4:5],proj4string = wgs84,data = data1), crs)
  
  #Make sure our data have same projections
  proj4string(utm_data) <- proj4string(road_network)
  
  #Find nearest line with maxDist=20m 
  nearest_line_sp <- snapPointsToLines(utm_data,road_network,maxDist=20)
  
  return(nearest_line_sp,utm_data)
}
#Get nearest line per year
c(nearest_2013,utm_proj_2013) <- nearest_line(rd_2013,edin_cons_streets)
c(nearest_2014,utm_proj_2014) <- nearest_line(rd_2014,edin_cons_streets)
c(nearest_2015,utm_proj_2015) <- nearest_line(rd_2015,edin_cons_streets)
c(nearest_2018,utm_proj_2018) <- nearest_line(rd_2018,edin_cons_streets)

#Get nearest line for pre and post 20mph 
c(nearest_pre,utm_proj_data) <- nearest_line(pre_20,edin_cons_streets)
nearest_post <- nearest_line(post_20,edin_cons_streets)

#Keep nearest line in df
nearest_pre_line <- data.frame(nearest_pre)
nearest_post_line <- data.frame(nearest_post)
nearest_line_2013 <- data.frame()

#Put ID column as variable 
edin_cons_streets@data <-rowid_to_column(edin_cons_streets@data, "ID")

#Merge the dataframes based on the ID of the nearest line in order to connect geoinformation with stasts19 
merged_data <- merge(nearest_pre_line,edin_cons_streets@data,by.x = "nearest_line_id" ,by.y = "ID")

#Keep the columns we need to extract information ( Accident_index, ,Layer, Speed_limit )
result_pre <- data.frame(merged_data[1],merged_data[2],merged_data[19],merged_data[38])

#Count how many accidents happen per layer and per speed limit
count1 <- aggregate(Accident_Index~LAYER,result_pre,length)
count2 <- aggregate(Accident_Index~Speed_limit,result_pre,length) 

#Convert data projections back to lat/long to plot with leaflet
edin_cons_streets <- spTransform(edin_cons_streets,"+init=epsg:4326")
rd_final <- spTransform(rd_final, "+init=epsg:4326")
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

map3 %>% 
  addPolylines(data = road_existing_20, color= "green")%>%
  addPolylines(data = road_local_20,color= "red")%>%
  addPolylines(data = road_main_20,color= "yellow")%>%
  addPolylines(data = road_30,color= "cyan")%>%
  addPolylines(data = road_40,color= "blue")%>%
  addPolylines(data = road_part,color= "black")%>%
  addPolylines(data = road_50,color= "purple")%>%
  addPolylines(data = road_trunk,color= "orange")
  

