data_manipulation <- function(edin_road_network,edin_road_data){
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
  library(grid)
  library(gridExtra)
  library(xtable)
  library(knitr)
  library(kableExtra)
  library(plyr)
  library(RColorBrewer)
  
  source('../functions/localSnapPointsToLines.R')
  source('../functions/count_pre.R')
  source('../functions/table_annual.R')
  source('../functions/nearest_line.R')
  source('../functions/delete_na.R')
  
  new_nearest_line <- function(df,road_net,tolerance){
    #Create planar(cartesian) projection 
    crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
    wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
    
    #Convert data to planar projection  
    road_net <- spTransform(road_net,crs)
    coord <- data.frame(df$X,df$Y)
    df <-  spTransform(SpatialPointsDataFrame(coords = coord,proj4string = wgs84,data = df), crs)
    
    #Make sure our data have same projections
    proj4string(df) <- proj4string(road_net)
    
    #Find nearest line with maxDist=10m 
    nearest_line_sp <- localSnapPointsToLines(df,road_net,maxDist = tolerance,withAttrs=TRUE)
    
    return(nearest_line_sp)
  }
  
  changed_layer <- function(data1,edin_cons_streets){
    #20mph existing
    mergedDf <- delete_na(data1[data1$LAYER == "20mph existing streets", ],"Accident_Index")
    network_existing <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c( "20mph local streets",'Trunk roads',"20mph main streets","30mph","Part time 20mph"),]
    new_lines_existing <- nearest_line(mergedDf,network_existing,12)
    new_nearest_road <- spTransform(new_lines_existing,"+init=epsg:4326")
    df1<- unique(merge(data.frame(new_nearest_road),network_existing@data,by.x = "nearest_line_id" ,by.y = "ID"))
    #30mph
    network_30mph <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c( "20mph local streets",'Trunk roads',"20mph main streets","20mph existing streets","Part time 20mph"),]
    mergedDf <- delete_na(data1[data1$LAYER == "30mph", ],"Accident_Index")
    new_lines_30mph <- nearest_line(mergedDf,network_30mph,12)
    new_nearest_road_30mph <- spTransform(new_lines_30mph,"+init=epsg:4326")
    df2<- unique(merge(data.frame(new_nearest_road_30mph),network_30mph@data,by.x = "nearest_line_id" ,by.y = "ID"))
    #20mph local
    network_20mphlocal <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c( "30mph",'Trunk roads', "20mph main streets","20mph existing streets","Part time 20mph"),]
    mergedDf <- delete_na(data1[data1$LAYER == c("20mph local streets"), ],"Accident_Index")
    new_lines_20mphlocal <- nearest_line(mergedDf,network_20mphlocal,12)
    new_nearest_road_20mphlocal <- spTransform(new_lines_20mphlocal,"+init=epsg:4326")
    df3 <- unique(merge(data.frame(new_nearest_road_20mphlocal),network_20mphlocal@data,by.x = "nearest_line_id" ,by.y = "ID"))
    #20mph main
    network_20mphmain <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c( "30mph",'Trunk roads',"20mph local streets","20mph existing streets","Part time 20mph"),]
    mergedDf <- delete_na(data1[data1$LAYER == c("20mph main streets"), ],"Accident_Index")
    new_lines_20mphmain <- nearest_line(mergedDf,network_20mphmain,12)
    new_nearest_road_20mphmain <- spTransform(new_lines_20mphmain,"+init=epsg:4326")
    df4 <- unique(merge(data.frame(new_nearest_road_20mphmain),network_20mphmain@data,by.x = "nearest_line_id" ,by.y = "ID"))
    return(unique(rbind.fill(df1,df2,df3,df4)))
  }
  
  changed_A_class <- function(data1,edin_cons_streets){
    #A
    mergedDf <- data1 %>% filter(data1$X1st_Road_Class %in% "A")
    network_existing <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c("20mph main streets",'Trunk roads',"30mph","Part time 20mph"),]
    new_lines_existing <- nearest_line(mergedDf,network_existing,12)
    new_nearest_road <- spTransform(new_lines_existing,"+init=epsg:4326")
    df1 <- unique(merge(data.frame(new_nearest_road),network_existing@data,by.x = "nearest_line_id" ,by.y = "ID"))
    return(unique(rbind.fill(df1)))
  }
  
  edin_road_data$X1st_Road_Class <- as.character(edin_road_data$X1st_Road_Class)
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 0] <- "Not a junction or within 20meters"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 1] <- "Motorway"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 2] <- "A(M)"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 3] <- "A"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 4] <- "B"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 5] <- "C"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 6] <- "Unclassified"
  
  nearest_road <- nearest_line(edin_road_data,edin_road_network,12)
  nearest_road <- spTransform(nearest_road,"+init=epsg:4326")
  mergedDf <- unique(merge(data.frame(nearest_road),edin_road_network@data,by.x = "nearest_line_id" ,by.y = "ID"))[,-c(1,2)]
  intersections <- mergedDf %>% filter(mergedDf$X2nd_Road_Class != -1 )
  
  changed <- changed_A_class(mergedDf,edin_cons_streets)[,-c(1,37,40,41,43,44,47,48,50)]
  changed$LAYER <- as.character(changed$LAYER.y)
  
  not_changed <- delete_na(mergedDf[!(mergedDf$Accident_Index %in% changed$Accident_Index),],"Accident_Index")
  final <- unique(rbind.fill(not_changed,changed))
  
  final$LAYER <- as.character((final$LAYER)) 
  final$Date <- as.character(final$Date)
  final$X1st_Road_Class <- as.character(final$X1st_Road_Class)
  
  return(final)
}



