data_manipulation <- function(){
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
  
  source('../functions/read_road_data.R')
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
  
  
  find_class_line <- function(data1,class,inter_type){
    new_lines_u <- nearest_line(delete_na(data1[data1$X1st_Road_Class == class, ],"X1st_Road_Class"),inter_type,15)
    new_lines_u <- spTransform(new_lines_u, "+init=epsg:4326")
    new_lines_u <- unique(merge(data.frame(new_lines_u),inter_type@data, by.x = "nearest_line_id", by.y = "ID"))
    return (new_lines_u[,-c(1)])
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
    mergedDf <- delete_na(data1[data1$X1st_Road_Class == "A", ],"Accident_Index")
    network_existing <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c("20mph main streets",'Trunk roads',"30mph","Part time 20mph"),]
    new_lines_existing <- nearest_line(mergedDf,network_existing,12)
    new_nearest_road <- spTransform(new_lines_existing,"+init=epsg:4326")
    df1 <- unique(merge(data.frame(new_nearest_road),network_existing@data,by.x = "nearest_line_id" ,by.y = "ID"))
    return(unique(rbind.fill(df1)))
  }
  
  #Read geodatabase for Edinburgh
  #Path
  dir_path <- "../data"
  gdb_path <- paste0(dir_path, "/20mph.gdb")
  gdb_layers <- ogrListLayers(gdb_path)
  
  #Read shapefile
  edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets", verbose = FALSE)
  #Put ID column as variable 
  edin_cons_streets@data <-rowid_to_column(edin_cons_streets@data, "ID")
  
  #Read shapefile
  edin_impl_zones <- readOGR(dsn = gdb_path, layer= "ImplementationZones", verbose = FALSE)
  #Change zone 1
  edin_impl_zones@data$ImplementationZone <- as.integer(edin_impl_zones@data$ImplementationZone)
  edin_impl_zones@data[6,]$ImplementationZone <- 7
  
  #Read geodatabase for Edinburgh
  #Path
  gdb_path <- paste0(dir_path, "/class")
  gdb_layers <- ogrListLayers(gdb_path)
  #Read road class network
  edin_aclass_streets <- readOGR(dsn = gdb_path, layer = "a_road_polyline", verbose = FALSE)
  proj4string(edin_aclass_streets) <- proj4string(edin_cons_streets)
  edin_bclass_streets <- readOGR(dsn = gdb_path, layer = "b_road_polyline", verbose = FALSE)
  proj4string(edin_bclass_streets) <- proj4string(edin_cons_streets)
  edin_cclass_streets <- readOGR(dsn = gdb_path, layer = "minor_rd_polyline", verbose = FALSE)
  proj4string(edin_cclass_streets) <- proj4string(edin_cons_streets)
  edin_aclass_streets <- spTransform(edin_aclass_streets, "+init=epsg:4326")
  edin_bclass_streets <- spTransform(edin_bclass_streets, "+init=epsg:4326")
  edin_cclass_streets <- spTransform(edin_cclass_streets, "+init=epsg:4326")
  
  #Transform to long/lat
  edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")
  edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")
  edin_road_network <- edin_cons_streets
  edin_road_network <- edin_road_network[edin_road_network@data$LAYER  %in% c("20mph existing streets",'Trunk roads', "20mph local streets","20mph main streets","30mph","Part time 20mph"),]
  edin_road_network <- spTransform(edin_road_network,"+init=epsg:4326")
  
  inter_a <- intersect(edin_aclass_streets,edin_impl_zones)
  inter_b <- intersect(edin_bclass_streets,edin_impl_zones)
  inter_c <- intersect(edin_cclass_streets,edin_impl_zones)
  inter <- bind(inter_a,inter_b,inter_c)
  inter <- spTransform(inter, "+init=epsg:4326")
  inter@data <-rowid_to_column(inter@data, "ID")
  
  edin_road_data <- read_road_data()
  edin_road_data$X1st_Road_Class <- as.character(edin_road_data$X1st_Road_Class)
  
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 0] <- "Not a junction or within 20meters"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 1] <- "Motorway"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 2] <- "A(M)"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 3] <- "A"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 4] <- "B"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 5] <- "C"
  edin_road_data$X1st_Road_Class[edin_road_data$X1st_Road_Class == 6] <- "Unclassified"
  
  network_main <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c("20mph main streets","30mph","Part time 20mph"),]
  network_main <- spTransform(network_main, "+init=epsg:4326")
  network_minor <- edin_cons_streets[edin_cons_streets@data$LAYER  %in% c("20mph existing streets","20mph local streets"),]
  network_minor <- spTransform(network_minor, "+init=epsg:4326")
  
  nearest_road <- nearest_line(edin_road_data,edin_road_network,12)
  nearest_road <- spTransform(nearest_road,"+init=epsg:4326")
  mergedDf <- unique(merge(data.frame(nearest_road),edin_road_network@data,by.x = "nearest_line_id" ,by.y = "ID"))[,-c(1,2)]
  intersections <- mergedDf[mergedDf$X2nd_Road_Class != -1 ,]
  
  #data that not found a line
  out <- delete_na(edin_road_data[!(edin_road_data$Accident_Index %in% mergedDf$Accident_Index),],"Accident_Index")
  nearest_out <- nearest_line(out,edin_cons_streets,12)
  nearest_out <- spTransform(nearest_out,"+init=epsg:4326")
  mergedDf_out <- unique(merge(data.frame(nearest_out),edin_cons_streets@data,by.x = "nearest_line_id" ,by.y = "ID"))[,-c(1,2)]
  
  first_change <- changed_A_class(mergedDf,edin_cons_streets)
  first_change <- first_change[,-c(1,37,40,41,43,44,47,48,50)]
  first_change$LAYER.y <- as.character(first_change$LAYER.y)
  first_change$LAYER <- first_change$LAYER.y
  
  not <- delete_na(mergedDf[!(mergedDf$Accident_Index %in% first_change$Accident_Index),],"Accident_Index")
  final <- unique(rbind.fill(not,first_change))
  final$LAYER <- as.character(final$LAYER)
  
  final$LAYER <- as.character((final$LAYER)) 
  final$Date <- as.character(final$Date)
  final$X1st_Road_Class <- as.character(final$X1st_Road_Class)
  
  #collisions that change type of road segment
  df2019 <- final[final$year == "2019", ] [,-c(42:46)]
  intersectiondf <- changed_layer(df2019,edin_cons_streets)[,-c(1,37,40,41,43,46,47,49)]
  
  #For these collisions find the road class 
  new_class <- new_nearest_line(intersectiondf,inter,18)
  new_class <- spTransform(new_class, "+init=epsg:4326")
  new_class <- unique(merge(data.frame(new_class),inter@data, by.x = "nearest_line_id", by.y = "ID"))[,-c(1)]
  
  new_class[new_class$CODE == 3001 ,]$X1st_Road_Class <- "A"
  new_class[new_class$CODE == 3002 ,]$X1st_Road_Class <- "B"
  new_class[new_class$CODE == 3004 ,]$X1st_Road_Class <- "Unclassified"
  new_class$LAYER <- new_class$LAYER.x
  new_class <- new_class[,-c(36:45,47:54)]
  
  second_change <- changed_A_class(new_class,edin_cons_streets)
  second_change$LAYER <- second_change$LAYER.y
  second_not <- delete_na(final[!(final$Accident_Index %in% second_change$Accident_Index),],"Accident_Index")
  second_final <- unique(rbind.fill(second_not,second_change))
  return(second_final)
}



