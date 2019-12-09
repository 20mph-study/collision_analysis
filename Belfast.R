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
  data <- data %>% filter(data$LGDNAME == "Belfast City")
  #data$Date <- as.Date(data$Date,format="%d/%m/%Y")
  #data <- na.omit(data[1:31])#remove NA obs
  return(data)
}

delete_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Load data from the network drive for Belfast road network
belfast_road_data <- readOGR('V:/Studies/MOVED/HealthImpact/data/20mph study collisions/20mph Speed Limit Streets/20mph_Speed_Limit_Streets.shp')


# Tranform the data by applying a projection
belfast_road_data <- spTransform(belfast_road_data, "+init=epsg:4326")

# Visualize using leaflet
leaflet(belfast_road_data) %>% addTiles() %>% addPolygons()

filename <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\Belfast\\Collisions 1998-2017.xls"
bel <- read_excel(filename)
belf_data <- read_excel(filename, sheet = 2)
colnames(belf_data) <- colnames(bel)

belfast_road_data@data
names(belfast_road_data)
summary(belfast_road_data)

belf_data <- belf_data %>% filter(belf_data$`Causeway Coast & Glens` == "Belfast City") %>% filter(belf_data$`60` %in% c(20,30,40)) 
