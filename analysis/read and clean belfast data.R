library(readxl)
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
library(knitr)
library(kableExtra)
library(plyr)

#Belfast data 
#PSNI data
dir_path <- "C:\\Users\\Kyriaki Kokka\\Desktop\\"
filename <- paste0(dir_path, "20mph study collisions\\Belfast\\Collisions 1998-2017.xls")
bel <- read_excel(filename)
belf_data <- read_excel(filename, sheet = 2)
colnames(belf_data) <- colnames(bel)
belf_data$a_date <- as.Date(belf_data$a_date,format="%m/%d/%Y")
belf_data <- belf_data[belf_data$a_date >="2013-02-01" & belf_data$a_date <= "2017-12-31", ] %>% filter(!is.na(a_date))

#Belfast data 2018
belf_2018 <-  read_csv(paste0(dir_path, "20mph study collisions\\Belfast\\Collisions Jan2018 Mar2019.csv"))
belf_2018$a_date <- as.Date(belf_2018$a_date,format="%m/%d/%Y")

belf_data <- rbind(belf_data,belf_2018)

#Convert easting/northing to lat/long
coord <- belf_data %>% st_as_sf(coords = c("a_gd1","a_gd2"), crs = 29903) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
belf_data <- data.frame(belf_data,coord)
belf_data <- belf_data %>% filter(belf_data$a_speed %in% c(20,30)) 

belf_control_data <- belf_data %>% filter(belf_data$LGDNAME == "Ards & North Down") 

belf_data <- belf_data %>% filter(belf_data$LGDNAME == "Belfast City") 


#Create new csv file with control zone data
write.csv(belf_control_data, file = paste0(dir_path, "20mph study collisions\\Belfast\\Belfast_control_data.csv"),row.names=FALSE)

#Create new csv file with the selected Belfast data
write.csv(belf_data, file = paste0(dir_path, "20mph study collisions\\Belfast\\Belfast_data.csv"),row.names=FALSE)
 