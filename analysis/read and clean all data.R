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

#2.Function for deleting Na points
delete_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#1.Function for keeping data of Edinburgh with speed limits 20/30/40
filter_data <- function(data){
  data <- data %>% filter(data$Speed_limit %in% c(20,30,40)) 
  data <- data %>% filter(data$`Local_Authority_(District)` == 923)
  #data$Date <- as.Date(data$Date,format="%d/%m/%Y")
  data <- na.omit(data[1:31])#remove NA obs
  return(data)
}

##Data path 
#dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"
dir_path <- "C:\\Users\\Kyriaki Kokka\\Desktop\\"

#Edinburgh
#Read csv files
#Read every year seperately
#Read csv file from 2005 to 2014
rd_source <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2005 to 2014.csv"))  
rd_source$Date <- as.Date(rd_source$Date,format="%d/%m/%Y")

#2013 data
rd_2013 <- rd_source[rd_source$Date >="2013-07-31" & rd_source$Date <= "2013-12-31", ] %>% filter(!is.na(Date))
rd_2013$Date <- as.Date(rd_2013$Date,format="%d/%m/%Y")

#2014 data
rd_2014 <- rd_source[rd_source$Date >="2014-01-01" & rd_source$Date <= "2014-12-31", ] %>% filter(!is.na(Date))
rd_2014$Date <- as.Date(rd_2014$Date,format="%d/%m/%Y")

#2015 data
rd_2015 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2015.csv"))
rd_2015$Date <- as.Date(rd_2015$Date,format="%d/%m/%Y")

#2016 data
rd_2016 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2016.csv"))
rd_2016$Date <- as.Date(rd_2016$Date,format="%d/%m/%Y")
rd_2016 <- rd_2016[rd_2016$Date >="2016-01-01" & rd_2016$Date <= "2016-07-20", ]

#2018 data
rd_2018 <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2018.csv"))
rd_2018$Date <- as.Date(rd_2018$Date,format="%d/%m/%Y")
rd_2018$Accident_Index <- as.character(rd_2018$Accident_Index)
rd_2018 <- rd_2018[rd_2018$Date >="2018-03-06" & rd_2018$Date <= "2018-12-31", ]

#2019 data
rd_2019 <- read_excel(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2019 Jan to May Edinburgh only.xls"))
rd_2019 <- rd_2019 %>% filter(rd_2019$`Speed Limit` %in% c(20,30,40)) 
coord <- rd_2019 %>% st_as_sf(coords = c("Grid Ref: Easting","Grid Ref: Northing"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
rd_2019 <- data.frame(rd_2019,coord)

#rename columns
rd_2019 <- rename(rd_2019,c("Reference.Number"="Accident_Index" ,   "Speed.Limit" = "Speed_limit",
                                           "Grid.Ref..Northing"="Location_Northing_OSGR", "Grid.Ref..Easting"= "Location_Easting_OSGR",
                                           "Accident.Year"="year",  "X"="Longitude",   "Y"="Latitude"))

#Create one dataset for all data
edin_road_data <-rbind(rd_2013,rd_2014,rd_2015,rd_2016,rd_2018)
edin_road_data <-delete_na(edin_road_data,c("Longitude","Latitude"))

#Keep a copy to use it in control zones  
control_zone_data <- edin_road_data %>% filter(edin_road_data$Speed_limit %in% c(20,30,40)) 
control_zone_data <- control_zone_data %>% filter(control_zone_data$`Local_Authority_(District)` %in% c("910","918","926","938","925","935","932"))

#Edinburgh's data
edin_road_data <- filter_data(edin_road_data)

#Create new csv file with the cleaned data for Edinburgh
write.csv(edin_road_data, file = paste0(dir_path, "20mph study collisions\\cleaned datasets\\edin_road_data.csv"),row.names=FALSE)

#Create new csv file with the cleaned data for control zones
write.csv(control_zone_data, file = paste0(dir_path, "20mph study collisions\\cleaned datasets\\edin_control_data.csv"),row.names=FALSE)

#Create new csv file with 2019 cleaned data 
write.csv(rd_2019, file = paste0(dir_path, "20mph study collisions\\cleaned datasets\\data2019.csv"),row.names=FALSE)

##Read casualties data
cs_source <- read_csv(paste0(dir_path, "20mph study collisions\\20mph study casualties\\Casualties2005to2014.csv"))
cs_2015 <- read_csv(paste0(dir_path, "20mph study collisions\\20mph study casualties\\Casualties2015.csv"))
cs_2016 <- read_csv(paste0(dir_path, "20mph study collisions\\20mph study casualties\\Casualties2016.csv"))
cs_2018 <- read_csv(paste0(dir_path, "20mph study collisions\\20mph study casualties\\Casualties2018.csv"))
cs_2018$Accident_Index <- as.character(cs_2018$Accident_Index)
cs_source <- cs_source[,c(1,8)]
cs_2015 <- cs_2015[,c(1,8)]
cs_2016 <- cs_2016[,c(1,8)]
cs_2018 <- cs_2018[,c(1,8)]

casualties_data <- rbind(cs_source,cs_2015,cs_2016,cs_2018)
#casualties_data <- merge(edin_road_data,casualties_data,by.x = "Accident_Index" ,by.y = "Accident_Index")
casualties_data <-delete_na(casualties_data,c("Accident_Index"))

#Create new csv file with the cleaned data
write.csv(casualties_data, file = paste0(dir_path, "20mph study collisions\\cleaned datasets\\edin_casualties_data.csv"),row.names=FALSE)

#Belfast data 
#PSNI data
filename <- paste0(dir_path, "20mph study collisions\\Belfast\\Collisions 1998-2017.xls")
bel <- read_excel(filename)
belf_data <- read_excel(filename, sheet = 2)
colnames(belf_data) <- colnames(bel)
belf_data$a_date <- as.Date(belf_data$a_date,format="%m/%d/%Y")
belf_data <- belf_data[belf_data$a_date >="2013-07-31" & belf_data$a_date <= "2016-07-20", ] %>% filter(!is.na(a_date))

#Belfast data 2018
belf_2018 <-  read_csv(paste0(dir_path, "20mph study collisions\\Belfast\\Collisions Jan2018 Mar2019.csv"))
belf_2018$a_date <- as.Date(belf_2018$a_date,format="%m/%d/%Y")

belf_data <- rbind(belf_data,belf_2018)

#Convert easting/northing to lat/long
coord <- belf_data %>% st_as_sf(coords = c("a_gd1","a_gd2"), crs = 29903) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
belf_data <- data.frame(belf_data,coord)
belf_data <- belf_data %>% filter(belf_data$a_speed %in% c(20,30,40)) 

belf_control_data <- belf_data %>% filter(belf_data$LGDNAME == "Ards & North Down") 

belf_data <- belf_data %>% filter(belf_data$LGDNAME == "Belfast City") 


#Create new csv file with control zone data
write.csv(belf_control_data, file = paste0(dir_path, "20mph study collisions\\Belfast\\Belfast_control_data.csv"),row.names=FALSE)
<<<<<<< HEAD:read and clean all data.R

#Create new csv file with the selected Belfast data
write.csv(belf_data, file = paste0(dir_path, "20mph study collisions\\Belfast\\Belfast_data.csv"),row.names=FALSE)

=======
>>>>>>> 0681d3c6d7c9ff40ba879a4599c35451837b935a:analysis/read and clean all data.R

#Create new csv file with the selected Belfast data
write.csv(belf_data, file = paste0(dir_path, "20mph study collisions\\Belfast\\Belfast_data.csv"),row.names=FALSE)
