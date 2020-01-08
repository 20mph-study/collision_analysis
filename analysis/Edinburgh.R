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
  return(data)
}

#2.Function for deleting Na points
delete_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#3.Read csv files
#read all at once
#files <- list.files(path = "C:\\Users\\Kyriaki Kokka\\Desktop\\20mph study collisions\\collisions\\", pattern = "*.csv", full.names = T)
#source_data <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id") 



#Read every year seperately
#Read csv file from 2005 to 2014
rd_source <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2005 to 2014.csv"))  
rd_source$Date <- as.Date(rd_source$Date,format="%d/%m/%Y")

#2013 data
rd_2013 <- rd_source[rd_source$Date >="2013-01-01" & rd_source$Date <= "2013-12-31", ] %>% filter(!is.na(Date))

#2014 data
rd_2014 <- rd_source[rd_source$Date >="2014-01-01" & rd_source$Date <= "2014-12-31", ] %>% filter(!is.na(Date))
rd_2014b <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2014.csv"))
rd_2014b$Date <- as.Date(rd_2014b$Date,format="%d/%m/%Y")
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

#Bind and clean the data
edin_road_data <-rbind(rd_2013,rd_2014,rd_2015,rd_2018) 
edin_road_data <- filter_data(edin_road_data)
edin_road_data <-delete_na(edin_road_data,c("Longitude","Latitude"))

#Create new csv file with the cleaned data  make it go to a correct path ?
write.csv(edin_road_data, file ="V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\collisions\\Edin_data.csv",row.names=FALSE)
