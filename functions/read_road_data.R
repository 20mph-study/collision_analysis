read_road_data <- function(){
  library(readxl)
  library(readr)
  
  #2.Function for deleting Na points
  delete_na <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  
  #1.Function for keeping data of Edinburgh with speed limits 20/30/40
  filter_data <- function(data){
    data <- data %>% filter(data$Speed_limit %in% c(20,30)) 
    data <- data %>% filter(data$`Local_Authority_(District)` == 923)
    #data$Date <- as.Date(data$Date,format="%d/%m/%Y")
    data <- na.omit(data[1:31])#remove NA obs
    return(data)
  }
  
  ##Data path 
  dir_path <- "../data"
  
  #Edinburgh
  #Read csv files
  #Read every year seperately
  #Read csv file from 2005 to 2014
  rd_source <- read_csv(paste0(dir_path, "/collisions 2005 to 2014.csv"))  
  rd_source$Date <- as.Date(rd_source$Date,format="%d/%m/%Y")
  
  #2013 data
  rd_2013 <- rd_source[rd_source$Date >="2013-07-31" & rd_source$Date <= "2013-12-31", ] %>% filter(!is.na(Date))
  rd_2013$Date <- as.Date(rd_2013$Date,format="%d/%m/%Y")
  
  #2014 data
  rd_2014 <- rd_source[rd_source$Date >="2014-01-01" & rd_source$Date <= "2014-12-31", ] %>% filter(!is.na(Date))
  rd_2014$Date <- as.Date(rd_2014$Date,format="%d/%m/%Y")
  
  #2015 data
  rd_2015 <- read_csv(paste0(dir_path, "/collisions 2015.csv"))
  rd_2015$Date <- as.Date(rd_2015$Date,format="%d/%m/%Y")
  
  #2016 data
  rd_2016 <- read_csv(paste0(dir_path, "/collisions 2016.csv"),
                      col_types = cols(Accident_Index=  col_character()))
  rd_2016$Date <- as.Date(rd_2016$Date,format="%d/%m/%Y")
  rd_2016 <- rd_2016[rd_2016$Date >="2016-01-01" & rd_2016$Date <= "2016-07-30", ]
  
  #2018 data
  rd_2018 <- read_csv(paste0(dir_path, "/collisions 2018.csv"),col_types = cols(Accident_Index=  col_character()))
  rd_2018$Date <- as.Date(rd_2018$Date,format="%d/%m/%Y")
  rd_2018$Accident_Index <- as.character(rd_2018$Accident_Index)
  rd_2018 <- rd_2018[rd_2018$Date >="2018-03-06" & rd_2018$Date <= "2018-12-31", ]
  
  #2019 data
  rd_2019 <- read_excel(paste0(dir_path, "/2019 DataJune.xls"))
  rd_2019 <- rd_2019 %>% filter(rd_2019$`Speed Limit` %in% c(20,30)) 
  coord <- rd_2019 %>% st_as_sf(coords = c("Grid Ref: Easting","Grid Ref: Northing"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  rd_2019 <- data.frame(rd_2019,coord)
  
  #rename columns
  rd_2019 <- rename(rd_2019,c("Reference.Number"="Accident_Index" ,   "Speed.Limit" = "Speed_limit",
                              "Grid.Ref..Northing"="Location_Northing_OSGR", "Grid.Ref..Easting"= "Location_Easting_OSGR",
                              "Accident.Year"="year",  "X"="Longitude",   "Y"="Latitude"))
  
  #Create one dataset for all data
  edin_road_data <-rbind(rd_2013,rd_2014,rd_2015,rd_2016,rd_2018)
  edin_road_data <-delete_na(edin_road_data,c("Longitude","Latitude"))
  
  
  #Edinburgh's data
  edin_road_data <- filter_data(edin_road_data)
  
  #Create new csv file with the cleaned data for Edinburgh
  #write.csv(edin_road_data, file = paste0(dir_path, "/edin_road_data.csv"),row.names=FALSE)
  
  #2019 data
  rd_2019a <- read_excel(paste0(dir_path, "/2019 DataJune.xls"))
  rd_2019b <- read_excel(paste0(dir_path, "/2019 Data - July to December.xls"))
  rd_2019 <- rbind.fill(rd_2019a,rd_2019b)
  
  rd_2019c <- read_excel(paste0(dir_path, "/2019 with road class.xls"))
  rd_2019c <- rd_2019c[,c(1,12,13)]
  rd_2019c <- rename(rd_2019c,c("Reference Number"="Accident_Index"  ))
  #rd_2019c$`1st Road Class` <-as.character(rd_2019c$`1st_Road_Class` )
  #rd_2019c$`2nd_Road_Class` <-as.character(rd_2019c$`2nd_Road_Class` )
  
  rd_2019 <- rd_2019 %>% filter(rd_2019$`Speed Limit` %in% c(20,30)) 
  coord <- rd_2019 %>% st_as_sf(coords = c("Grid Ref: Easting","Grid Ref: Northing"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  rd_2019 <- data.frame(rd_2019,coord)
  
  #rename columns
  rd_2019 <- rename(rd_2019,c("Reference.Number"="Accident_Index" ,   "Speed.Limit" = "Speed_limit",
                              "Grid.Ref..Northing"="Location_Northing_OSGR", "Grid.Ref..Easting"= "Location_Easting_OSGR",
                              "Accident.Year"="year",  "X"="Longitude",   "Y"="Latitude","Accident.Date" = "Date", 
                              "Accident.Severity"  = "Accident_Severity",
                              "Number.of.Casualties" = "Number_of_Casualties", "Number.of.Vehicles" = "Number_of_Vehicles"))
  
  rd_2019$Date <- as.Date(rd_2019$Date,format="%d/%m/%Y")
  rd_2019$`Local_Authority_(District)` <- 923
  edin_road_data <- edin_road_data %>% mutate(year = format(as.Date(edin_road_data$Date, format="%m/%d/%Y"),"%Y"))
  rd_2019 <- rd_2019 %>% mutate(year = format(as.Date(rd_2019$Date, format="%m/%d/%Y"),"%Y"))
  edin_data <- rbind.fill(edin_road_data,rd_2019)
  ed <- merge(edin_data,rd_2019c,by.x="Accident_Index",by.y="Accident_Index")
  
  ed$`1st_Road_Class`  <- ed$`1st Road Class`
  ed$`2nd_Road_Class` <- ed$`2nd Road Class`
  edin_road_data <- rbind.fill(edin_road_data,ed[,c(1:33)])
  edin_road_data <-rowid_to_column(edin_road_data, "ID")
  
  #find the new longitude and latitude data
  edin_road_data <-  rename(edin_road_data,c("Longitude"="Longitude before","Latitude"="Latitude before")) 
  coord <- edin_road_data %>% st_as_sf(coords = c("Location_Easting_OSGR","Location_Northing_OSGR"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  edin_road_data <- data.frame(edin_road_data,coord)
  edin_road_data <-  rename(edin_road_data,c("X"="Longitude","Y"="Latitude"))
  edin_road_data <- edin_road_data[,-c(34)]
  return(edin_road_data)
}
