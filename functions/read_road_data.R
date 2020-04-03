read_road_data <- function(){
  library(readxl)
  library(readr)
  
  edin_road_data <- read_csv(paste0(dir_path, "/edin_road_data.csv"))
  edin_road_data <- edin_road_data %>% filter(edin_road_data$Speed_limit %in% c(20,30)) 
  
  #2016 data
  rd_2016 <- read_csv(paste0(dir_path, "/collisions_2016.csv"))
  rd_2016$Date <- as.Date(rd_2016$Date,format="%d/%m/%Y")
  rd_2016 <- rd_2016[rd_2016$Date > "2016-07-20" & rd_2016$Date <= "2016-07-30", ] 
  rd_2016 <- rd_2016 %>% filter(rd_2016$`Local_Authority_(District)` == "923")
  rd_2016 <- rd_2016 %>% filter(rd_2016$Speed_limit %in% c(20,30))
  
  
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
  edin_road_data <- rbind.fill(edin_road_data,rd_2016)
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
  return(edin_road_data)
}
