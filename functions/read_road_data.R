read_road_data <- function(){
  edin_road_data <- read_csv(paste0(dir_path, "/edin_road_data.csv"))
  edin_road_data <- edin_road_data %>% filter(edin_road_data$Speed_limit %in% c(20,30)) 
  
  #rd_2019 <- read_csv(paste0(dir_path, "/data2019.csv"))
  #2019 data
  rd_2019 <- read_excel(paste0(dir_path, "/2019 DataJune.xls"))
  rd_2019 <- rd_2019 %>% filter(rd_2019$`Speed Limit` %in% c(20,30)) 
  coord <- rd_2019 %>% st_as_sf(coords = c("Grid Ref: Easting","Grid Ref: Northing"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  rd_2019 <- data.frame(rd_2019,coord)
  
  
  #rename columns
  rd_2019 <- rename(rd_2019,c("Reference.Number"="Accident_Index" ,   "Speed.Limit" = "Speed_limit",
                              "Grid.Ref..Northing"="Location_Northing_OSGR", "Grid.Ref..Easting"= "Location_Easting_OSGR",
                              "Accident.Year"="year",  "X"="Longitude",   "Y"="Latitude","Accident.Date" = "Date", "Accident.Severity"  = "Accident_Severity",
                              "Number.of.Casualties" = "Number_of_Casualties", "Number.of.Vehicles" = "Number_of_Vehicles"))
  
  rd_2019$Date <- as.Date(rd_2019$Date,format="%d/%m/%Y")
  
  edin_road_data <- edin_road_data %>% mutate(year = format(as.Date(edin_road_data$Date, format="%m/%d/%Y"),"%Y"))
  rd_2019 <- rd_2019 %>% mutate(year = format(as.Date(rd_2019$Date, format="%m/%d/%Y"),"%Y"))
  edin_road_data <- rbind.fill(edin_road_data,rd_2019)
  
  edin_road_data <-rowid_to_column(edin_road_data, "ID")
  
  #find the new longitude and latitude data
  edin_road_data <-  rename(edin_road_data,c("Longitude"="Longitude before","Latitude"="Latitude before")) 
  coord <- edin_road_data %>% st_as_sf(coords = c("Location_Easting_OSGR","Location_Northing_OSGR"), crs = 27700) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  edin_road_data <- data.frame(edin_road_data,coord)
  edin_road_data <-  rename(edin_road_data,c("X"="Longitude","Y"="Latitude")) 
  return(edin_road_data)
}