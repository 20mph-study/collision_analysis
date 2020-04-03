read_controlzone_data <- function(){
  ##Data path 
  dir_path <- "../data"
  
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
  rd_2016 <- read_csv(paste0(dir_path, "/collisions_2016.csv"))
  rd_2016$Date <- as.Date(rd_2016$Date,format="%d/%m/%Y")
  
  #2017 data
  rd_2017 <- read_csv(paste0(dir_path, "/collisions 2017.csv"))
  rd_2017$Date <- as.Date(rd_2017$Date,format="%d/%m/%Y")
  
  #2018 data
  rd_2018 <- read_csv(paste0(dir_path, "/collisions 2018.csv"))
  rd_2018$Date <- as.Date(rd_2018$Date,format="%d/%m/%Y")
  
  control_zone_data <- rbind(rd_2013,rd_2014,rd_2015,rd_2016,rd_2017,rd_2018)
  control_zone_data <- control_zone_data %>% filter(control_zone_data$Speed_limit %in% c(20,30)) 
  control_zone_data <- control_zone_data %>% filter(control_zone_data$`Local_Authority_(District)` %in% c("910","925","918","935","926"))
  control_zone_data <- control_zone_data %>% mutate(year = format(as.Date(control_zone_data$Date, format="%m/%d/%Y"),"%Y"))
  control_zone_data$Date <- as.character(control_zone_data$Date)
  control_zone_data$`Local_Authority_(District)` <- as.character(control_zone_data$`Local_Authority_(District)` )
  control_zone_data <- delete_na(control_zone_data,"Longitude")
  
  return(control_zone_data)
}
