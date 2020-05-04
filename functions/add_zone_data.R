add_zone_data <- function(){
  rd2016 <- read_csv(paste0(dir_path, "/collisions 2016.csv"))
  rd2016$Date <- as.Date(rd2016$Date,format="%d/%m/%Y")
  rd2016 <- rd2016 %>% filter(rd2016$Date >= "2016-07-31" & rd2016$Date <= "2016-12-31" )
  rd2017 <- read_csv(paste0(dir_path, "/collisions 2017.csv"))
  rd2017$Date <- as.Date(rd2017$Date,format="%d/%m/%Y")
  rd2018 <- read_csv(paste0(dir_path, "/collisions 2018.csv"))
  rd2018$Date <- as.Date(rd2018$Date,format="%d/%m/%Y")
  rd2018 <-rd2018 %>% filter(rd2018$Date >= "2018-01-01" & rd2018$Date <= "2018-03-05" )
  
  rd <- rbind(rd2016,rd2017,rd2018)
  rd <- rd %>% mutate(year = format(as.Date(rd$Date, format="%m/%d/%Y"),"%Y"))
  rd <- rd %>% filter(rd$Speed_limit %in% c(20,30))
  rd$`Local_Authority_(District)` <- as.character(rd$`Local_Authority_(District)`)
  rd <- delete_na(rd,c("Longitude","Latitude"))
  return(rd)
}
