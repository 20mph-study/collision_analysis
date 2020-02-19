read_controlzone_data <- function(){
  control_zone_data <- read_csv(paste0(dir_path, "/edin_control_data.csv"))
  control_zone_data <- rename(control_zone_data,c("Longitude"="Longitude before","Latitude"="Latitude before")) 
  control_zone_data <- control_zone_data %>% filter(control_zone_data$Speed_limit %in% c(20,30)) 
  control_zone_data <- control_zone_data %>% mutate(year = format(as.Date(control_zone_data$Date, format="%m/%d/%Y"),"%Y"))
  return(control_zone_data)
}
