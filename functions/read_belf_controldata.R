read_belf_controldata <- function(dir_path){
  
  belf_19 <- read_excel(paste0(dir_path,"/Collisions_Apr-Dec2019.xlsx"))
  coord <-belf_19 %>% st_as_sf(coords = c("a_gd1","a_gd2"), crs = 29903) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  belf_19 <- data.frame(belf_19,coord)
  belf_19 <- belf_19 %>% filter(belf_19$LGDNAME == "Ards & North Down")
  
  
  belf_control <- read_csv( paste0(dir_path, "/Belfast/Belfast_control_data.csv"))
  
  belf_control_data <- rbind.fill(belf_19,belf_control)
  
  belf_control_data <- delete_na(belf_control_data,c("X","Y"))
  belf_control_data <- belf_control_data %>% filter(belf_control_data$a_speed %in% c(20,30)) 
  belf_control_data <- belf_control_data %>% mutate(year = format(as.Date(belf_control_data$a_date, format="%m/%d/%Y"),"%Y"))
  return(belf_control_data)
}
