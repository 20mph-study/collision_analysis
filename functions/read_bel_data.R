read_bel_data <- function(dir_path){

  filename <- paste0(dir_path, "/Belfast/Collisions 1998-2017.xls")
  bel <- read_excel(filename)
  belf_data <- read_excel(filename, sheet = 2)
  colnames(belf_data) <- colnames(bel)
  belf_data$a_date <- as.Date(belf_data$a_date,format="%m/%d/%Y")
  belf_data <- belf_data[belf_data$a_date >="2013-02-01" & belf_data$a_date <= "2017-12-31", ] %>% filter(!is.na(a_date))
  
  #Belfast data 2018
  belf_2018 <-  read_csv(paste0(dir_path, "/Belfast/Collisions Jan2018 Mar2019.csv"))
  belf_2018$a_date <- as.Date(belf_2018$a_date,format="%m/%d/%Y")
  belf_2018$x <- as.POSIXct(belf_2018$a_time, origin="1970-01-01")
  belf_2018$time_bef_format <- belf_2018$a_time
  belf_2018$a_time <- format(belf_2018$x, format="%H:%M")
  belf_2018 <- belf_2018[,-c(25,26)]
  
  belf_data <- rbind(belf_data,belf_2018)
  belf_data <- belf_data %>% filter(belf_data$a_speed %in% c(20,30)) 
  belf_data <- belf_data %>% filter(belf_data$LGDNAME == "Belfast City") 
  
  belf_19 <- read_excel(paste0(dir_path,"/Belfast/Collisions_Apr-Dec2019.xlsx"))
  belf_19$a_date <- as.Date(belf_19$a_date,format="%m/%d/%Y")
  belf_19 <- belf_19 %>% filter(belf_19$a_speed %in% c(20,30)) 
  belf_19 <- belf_19 %>% filter(belf_19$LGDNAME == "Belfast City")  

  
  belfast <- rbind.fill(belf_19,belf_data)
  
  coord <- belfast %>% st_as_sf(coords = c("a_gd1","a_gd2"), crs = 29903) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  belfast <- data.frame(belfast,coord)
  
  belfast <- delete_na(belfast,c("X","Y"))
  belfast <- belfast %>% mutate(year = format(as.Date(belfast$a_date, format="%m/%d/%Y"),"%Y"))
  
  
  return(belfast)
}
