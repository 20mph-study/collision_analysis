read_bel_data <- function(dir_path){
  belf_19 <- read_excel(paste0(dir_path,"/Collisions_Apr-Dec2019.xlsx"))
  coord <-belf_19 %>% st_as_sf(coords = c("a_gd1","a_gd2"), crs = 29903) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble()
  belf_19 <- data.frame(belf_19,coord)
  belf_19 <- belf_19 %>% filter(belf_19$a_speed %in% c(20,30)) 
  belf_19 <- belf_19 %>% filter(belf_19$LGDNAME == "Belfast City") 
  
  belf_data <- read_csv( paste0(dir_path, "/Belfast/Belfast_data.csv"))
  
  belfast <- rbind.fill(belf_19,belf_data)
  belfast <- delete_na(belfast,c("X","Y"))
  belfast <- belfast %>% mutate(year = format(as.Date(belfast$a_date, format="%m/%d/%Y"),"%Y"))
  
  
  return(belfast)
}
