read_bel_data <- function(dir_path){
  belf_data <- read_csv( paste0(dir_path, "/Belfast/Belfast_data.csv"))
  belf_data <- delete_na(belf_data,c("X","Y"))
  belf_data <- belf_data %>% mutate(year = format(as.Date(belf_data$a_date, format="%m/%d/%Y"),"%Y"))
  belf_data <- belf_data %>% filter(belf_data$a_speed %in% c(20,30)) 
  return(belf_data)
}