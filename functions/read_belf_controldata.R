read_belf_controldata <- function(dir_path){
  belf_control_data <- read_csv( paste0(dir_path, "/Belfast/Belfast_control_data.csv"))
  belf_control_data <- delete_na(belf_control_data,c("X","Y"))
  belf_control_data <- belf_control_data %>% filter(belf_control_data$a_speed %in% c(20,30)) 
  belf_control_data <- belf_control_data %>% mutate(year = format(as.Date(belf_control_data$a_date, format="%m/%d/%Y"),"%Y"))
  return(belf_control_data)
}