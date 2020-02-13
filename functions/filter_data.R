filter_data <- function(data){
  data <- data %>% filter(data$Speed_limit %in% c(20,30,40)) 
  data <- data %>% filter(data$`Local_Authority_(District)` == 923)
  #data$Date <- as.Date(data$Date,format="%d/%m/%Y")
  data <- na.omit(data[1:31])#remove NA obs
  return(data)
}