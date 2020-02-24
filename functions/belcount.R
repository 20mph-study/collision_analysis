belcount <- function(data_df){
  count2 <- aggregate(a_ID~a_speed,data_df,length)
  return(count2)
}
