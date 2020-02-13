#Count accidents per layer
count_pre <- function(data_df){
  count1 <-  aggregate(Accident_Index~LAYER,data_df,length)
  return(count1)
}
