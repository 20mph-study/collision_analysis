#Count accidents per zone
count_impl <- function(data_df){
  count1 <- aggregate(Accident_Index~ImplementationZone,data_df,length)
  print(count1)
  return(count1)
}