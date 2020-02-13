#Count accidents per speed limit
count_post <- function(data_df){
  count1 <-  aggregate(Accident_Index~Speed_limit,data_df,length)
  return(count1)
}
