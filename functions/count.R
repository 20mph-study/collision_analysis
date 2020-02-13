#Count function
count <- function(data_df){
  count1 <- aggregate(Accident_Index~LAYER,data_df,length)
  count2 <- aggregate(Accident_Index~Speed_limit,data_df,length)
  print(count1)
  print(count2)
}
