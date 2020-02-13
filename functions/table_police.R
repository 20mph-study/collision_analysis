table_police <- function(p,po){
  sum_pre_2 <-sum(p[p$Speed_limit == 20,2],p[p$Speed_limit == 30,2])
  sum_post_2 <-sum(po[po$Speed_limit == 20,2],po[po$Speed_limit == 30,2])
  
  collisions_pre_20mph <- c(p[p$Speed_limit == 20,2],p[p$Speed_limit == 30,2],sum_pre_2)
  collisions_post_20mph <- c(po[po$Speed_limit == 20,2],po[po$Speed_limit == 30,2],sum_post_2)
  
  rate_pre_20mph <- round(collisions_pre_20mph/3,2)
  rate_post_20mph <- round(collisions_post_20mph/1.3,2)
  
  diff_in_rates <-  rate_pre_20mph - rate_post_20mph
  percentage_diff_in_rates <- round(diff_in_rates/rate_pre_20mph,2)
  
  type <- c("Streets at 20mph","Streets at 30mph","All streets")
  df <- data.frame(type,collisions_pre_20mph,collisions_post_20mph,rate_pre_20mph,rate_post_20mph,diff_in_rates,percentage_diff_in_rates * 100)
  colnames(df)=c("Type", "collisions pre-20mph","collisions post-20mph","rate pre-20mph", "rate post-20mph","diff in rates", "Perc.diff.rates")
  
  kable(df,caption = "Table 2: Average annual road traffic collision rates in the city of Edinburgh (police)") %>% kable_styling(bootstrap_options = c("basic"))
}