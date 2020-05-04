table_zones <- function(total_zones){
  zone_1a <- nrow(total_zones %>% filter(total_zones$Date >= "2013-07-31" & total_zones$Date <= "2016-07-30" & total_zones$ImplementationZone %in% c("1")))
  zone_1b <- nrow(total_zones %>% filter(total_zones$Date >= "2013-07-31" & total_zones$Date <= "2016-07-30" & total_zones$ImplementationZone %in% c("7")))
  zone_2 <- nrow(total_zones %>% filter(total_zones$Date >= "2014-02-28" & total_zones$Date <= "2017-02-27" & total_zones$ImplementationZone %in% c("2")))
  zone_3 <- nrow(total_zones %>% filter(total_zones$Date > "2014-02-28" & total_zones$Date <= "2017-02-27" & total_zones$ImplementationZone %in% c("3")))
  zone_4 <- nrow(total_zones %>% filter(total_zones$Date >= "2014-08-16" & total_zones$Date <= "2017-08-15" & total_zones$ImplementationZone %in% c("4")))
  zone_5 <- nrow(total_zones %>% filter(total_zones$Date >= "2014-08-16" & total_zones$Date <= "2017-08-15" & total_zones$ImplementationZone %in% c("5")))
  zone_6 <- nrow(total_zones %>% filter(total_zones$Date >= "2015-03-05" & total_zones$Date <= "2018-03-04" & total_zones$ImplementationZone %in% c("6")))
  
  all <- sum(zone_1a,zone_1b,zone_2,zone_3,zone_4,zone_5,zone_6 )
  
  zone_1a_p <- nrow(total_zones %>% filter(total_zones$Date > "2016-07-31"  & total_zones$ImplementationZone %in% c("1")))
  zone_1b_p <- nrow(total_zones %>% filter(total_zones$Date > "2016-07-31"  & total_zones$ImplementationZone %in% c("7")))
  zone_2_p <-  nrow(total_zones %>% filter(total_zones$Date > "2017-02-28"  & total_zones$ImplementationZone %in% c("2")))
  zone_3_p <-  nrow(total_zones %>% filter(total_zones$Date > "2017-02-28"  & total_zones$ImplementationZone %in% c("3")))
  zone_4_p <-  nrow(total_zones %>% filter(total_zones$Date > "2017-08-16"  & total_zones$ImplementationZone %in% c("4")))
  zone_5_p <-  nrow(total_zones %>% filter(total_zones$Date > "2017-08-16"  & total_zones$ImplementationZone %in% c("5")))
  zone_6_p <-  nrow(total_zones %>% filter(total_zones$Date > "2018-03-05"  & total_zones$ImplementationZone %in% c("6")))
  all_p <- sum(zone_1a_p,zone_1b_p,zone_2_p,zone_3_p,zone_4_p,zone_5_p,zone_6_p)
  
  collisions_pre_20mph <- c(zone_1a,zone_1b,zone_2,zone_3,zone_4,zone_5,zone_6,all)
  collisions_post_20mph <- c(zone_1a_p,zone_1b_p,zone_2_p,zone_3_p,zone_4_p,zone_5_p,zone_6_p,all_p)
  
  rate_pre_20mph <- round(collisions_pre_20mph/3,2)
  rate_post_20mph <- round(collisions_post_20mph/c(3.42,3.42,2.83,2.83,2.63,2.63,1.83),2)
  rate_post_20mph[8] <- sum(rate_post_20mph[1:7])
  
  diff_in_rates <-  rate_pre_20mph - rate_post_20mph
  percentage_diff_in_rates <- round(diff_in_rates/rate_pre_20mph,2)
  
  type <- c("1a","1b","2","3","4","5","6","All zones")
  df <- data.frame(type,collisions_pre_20mph,collisions_post_20mph,rate_pre_20mph,rate_post_20mph,diff_in_rates,percentage_diff_in_rates * 100)
  colnames(df)=c("Implementation zone", "collisions pre-20mph","collisions post-20mph","rate pre-20mph", "rate post-20mph","diff in rates", "Perc.diff.rates")
  
  kable(df,caption = "Table 3: Average annual road traffic collision rates in the city of Edinburgh per implementation zone") %>% kable_styling(bootstrap_options = c("striped", "hover"))
  
}
