table_zones <- function(total_zones){
  s_pre <- total_zones[total_zones$Date >= "2013-07-31" & total_zones$Date <= "2016-07-30", ]
  zone_1a <- length(s_pre[s_pre$ImplementationZone%in%"1",1])
  zone_1b <- length(s_pre[s_pre$ImplementationZone%in%"7",1])
  
  s_pre <- total_zones[total_zones$Date >= "2014-02-28" & total_zones$Date <= "2017-02-27", ]
  zone_2 <- length(s_pre[s_pre$ImplementationZone%in%"2",1])
  zone_3 <- length(s_pre[s_pre$ImplementationZone%in%"3",1])
  
  s_pre <- total_zones[total_zones$Date >= "2014-08-16" & total_zones$Date <= "2017-08-15", ]
  zone_4 <- length(s_pre[s_pre$ImplementationZone%in%"4",1])
  zone_5 <- length(s_pre[s_pre$ImplementationZone%in%"5",1])
  
  s_pre6 <-  total_zones[total_zones$Date >= "2015-03-05" & total_zones$Date <= "2018-03-04", ]
  zone_6 <- length(s_pre6[s_pre6$ImplementationZone%in%"6",1])
  
  all <- sum(zone_1a,zone_1b,zone_2,zone_3,zone_4,zone_5,zone_6 )
  
  s_post <-  total_zones[total_zones$Date > "2016-07-31" & total_zones$Date <= "2018-12-31", ]
  
  zone_1a_p <- length(s_post[s_post$ImplementationZone=="1",1])
  zone_1b_p <- length(s_post[s_post$ImplementationZone=="7",1])
  
  s_post <-  total_zones[total_zones$Date > "2017-02-28" & total_zones$Date <= "2018-12-31", ]
  zone_2_p <- length(s_post[s_post$ImplementationZone=="2",1])
  zone_3_p <- length(s_post[s_post$ImplementationZone=="3",1])
  
  s_post <-  total_zones[total_zones$Date > "2017-08-16" & total_zones$Date <= "2018-12-31", ]
  zone_4_p <- length(s_post[s_post$ImplementationZone=="4",1])
  zone_5_p <- length(s_post[s_post$ImplementationZone=="5",1])
  
  s_post <-  total_zones[total_zones$Date > "2018-03-05" & total_zones$Date <= "2018-12-31", ]
  zone_6_p <- length(s_post[s_post$ImplementationZone=="6",1])
  all_p <- sum(zone_1a_p,zone_1b_p,zone_3_p,zone_4_p,zone_5_p,zone_6_p)
  
  collisions_pre_20mph <- c(zone_1a,zone_1b,zone_2,zone_3,zone_4,zone_5,zone_6,all)
  collisions_post_20mph <- c(zone_1a_p,zone_1b_p,zone_2_p,zone_3_p,zone_4_p,zone_5_p,zone_6_p,all_p)
  
  rate_pre_20mph <- round(collisions_pre_20mph/3,2)
  rate_post_20mph <- round(collisions_post_20mph/c(2.6,2.6,2,2,1.5,1.5,0.9),2)
  
  diff_in_rates <-  rate_pre_20mph - rate_post_20mph
  percentage_diff_in_rates <- round(diff_in_rates/rate_pre_20mph,2)
  
  type <- c("1a","1b","2","3","4","5","6","All zones")
  df <- data.frame(type,collisions_pre_20mph,collisions_post_20mph,rate_pre_20mph,rate_post_20mph,diff_in_rates,percentage_diff_in_rates * 100)
  colnames(df)=c("Implementation zone", "collisions pre-20mph","collisions post-20mph","rate pre-20mph", "rate post-20mph","diff in rates", "Perc.diff.rates")
  
  kable(df,caption = "Table 3: Average annual road traffic collision rates in the city of Edinburgh per implementation zone") %>% kable_styling(bootstrap_options = c("striped", "hover"))
  
}
