belf_annual_table <- function(belf_data,pre,post){
  source('../functions/belcount_pre.R')
  
  sum_pre <- pre[pre$a_speed == 20,2] + pre[pre$a_speed == 30,2] 
  sum_post <- post[post$a_speed == 20,2] + post[post$a_speed == 30,2] 
  
  sum1 <- c(nrow(belf_data[belf_data$year >= "2013" & belf_data$year <= "2016",]))
  sum2 <- c(nrow(belf_data[belf_data$year >= "2018" & belf_data$year <= "2019",]))
  
  #belcount_pre
  not_changed <- delete_na(belf_data[!(belf_data$a_ID %in% merged$a_ID),],"a_ID")
  pre_not_changed <- belcount_pre(not_changed[not_changed$year >= "2013" & not_changed$year <= "2016",] )
  post_not_changed <- belcount_pre(not_changed[not_changed$year >= "2018" & not_changed$year <= "2019",] )
  
  pre_not_changed_20 <- pre_not_changed[1,2] 
  pre_not_changed_30 <- pre_not_changed[2,2] 
  
  post_not_changed_20 <-  post_not_changed[1,2]
  post_not_changed_30 <-  post_not_changed[2,2]
  
  collisions_pre_20mph <- c(sum_pre, pre_not_changed_20 ,pre_not_changed_30 , sum1)
  collisions_post_20mph <- c(sum_post, post_not_changed_20,post_not_changed_30, sum2)
  
  rate_pre_20mph <- round(collisions_pre_20mph / 3, 2)
  rate_post_20mph <- round(collisions_post_20mph / 1.25, 2)
  
  diff_in_rates <-  rate_pre_20mph - rate_post_20mph
  percentage_diff_in_rates <- round(diff_in_rates / rate_pre_20mph * 100, 2)
  
  type <- c("Streets changed to 20mph","at 20mph ","at 30mph","All streets")
  df <- data.frame(type, collisions_pre_20mph, collisions_post_20mph, rate_pre_20mph, rate_post_20mph, diff_in_rates, percentage_diff_in_rates)
  colnames(df)=c("Type", "collisions pre-20mph (2013 / 2016 - 3 years)", "collisions post-20mph (2018 / 2019 - 13 months)", "rate pre-20mph", "rate post-20mph","diff in     rates = pre - post", "Perc.diff.rates = (pre - post) / pre * 100")
  kable(df,caption = "Table 1: Average annual road traffic collision rates in the city of Belfast (PSNI data)") %>% kable_styling(bootstrap_options = c("striped",      
                                                                                                                                                        "hover")) %>%
    footnote(general = " ", number =c("All accidents at 20mph, 30mph")) %>% 
    pack_rows("Streets stayed", 2, 3,label_row_css = "background-color: #e5f5e0 ;") %>% 
    pack_rows("Streets changed", 1, 1,label_row_css = "background-color: #e5f5e0;") %>% 
    pack_rows("All streets", 4, 4,label_row_css = "background-color:#e5f5e0 ;")
}