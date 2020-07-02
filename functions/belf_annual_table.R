belf_annual_table <- function(total_zones,total,pre,post){
  source('../functions/belcount_pre.R')
  
  not_changed <- delete_na(total_zones[!(total_zones$a_ID %in% total$a_ID),],"a_ID")
  
  sum1 <- c(nrow(total_zones[total_zones$a_date >= "2013-02-01" & total_zones$a_date <= "2016-01-31",]))
  sum2 <- c(nrow(total_zones[total_zones$a_date >= "2016-02-02" & total_zones$a_date <= "2019-12-31",]))
  
  #belcount_pre
  not_changed_pre <-  c(nrow(not_changed[not_changed$a_date >= "2013-02-01" & not_changed$a_date <= "2016-01-31",]))
  not_changed_post <- c(nrow(not_changed[not_changed$a_date >= "2016-02-02" & not_changed$a_date <= "2019-12-31",]))
  
  changed_pre <- sum1 - not_changed_pre
  changed_post <- sum2 - not_changed_post

  collisions_pre_20mph <- c(changed_pre, not_changed_pre, sum1)
  collisions_post_20mph <- c(changed_post, not_changed_post, sum2)
  
  rate_pre_20mph <- round(collisions_pre_20mph / 3, 2)
  rate_post_20mph <- round(collisions_post_20mph / 3.92, 2)
  
  diff_in_rates <-  rate_pre_20mph - rate_post_20mph
  percentage_diff_in_rates <- round(diff_in_rates / rate_pre_20mph, 2)
  
  type <- c("Streets changed to 20mph","20mph, 30mph","All streets")
  df <- data.frame(type, collisions_pre_20mph, collisions_post_20mph, rate_pre_20mph, rate_post_20mph, diff_in_rates, percentage_diff_in_rates * 100)
  colnames(df)=c("Type", "collisions pre-20mph (2013 / 2016 - 3 years)", "collisions post-20mph (2016 / 2019 - 3years & 11 months)", "rate pre-20mph", "rate post-20mph","diff in rates = pre - post", "Perc.diff.rates = (pre - post) / pre * 100")
  kable(df,caption = "Table 1: Average annual road traffic collision rates in the city of Belfast (PSNI data)") %>% kable_styling(bootstrap_options = c("striped","hover")) %>%
    footnote(general = " ", number =c("All accidents at 20mph, 30mph")) %>% 
    pack_rows("Streets stayed", 2, 2,label_row_css = "background-color: #e5f5e0 ;") %>% 
    pack_rows("Streets changed", 1, 1,label_row_css = "background-color: #e5f5e0;") %>% 
    pack_rows("All streets", 3, 3,label_row_css = "background-color:#e5f5e0 ;")
}
