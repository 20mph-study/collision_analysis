table_annual <- function(pre,post){
  #count streets that have changed from 30 to 20mph
  s1 <- pre[pre$LAYER == "20mph main streets",2] + pre[pre$LAYER == "20mph local streets",2]
  s2 <- post[post$LAYER == "20mph main streets",2] + post[post$LAYER == "20mph local streets",2]
  
  #sum all streets pre and post
  sum_pre <-sum(pre[pre$LAYER == "30mph",2],pre[pre$LAYER == "Trunk roads",2],pre[pre$LAYER == "20mph existing streets",2],pre[pre$LAYER == "Part time 20mph",2],s1)
  sum_post <-sum(post[post$LAYER == "30mph",2],post[post$LAYER == "20mph existing streets",2],post[post$LAYER == "Part time 20mph",2],s2)
  
  #create column pre and post
  collisions_pre_20mph <- c(pre[pre$LAYER == "30mph",2]+pre[pre$LAYER == "Trunk roads",2],pre[pre$LAYER == "20mph existing streets",2],pre[pre$LAYER == "Part time 20mph",2],s1,sum_pre)
  collisions_post_20mph <- c(post[post$LAYER == "30mph",2],post[post$LAYER == "20mph existing streets",2],post[post$LAYER == "Part time 20mph",2],s2,sum_post)
  
  #rate per year
  rate_pre_20mph <- round(collisions_pre_20mph/3,2)
  rate_post_20mph <- round(collisions_post_20mph/1.83,2)
  
  diff_in_rates <-  rate_pre_20mph - rate_post_20mph
  percentage_diff_in_rates <- round(diff_in_rates/rate_pre_20mph,2) 
  percentage_diff_in_rates <- percentage_diff_in_rates *100
  
  
  type <- c("at 30mph","at 20mph","Part time 20mph : a traffic calming measure in specific hours per day. 
                                       e.g outside of schools, at the start and end of the school day","at 30mph changed to 20mph","All streets")
  df <- data.frame(type,collisions_pre_20mph,collisions_post_20mph,rate_pre_20mph,rate_post_20mph,diff_in_rates,percentage_diff_in_rates)
  colnames(df)=c(" ", "collisions pre-20mph ( 2013 / 2016 - 3 years )","collisions post-20mph ( 2018 / 2019 - 22 months )","rate pre-20mph ",
                 "rate post-20mph ","diff in rates = pre - post", "Perc.diff.rates = (pre - post) / pre * 100")
  
  kable(df,caption = "Table 1: Average annual road traffic collision rates in the city of Edinburgh (council data)") %>% 
    kable_styling(bootstrap_options = c("basic","hover"),font_size = 11 ) %>%  
    footnote(general = " ", number =c("All accidents at 20mph, 30mph")) %>% 
    pack_rows("Streets stayed", 1, 3,label_row_css = "background-color: #e5f5e0 ;") %>% 
    pack_rows("Streets changed", 4, 4,label_row_css = "background-color: #e5f5e0;") %>% 
    pack_rows("All streets", 5, 5,label_row_css = "background-color:#e5f5e0 ;")
  #e5f5e0
}
 