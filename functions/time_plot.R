library(cowplot)
time_plot <- function(edin_road_data){
  a <- ggplot(edin_road_data) +
    aes(x = Speed_limit) +
    geom_histogram(bins = 30L, fill = "#a6d96a") +
    theme_minimal() +
    facet_wrap(vars(year)) 
  
  b <- ggplot(edin_road_data) +
    aes(x = Date, colour = Speed_limit) +
    geom_histogram(bins = 30L, fill = "#a6d96a") +
    scale_color_distiller(palette = "Greens") +
    theme_minimal() 
  
  c <- ggplot(edin_road_data) +
    aes(x = Date, weight = Speed_limit) +
    geom_bar(fill = "#1a9850") +
    theme_minimal()
  
  plot_grid(a,b,c,labels = "AUTO")
}