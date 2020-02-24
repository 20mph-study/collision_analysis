time_plot_belf <- function(belf_data){
  library(cowplot)
  a <- ggplot(belf_data) +
    aes(x = a_speed) +
    geom_histogram(bins = 30L, fill = "#a6d96a") +
    theme_minimal() +
    facet_wrap(vars(year))
  
  b <- ggplot(belf_data) +
    aes(x = a_date, colour = a_speed) +
    geom_histogram(bins = 30L, fill = "#a6d96a") +
    scale_color_distiller(palette = "Greens") +
    theme_minimal()
  
  c <- ggplot(belf_data) +
    aes(x = a_date, weight = a_speed) +
    geom_bar(fill = "#1a9850") +
    theme_minimal()
  
  plot_grid(a,b,c,labels = "AUTO")
}
