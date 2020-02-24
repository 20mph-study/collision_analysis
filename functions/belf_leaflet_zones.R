belf_leaflet_zones <- function(belf_impl_out,total){
  leaflet(belf_impl_out) %>% addTiles() %>% addPolygons() %>%
    setView(lng = -5.926437 , lat = 54.607868, zoom = 14) %>% 
    addMarkers(lng = total$Longitude, lat = total$Latitude, 
               popup = paste("Accident index: ", total$a_ID, "<br>",
                             "Speed limit: ", total$a_speed, "<br>",
                             "Accident severity: " , total$a_type ))
}
