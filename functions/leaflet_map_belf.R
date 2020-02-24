leaflet_map_belf <- function(belfast_road_data,total){
  leaflet(belfast_road_data) %>% addTiles() %>%  addPolylines(group = 'roads') %>%
  setView(lng = -5.926437 , lat = 54.607868, zoom = 14) %>%
  addCircleMarkers(data = total,lng = total$X.1,lat=total$Y.1, group = 'incidents') %>% 
  addMarkers(lng = total$X, lat = total$Y,popup = paste("Accident index: ", total$a_ID, "<br>","Speed limit: ", total$a_speed ), group = 'incidents linked to a road') %>%
  addLayersControl(overlayGroups = c('roads', 'incidents','incidents linked to a road'),options = layersControlOptions(collapsed = FALSE))
}
