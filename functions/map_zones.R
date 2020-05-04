map_zones <- function(edin_impl_zones){
  edin_impl_zones <- st_as_sf(edin_impl_zones)
  #Leaflet maps of the zones & accidents
  zone_1 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 1)
  zone_2 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 2)
  zone_3 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 3)
  zone_4 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 4)
  zone_5 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 5)
  zone_6 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 6)
  zone_7 <- filter(edin_impl_zones,edin_impl_zones$ImplementationZone == 7)
  
  #Visualize
  map_zones <- leaflet() %>% addTiles() %>%  
    addPolygons(data = zone_1, color= "green",group = " zone 1 ")%>%
    addPolygons(data = zone_2,color= "red",group = " zone 2 ")%>%
    addPolygons(data = zone_3,color= "pink",group = " zone 3 ") %>%
    addPolygons(data = zone_4,color= "cyan",group = " zone 4 ")%>%
    addPolygons(data = zone_5,color= "blue",group = " zone 5 ")%>%
    addPolygons(data = zone_6,color= "orange",group = " zone 6 ")%>%
    addPolygons(data = zone_7,color= "black",group = " zone 7 ") %>% 
    addLayersControl(overlayGroups = c( " zone 1 "," zone 2 "," zone 3 "," zone 4 "," zone 5 "," zone 6 "," zone 7 "),
                     options = layersControlOptions(collapsed = FALSE))
  map_zones <- map_zones %>% addLegend("bottomright",colors =c("green",  "red","pink", "cyan", "blue", "orange", "black"),
                                       labels= c("1(1a):City Centre", "2:North","3:South Central/East","4:North West","5:West", "6:South","7(1b):Rural West"),
                                       title= "Implementation zones", opacity = 1) %>% setView(lng = -3.188267 , lat = 55.953251, zoom = 11)
  return(map_zones)
}
