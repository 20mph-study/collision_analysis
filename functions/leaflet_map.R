#Function for leaflet visualisations
leaflet_map <- function(final_df,road_df,edin_streets){
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  #Convert data to planar projection  
  edin_streets <- spTransform(edin_streets,crs)
  coordinates(road_df) <- ~ Longitude + Latitude
  coordinates(final_df) <- ~ X + Y
  
  #Convert data projections back to lat/long to plot with leaflet
  edin_streets <- spTransform(edin_streets,"+init=epsg:4326")
  
  #Same projections
  proj4string(final_df) <- proj4string(edin_streets)
  proj4string(road_df) <- proj4string(edin_streets)
  
  edin_streets <- st_as_sf(edin_streets)
  #Leaflet maps of the road network , accidents and their projections
  road_existing_20 <-edin_streets%>% filter(edin_streets$LAYER == "20mph existing streets")
  road_local_20 <- filter(edin_streets, edin_streets$LAYER == "20mph local streets")
  road_main_20 <- filter(edin_streets, edin_streets$LAYER == "20mph main streets")
  road_30 <- filter(edin_streets, edin_streets$LAYER == "30mph")
  road_40 <- filter(edin_streets, edin_streets$LAYER == "40mph")
  road_part <- filter(edin_streets, edin_streets$LAYER == "Part time 20mph")
  road_50 <- filter(edin_streets, edin_streets$LAYER == "50, 60 or 70mph")
  road_trunk <- filter(edin_streets, edin_streets$LAYER == "Trunk roads")
  
  
  #Visualize
  map <- leaflet() %>% addTiles() %>% addPolygons(data = edin_streets)
  #map <- map %>%  addLegend("bottomright",colors =c("green",  "red", "yellow", "cyan", "blue","black","purple","orange"),
  #labels= c("existing 20mph", "local 20mph","main 20 mph","30mph","40mph", "part time 20mph","50,60 or 70mph","Trunk roads"),
  #title= "Road network",opacity = 1)
  map_near <-map %>% leaflet() %>% addTiles() %>% addMarkers(lng =final_df$X, lat = final_df$Y,
                                                             popup =paste("Accident index: ", final_df$Accident_Index, "<br>",
                                                                          "Speed limit: ", final_df$Speed_limit ,"<br>",
                                                                          "Layer: ",final_df$LAYER,"<br>",
                                                                          "Casualty severity: ",final_df$Casualty_Severity))
  road_df <- (data.frame(road_df))
  map3 <- map_near %>% addCircleMarkers(lng =road_df$Longitude , lat = road_df$Latitude,
                                        popup =paste("Accident index: ", road_df$Accident_Index, "<br>",
                                                     "Speed limit: ", road_df$Speed_limit ,"<br>",
                                                     "Casualty severity: ",road_df$Casualty_Severity ))
  

  map3 <- map3 %>% addLegend("bottomright",colors =c("green",  "red", "yellow", "cyan", "blue","black","purple","orange"),
                      labels= c("existing 20mph", "local 20mph","main 20 mph","30mph","40mph", "part time 20mph","50,60 or 70mph","Trunk roads"),
                      title= "Road network",opacity = 1)
  
  map3 %>%
  setView(lng = -3.188267 , lat = 55.953251, zoom = 11)%>%
    addPolylines(data = road_existing_20, color= "green")%>%
    addPolylines(data = road_local_20,color= "red")%>%
    addPolylines(data = road_main_20,color= "yellow")%>%
    addPolylines(data = road_30,color= "cyan")%>%
    addPolylines(data = road_40,color= "blue")%>%
    addPolylines(data = road_part,color= "black")%>%
    addPolylines(data = road_50,color= "purple")%>%
    addPolylines(data = road_trunk,color= "orange")
}
