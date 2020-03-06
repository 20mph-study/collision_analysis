#Function for the nearest line
nearest_line <-function(df,road_net,tolerance){
  #Create planar(cartesian) projection 
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  #Convert data to planar projection  
  road_net <- spTransform(road_net,crs)
  coord <- data.frame(df$Longitude,df$Latitude)
  df <-  spTransform(SpatialPointsDataFrame(coords = coord,proj4string = wgs84,data = df), crs)
  
  #Make sure our data have same projections
  proj4string(df) <- proj4string(road_net)
  
  #Find nearest line with maxDist=6m 
  nearest_line_sp <- localSnapPointsToLines(df,road_net,maxDist = tolerance,withAttrs=TRUE)
  
  return(nearest_line_sp)
}
