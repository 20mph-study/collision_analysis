buffer_data <-function(data,zone){
  #Create planar(cartesian) projection 
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  zone <- spTransform(zone,crs)
  data <-  spTransform(SpatialPointsDataFrame(coords = data[4:5],proj4string = wgs84,data = data), crs)
  
  proj4string(data) <- proj4string(zone)
  counts2 <- raster::intersect(data,zone) 
  return(counts2@data)
}