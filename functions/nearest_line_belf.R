nearest_line_belf <- function(belf_data,belfast_road_data){
  source('../functions/localSnapPointsToLines.R')
  #Nearest line
  df <- belf_data
  road_net <- belfast_road_data
  
  #Create planar(cartesian) projection 
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  #Convert data to planar projection  
  road_net <- spTransform(road_net,crs)
  df <-  spTransform(SpatialPointsDataFrame(coords = df[25:26],proj4string = wgs84,data = df), crs)
  
  #Make sure our data have same projections
  proj4string(df) <- proj4string(road_net)
  
  #Find nearest line with maxDist=10m 
  nearest_line_sp <- localSnapPointsToLines(df,road_net,maxDist=12,withAttrs = T)
  
  #belfast_road_data@data <-rowid_to_column(belfast_road_data@data, "ID")
  nearest_line_sp <- spTransform(nearest_line_sp,"+init=epsg:4326")
  
  #Merge the dataframes based on the ID of the nearest line in order to connect geoinformation
  merged <- merge(data.frame(nearest_line_sp), belfast_road_data@data,by.x = "nearest_line_id" ,by.y = "ID")
  return(merged)
}
