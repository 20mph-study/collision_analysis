read_belf_road <- function(dir_path){
  # Load data from the network drive for Belfast road network
  belfast_road_data <- readOGR(paste0(dir_path, '/20mph Speed Limit Streets/20mph_Speed_Limit_Streets.shp'), verbose = FALSE)
  # Tranform the data by applying a projection
  belfast_road_data <- spTransform(belfast_road_data, "+init=epsg:4326")

  return(belfast_road_data)
}
