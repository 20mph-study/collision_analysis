read_belfcontrol_zone <- function(dir_path){
  gdb_path_out <- paste0(dir_path, "/OA_ni-ESRI_format/OA_ni.shp")
  gdb_layers <- ogrListLayers(gdb_path_out)
  
  belf_impl_zones <- readOGR(dsn = gdb_path_out, layer="OA_ni", verbose = FALSE)
  belf_impl_zones <- spTransform(belf_impl_zones, "+init=epsg:4326")
  
  belf_control_zones <- belf_impl_zones[belf_impl_zones@data$OA_CODE %in% c("95BB060001","95BB060002","95BB060003","95BB060004","95BB060005","95BB060006",
                                                                          "95BB060007","95BB060008","95BB060009","95BB060010","95BB060011"),]
  
  return(belf_control_zones)
}
