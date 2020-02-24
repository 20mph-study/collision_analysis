read_belf_impl_zones <- function(dir_path){
  gdb_path_out <- paste0(dir_path, "/OA_ni-ESRI_format/OA_ni.shp")
  gdb_layers <- ogrListLayers(gdb_path_out)
  
  belf_impl_zones <- readOGR(dsn = gdb_path_out, layer="OA_ni", verbose = FALSE)
  belf_impl_zones <- spTransform(belf_impl_zones, "+init=epsg:4326")
  belf_impl_zones_out <- belf_impl_zones[belf_impl_zones@data$OA_CODE %in% c("95GG390015","95GG200003","95GG350003","95GG350004",
                                                                             "95GG390013","95GG350001","95GG210002","95GG400007",
                                                                             "95GG390012","95GG390011","95GG350005"),]
  return(belf_impl_zones_out)
}
