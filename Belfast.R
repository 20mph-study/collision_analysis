# Load libraries
library(rgdal)
library(leaflet)
library(tidyverse)
library(rgeos)
library(raster)
library(ggplot2)
library(sf)
library(mapview)
library(maptools)
library(sp)
library(readxl)
library(readr)

#1.Function for keeping data of Edinburgh with speed limits 20/30/40
filter_data <- function(data){
  data <- data %>% filter(data$Speed_limit %in% c(20,30,40)) 
  data <- data %>% filter(data$LGDNAME == "Belfast City")
  #data$Date <- as.Date(data$Date,format="%d/%m/%Y")
  #data <- na.omit(data[1:31])#remove NA obs
  return(data)
}

delete_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#3.Function for the nearest line
nearest_line <-function(df,road_net){
  #Create planar(cartesian) projection 
  crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
  wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat
  
  #Convert data to planar projection  
  road_net <- spTransform(road_net,crs)
  df <-  spTransform(SpatialPointsDataFrame(coords = ~ Latitude + Longitude, proj4string = wgs84,data = df), crs)
  
  #Make sure our data have same projections
  proj4string(df) <- proj4string(road_net)
  
  #Find nearest line with maxDist=20m 
  nearest_line_sp <- snapPointsToLines(df,road_net,maxDist=20)
  
  return(nearest_line_sp)
}

convert_latlong <-function(rd){
  # Load libraries
  library(rgdal)
  library(tidyverse)
  library(readxl)
  library(leaflet)
  
  # Let's rename columns to latitude and longitude
  rd <- rd %>% rename("Latitude" = "a_gd1", "Longitude" = "a_gd2")
  
  # Remove rows without valid coordinates
  rd <- filter(rd, Latitude != "" | Longitude != "")
  
  # Create a coords df with Northings and Eastings (although they've been renamed as latlng)
  coords <- cbind(Longitude = as.numeric(as.character(rd$Longitude)),Latitude = as.numeric(as.character(rd$Latitude)))
  
  # Create a spatialdataframe
  rd <- SpatialPointsDataFrame(coords, data = rd, proj4string = CRS("+init=epsg:27700"))
  
  # Create CRS for latlng
  latlong <- "+init=epsg:4326"
  
  # Convert Northing/Easting to Lat/Lng
  rd <- spTransform(rd, latlong)
  return(rd)
}

#-----------------------------------------------------------------------------------------------------------------------------------------------

#2.Gis 
#Read geodatabase for Belfast

#Road data 
# Load data from the network drive for Belfast road network
belfast_road_data <- readOGR('V:Studies\\MOVED\\HealthImpact\\data\\20mph study collisions\\20mph Speed Limit Streets\\20mph_Speed_Limit_Streets.shp')
# Tranform the data by applying a projection
belfast_road_data <- spTransform(belfast_road_data, "+init=epsg:4326")
# Visualize using leaflet
leaflet(belfast_road_data) %>% addTiles() %>% addPolygons() 

#Output Areas 
gdb_path_out <- paste0(dir_path, "20mph study collisions\\belf out")
gdb_layers <- ogrListLayers(gdb_path_out)
print(gdb_layers)

belf_impl_zones_out <- readOGR(dsn = gdb_path_out, layer="OA_ni")
#leaflet(belf_impl_zones) %>% addTiles() %>% addPolygons()
belf_impl_zones_out <- spTransform(belf_impl_zones_out, "+init=epsg:4326")
inter <- intersect(belfast_road_data, belf_impl_zones_out)
control_zones_out <- unique(inter@data$OA_CODE)
print(control_zones_out)
leaflet(inter) %>% addTiles()

belf_impl_zones_out <- belf_impl_zones_out[belf_impl_zones_out@data$OA_CODE %in% c("95GG390015","95GG200003","95GG350003","95GG350004",
                                                                                   "95GG390013","95GG350001","95GG210002","95GG400007",
                                                                                   "95GG390012","95GG290015","95GG390011","95SS100003",
                                                                                   "95SS100001","95GG350005","95GG040001","95GG040007"),]

belf_impl_zones_out <- spTransform(belf_impl_zones_out, "+init=epsg:4326")
leaflet(belf_impl_zones_out) %>% addTiles() %>% addPolygons()

dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"
gdb_path_small <- paste0(dir_path, "20mph study collisions\\Belfast Small Areas")
gdb_layers <- ogrListLayers(gdb_path_small)

belf_impl_zones_small <- readOGR(dsn = gdb_path_small, layer="SA2011")
belf_impl_zones_small <- spTransform(belf_impl_zones_small, "+init=epsg:4326")
inter_small <- intersect(belfast_road_data, belf_impl_zones_small)
small_control_zones <- unique(inter_small@data$SA2011)
#print(small_control_zones)
#leaflet(inter_small) %>% addTiles() %>% addPolygons(popup = inter_small$SA2011)

belf_impl_zones_small <- belf_impl_zones_small[belf_impl_zones_small$SA2011 %in% c("N00001416", "N00001131", "N00001351", "N00001352",
                                                                                   "N00001418","N00001350", "N00001155", "N00001439", 
                                                                                   "N00001417","N00001273"),]

belf_impl_zones_small <- spTransform(belf_impl_zones_small, "+init=epsg:4326")
#belf_impl_zones_small <- st_as_sf(belf_impl_zones_try)
leaflet(belf_impl_zones_small) %>% addTiles() %>% addPolygons()

#Read PSNI data
#Belfast data 
filename <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\Belfast\\Collisions 1998-2017.xls"
bel <- read_excel(filename)
belf_data <- read_excel(filename, sheet = 2)
colnames(belf_data) <- colnames(bel)

belf_data <- belf_data %>% filter(belf_data$LGDNAME == "Belfast City") 
belf_data <- belf_data %>% filter(belf_data$a_speed %in% c(20,30,40)) 
belf_data$a_date <- as.Date(belf_data$a_date,format="%d/%m/%Y")
belf_data <- belf_data[belf_data$a_date >="2013-01-01" & belf_data$a_date <= "2015-12-31", ] %>% filter(!is.na(a_date))
belf_data <- delete_na(belf_data,c("a_gd1","a_gd2"))

#Create new csv file with the cleaned data
write.csv(belf_data, file ="V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\collisions\\Belf_data.csv",row.names=FALSE)

dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"

#Read filtered data for Belfast 20/30/40mph
belf_road_data <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\Belf_data.csv"))

#belf_data_converted <- convert_latlong(belf_road_data)

rd <- belf_road_data
#------------------------------Convert to long/lat--------------------------------------------------------------

# Load libraries
library(rgdal)
library(tidyverse)
library(readxl)
library(leaflet)

# Source - or guide 
# http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf

# Let's rename columns to latitude and longitude
rd <- rd %>% rename("Latitude" = "a_gd1", "Longitude" = "a_gd2")

# Remove rows without valid coordinates
rd <- filter(rd, Latitude != "" | Longitude != "")

# Create a coords df with Northings and Eastings (although they've been renamed as latlng)
coords <- cbind(Latitude = as.numeric(as.character(rd$Latitude)), Longitude = as.numeric(as.character(rd$Longitude)))

# Create a spatialdataframe
rd <- SpatialPointsDataFrame(coords, data = rd, proj4string = CRS("+init=epsg:27700"))

# Create CRS for latlng
latlong <- "+init=epsg:4326"

# Convert Northing/Easting to Lat/Lng
rd <- spTransform(rd, latlong)


rd <- st_as_sf(rd)
# Plot
 leaflet(rd) %>% addTiles() 
# Setview to city of Edinburgh

# Add circles
m %>% addCircles(weight = 3, radius=40, color="#ffa500", stroke = TRUE, fillOpacity = 0.8)
















#-------------------NEAREST LINE-------------------------------

#nearest line
crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat

#Convert data to planar projection  
bel_rd <- belfast_road_data
bel_rd <- spTransform(bel_rd,crs)
df <-  spTransform(SpatialPointsDataFrame(coords = belf_data_converted@coords,proj4string = wgs84,data = belf_data_converted@data), crs)

#Make sure our data have same projections
proj4string(df) <- proj4string(bel_rd)

#Find nearest line with maxDist=20m 
nearest_line_bel <- snapPointsToLines(df,bel_rd,maxDist=20)

nearest_line_bel <- spTransform(nearest_line_bel,"+init=epsg:4326")
edin_streets <- spTransform(edin_streets,"+init=epsg:4326")





#------------------------TRANSFORM TO LAT/LONG-----------------------------------------------------

ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

### Create coordinates variable
coords <- cbind(Easting = as.numeric(as.character(dat$a_gd1)),
                Northing = as.numeric(as.character(dat$a_gd2)))

### Create the SpatialPointsDataFrame
dat_SP <- SpatialPointsDataFrame(coords,
                                 data = dat,
                                 proj4string = CRS("+init=epsg:27700"))

### Convert
dat_SP_LL <- spTransform(dat_SP, CRS(latlong))


#--------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------

proj4string(belf) <- proj4string(belf_impl_zones)


y%>% leaflet(belf_impl_zones) %>% addTiles() %>% addPolylines(popup = belf_impl_zones$SA2011)
y %>% z

belf_impl_zones_try <- belf_impl_zones[belf_impl_zones@data$SA2011 %in% c("N00000176","N00000177","N00000178","N00000179","N00000180","N00000181"),]
belf_impl_zones_try <- spTransform(belf_impl_zones_try, "+init=epsg:4326")
belf_impl_zones_try <- st_as_sf(belf_impl_zones_try)

leaflet(belf_impl_zones_try) %>% addTiles() %>% addPolylines(popup = ) 

gdb_path <- paste0(dir_path, "20mph study collisions\\Belfast Output Areas")
gdb_layers <- ogrListLayers(gdb_path)
print(gdb_layers)
belf_impl_zones <- readOGR(dsn = gdb_path, layer="SA2011")
belf_cons_streets <- readOGR(dsn = gdb_path,layer="ESRI Shapefile")
belf_impl_zones <- spTransform(belf_impl_zones, "+init=epsg:4326")

library(raster)
s <- shapefile("V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\Belfast Output Areas\\SOA2011.shp")
s <- spTransform(s, "+init=epsg:4326")

leaflet(s) %>% addTiles() %>% addPolygons()

ss <- shapefile("V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\output borders\\SOA2011.shp")
ss <- spTransform(ss, "+init=epsg:4326")

gdb_path <- paste0(dir_path, "20mph study collisions\\Belf shapefile")
gdb_layers <- ogrListLayers(gdb_path)
print(gdb_layers)
belf_impl_zones <- readOGR(dsn = gdb_path, layer="FinalRecommendations")


#---------------------------------------------------------------------------------------------------------------------


filename <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\Belfast\\Collisions 1998-2017.xls"
bel <- read_excel(filename)
belf_data <- read_excel(filename, sheet = 2)
colnames(belf_data) <- colnames(bel)

belf_data <- belf_data %>% filter(belf_data$LGDNAME == "Belfast City") 
belf_data <- belf_data %>% filter(belf_data$a_speed %in% c(20,30,40)) 
belf_data$a_date <- as.Date(belf_data$a_date,format="%d/%m/%Y")
belf_data <- belf_data[belf_data$a_date >="2013-01-01" & belf_data$a_date <= "2015-12-31", ] %>% filter(!is.na(a_date))
belf_data <- delete_na(belf_data,c("a_gd1","a_gd2"))

#Create new csv file with the cleaned data
write.csv(belf_data, file ="V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\collisions\\Belf_data.csv",row.names=FALSE)

dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"



#Read filtered data for Belfast 20/30/40mph
belf_road_data <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\Belf_data.csv"))

belf_data_converted <- convert_latlong(belf_road_data)
#belf_mapping <- nearest_line(belf_data,belfast_road_data)

crs <- CRS( "+proj=utm +zone=32 +ellps=WGS72 +units=m +no_defs")     # UTM zone = 32 N
wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # long/lat

#Convert data to planar projection  
bel <- belf_data
road_net <- spTransform(belfast_road_data,crs)
#coordinates(belf_data) <- ~ Latitude + Longitude
bel <-  spTransform(SpatialPointsDataFrame(coords = rbind(bel$Longitude,bel$Latitude), proj4string = wgs84,data = bel), crs)

#Make sure our data have same projections
proj4string(df) <- proj4string(road_net)
