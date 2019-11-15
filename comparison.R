# Load libraries
library(rgdal)
library(leaflet)
library(tidyverse)
library(rgeos)

#-----------------------------
#1.Data-----------------------
#-----------------------------

### Replace V:\\Studies\\MOVED\\HealthImpact\\Data\\ with C:
dir_path <- "V:\\Studies\\MOVED\\HealthImpact\\Data\\"

### Visualize collisions data - 2015 data
source_rd <- read_csv(paste0(dir_path, "20mph study collisions\\collisions\\collisions 2015.csv"))

#create sp doc
rd <- source_rd %>% filter(`Local_Authority_(District)` == 923)
rd <- rd[1:100,]

#Keep a copy in data frame
rd1 <- rd

#add coordinates to make it spatial data
coordinates(rd) <- ~Longitude + Latitude

#add a buffer around the point
pointsBuffer <- gBuffer(rd, width=.001, byid = TRUE)

#### Edinburgh visualizations
## Read geodatabase
gdb_path <- paste0(dir_path, "20mph study collisions\\20mph.gdb")

gdb_layers <- ogrListLayers(gdb_path)

edin_impl_zones <- readOGR(dsn = gdb_path, layer="ImplementationZones")
# Transform the data by applying a projection
edin_impl_zones <- spTransform(edin_impl_zones, "+init=epsg:4326")

# Visualize using leaflet
leaflet(edin_impl_zones) %>% addTiles() %>% addPolygons()

edin_cons_streets <- readOGR(dsn = gdb_path,layer="Consultation20mphStreets")
# Transform the data by applying a projection
edin_cons_streets <- spTransform(edin_cons_streets, "+init=epsg:4326")

points_polygons <- gBuffer(rd, width=.0005, byid = TRUE)

proj4string(points_polygons) <- proj4string(edin_cons_streets)

intersect_lines <- gIntersection(edin_cons_streets, points_polygons, byid = T)

# Visualize using leaflet
leaflet() %>% addTiles() %>% addPolygons(data = points_polygons) %>% addPolygons(data = intersect_lines, color = 'Black')

overlapped_incidents_roads <- over(edin_cons_streets, points_polygons, returnList=TRUE)
#over(points_polygons, rd)

library(ggplot2)
d3 <- fortify(points_polygons)

df_road <- fortify(edin_cons_streets)
df_road[,1] <- round(df_road[,1],digits = 6)
df_road[,2] <- round(df_road[,2],digits = 6)
d3[,1] <- round(d3[,1],digits = 6)
d3[,2] <- round(d3[,2],digits = 6)


#-----------------------------------------
#2.Plots----------------------------------
#-----------------------------------------

#plot without buffer
y <- ggplot(data = df_road, aes(x = long, y = lat,group=group)) + geom_path() 
y+ geom_point(data = rd1, aes(x = Longitude, y = Latitude,group=Speed_limit))

#plot with buffer
y <- ggplot(data = df_road, aes(x = long, y = lat,group=group)) + geom_path() 
y+ geom_point(data = d3, aes(x = long, y = lat,group=group))

#----------------------------------------
#3. Comparison---------------------------
#----------------------------------------

#compare collisions with road (long,lat)
#1.with inner_join , all possible combinations ?

common22 <- inner_join( d3,df_road,by =c("lat"="lat") )
common11 <- inner_join( d3,df_road,by =c("long"="long") )
common <- inner_join (common11,common22,by = c("long"="long.y"))

common22b <- inner_join( df_road,d3,by =c("lat"="lat") )
common11b <- inner_join( df_road,d3,by =c("long"="long") )
commonb <- inner_join (common11b,common22b,by = c("long"="long.y"))

#inner_join(d3, df_road) %>% count(group)

#2.with merged
merged_by_long <- merge(x = d3, y = df_road, by = ("long")) 
merged_by_lat <- merge(x = d3, y = df_road, by = ("lat")) 
merged_both <- merge(x = d3, y = df_road, by = c("lat", "long")) 

#3.with %in% or ==
