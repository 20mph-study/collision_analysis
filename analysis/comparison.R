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

# keeps intersect lines in df
library(raster)
inter <- intersect(edin_cons_streets,points_polygons)

# Visualize using leaflet
leaflet() %>% addTiles() %>% addPolygons(data = points_polygons) %>% addPolygons(data = intersect_lines, color = 'Black')

#over for collisions
overlapped_incidents_roads <- over(edin_cons_streets, points_polygons, returnList=FALSE)
clean_overlapped <-  na.omit(overlapped_incidents_roads[1:29])
names(clean_overlapped)

#over for roads
overlapped_incidents_roads_edin <- over( points_polygons, edin_cons_streets,returnList=FALSE)
print(overlapped_incidents_roads_edin)

#count
overlapped_incidents_roads_edin%>%count(LAYER=="20mph local streets")


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
