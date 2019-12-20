# Load libraries
library(rgdal)
library(tidyverse)
library(readxl)
library(leaflet)

# Source - or guide 
# http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf

# Read 2019 data
rd <- read_excel("V:\\Studies\\MOVED\\HealthImpact\\Data\\20mph study collisions\\collisions\\collisions 2019 Jan to May Edinburgh only.xls")

# Let's rename columns to latitude and longitude
rd <- rd %>% rename("Latitude" = "Grid Ref: Easting", "Longitude" = "Grid Ref: Northing")

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

# Plot
m <- leaflet(rd) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
# Setview to city of Edinburgh
m <- m %>% setView(-3.188267, 55.953251, zoom = 12)
# Add circles
m %>% addCircles(weight = 3, radius=40, color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 

