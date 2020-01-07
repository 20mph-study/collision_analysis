# Create a copy of a function that DOES NOT identify the class of points, and restrict the function to stop working with attrs for points
# Source: http://geotux.tuxfamily.org/index.php/en/geo-blogs/item/296-snapping-points-to-lines-in-r

localSnapPointsToLines <- function( points, lines, maxDist=NA, withAttrs=TRUE) {
  
  require("rgeos")
  
  if (!is.na(maxDist)) {
    w = gWithinDistance(points, lines, dist=maxDist, byid=TRUE)
    validPoints = apply(w,2,any)
    validLines = apply(w,1,any)
    points = points[validPoints,]
    lines =  lines[validLines,]
  }
  
  d = gDistance(points, lines, byid=TRUE) 
  nearest_line_index = apply(d, 2, which.min) # Position of each nearest line in lines object 
  
  coordsLines = coordinates(lines)  
  coordsPoints = coordinates(points)  
  
  # Get coordinates of nearest points lying on nearest lines
  mNewCoords = vapply(1:length(points), 
                      function(x) 
                        nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                           coordsPoints[x,]), FUN.VALUE=c(0,0))
  
  # Recover lines' Ids (Ids and index differ if maxDist is given)
  if (!is.na(maxDist)) nearest_line_id = as.numeric(rownames(d)[nearest_line_index])+1 
  else nearest_line_id = nearest_line_index 
  
  # Create data frame and sp points
  if (withAttrs) df = cbind(points@data, nearest_line_id) 
  else df = data.frame(nearest_line_id, row.names=names(nearest_line_index))
  
  SpatialPointsDataFrame(coords=t(mNewCoords), data=df, 
                         proj4string=CRS(proj4string(points)))
}
