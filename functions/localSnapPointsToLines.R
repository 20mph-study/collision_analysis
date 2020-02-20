# Create a copy of a function that DOES NOT identify the class of points, and restrict the function to stop working with attrs for points
# Source: https://rdrr.io/cran/maptools/src/R/point_line_distance.R

localSnapPointsToLines <- function( points, lines, maxDist=NA, withAttrs=TRUE, idField=NA) {
  
  if (rgeosStatus()) {
    if (!requireNamespace("rgeos", quietly = TRUE))
      stop("package rgeos required for snapPointsToLines")
  } else
    stop("rgeos not installed")
  
  if (is(points, "SpatialPoints") && missing(withAttrs))
    withAttrs = FALSE
  
  # if (is(points, "SpatialPoints") && withAttrs==TRUE)
  #   stop("A SpatialPoints object has no attributes! Please set withAttrs as FALSE.")
  
  d = rgeos::gDistance(points, lines, byid=TRUE) 
  
  if(!is.na(maxDist)){
    distToLine <- apply(d, 2, min, na.rm = TRUE)  
    validPoints <- distToLine <= maxDist  # indicates which points are within maxDist of a line
    distToPoint <- apply(d, 1, min, na.rm = TRUE)
    validLines <- distToPoint <= maxDist 
    
    # Drop elements beyond maxdist
    points <- points[validPoints, ]
    lines = lines[validLines, ]
    d  = d[ validLines,  validPoints, drop = FALSE]
    distToLine <- distToLine[validPoints]
    
    # If no points are within maxDist return an empty SpatialPointsDataFrame object 
    if(!any(validPoints)){
      if(is.na(idField)){
        idCol = character(0)
      } else {
        idCol = lines@data[,idField][0]
      }
      newCols = data.frame(nearest_line_id  = idCol, snap_dist = numeric(0))
      if(withAttrs) df <- cbind(points@data, newCols) else df <- newCols
      res <- SpatialPointsDataFrame(points, data=df, 
                                    proj4string=CRS(proj4string(points)), match.ID = FALSE)
      return(res)
    }
    
  } else { # If no maxDist arg still calculate distToLine so it can be returned
    distToLine = apply(d, 2, min, na.rm = TRUE)
  }
  
  nearest_line_index = apply(d, 2, which.min) # Position of each nearest line in lines object 
  
  coordsLines = coordinates(lines)  
  coordsPoints = coordinates(points)  
  
  # Get coordinates of nearest points lying on nearest lines
  mNewCoords = vapply(1:length(points), 
                      function(x) 
                        nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                           coordsPoints[x,]), FUN.VALUE=c(0,0))
  
  # Recover lines' Ids (If no id field has been specified, take the sp-lines id)
  if (!is.na(idField)) { 
    nearest_line_id = lines@data[,idField][nearest_line_index] 
  }  else {
    nearest_line_id = sapply(slot(lines, "lines"), function(i) slot(i, "ID"))[nearest_line_index] 
  }
  # Create data frame and sp points
  if (withAttrs) df = cbind(points@data, data.frame(nearest_line_id, snap_dist = distToLine)) 
  else df = data.frame(nearest_line_id, snap_dist = distToLine, row.names=names(nearest_line_index))
  
  SpatialPointsDataFrame(coords=t(mNewCoords), data=df, 
                         proj4string=CRS(proj4string(points)))
}
