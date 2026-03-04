#' Extract 3D covariates from raster stack
#' 
#' @param raster_stack SpatRaster stack; must have "depth", which is used
#' to find layer associated with a given z value
#' @param points Matrix or data frame with three columns, for x, y and z
#' coordinates
#' 
#' @return Vector of covariate values at points.


extract_3d2 <- function(raster_stack, points) {
  # Grid of z values in raster stack
  z_grid <- depth(raster_stack)
  
  if(class(points) == "data.frame") {
    # Convert points to matrix
    points <- data.frame(x = points$x, y = points$y, z = points$z)
    pts_mat <- as.matrix(points)
    colnames(pts_mat) <- c("x", "y", "z")
  }
  
  # Get nearest layer for each point
  z_diff <- abs(outer(pts_mat[, "z"], z_grid, "-"))
  pts_layer <- max.col(-z_diff, ties.method = "first")
  
  # Group points by layer
  layer_indices <- unique(pts_layer)
  
  # Loop over layers
  values <- rep(NA, nrow(pts_mat))
  for(layer in layer_indices) {
    # Get points for this layer
    index <- pts_layer == layer
    pts <- pts_mat[index, c("x", "y"), drop = FALSE]
    
    # Extract values 
    values[index] <- terra::extract(x = raster_stack[[layer]],
                                    y = pts)[,1]
  }
  
  return(values)
}

