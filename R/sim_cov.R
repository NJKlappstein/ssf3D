
#' Generate a covariate in 3d
#'
#' @param nx Number of grid cells in x dimension
#' @param ny Number of grid cells in y dimension
#' @param nz Number of grid cells in z dimension
#' @param window_size Diameter of moving window spherical kernel
#' 
#' @return 3D array of covariate values, with nx rows, ny columns, and nz layers
#' 
#' @export
#' @importFrom imager as.cimg convolve

sim_cov <- function(nx = 100, ny = 100, nz = 20, window_size = 5) {
  if(window_size %% 2 == 0) {
    window_size <- window_size + 1
    message(paste0("window_size must be odd, changed to ", window_size))
  }
  
  # Padding to avoid boundary effects
  radius <- (window_size - 1)/2
  nx_padded <- nx + 2 * radius
  ny_padded <- ny + 2 * radius
  nz_padded <- nz + 2 * radius
  
  # Uniform random noise (dummy 4th dim required by imager)
  noise <- array(runif(nx_padded * ny_padded * nz_padded), 
                 dim = c(nx_padded, ny_padded, nz_padded, 1))
  
  # Kernel = 3D ball of diameter window_size
  grid1d <- (-radius) : radius
  grid3d <- expand.grid(i = grid1d, j = grid1d, k = grid1d)
  grid3d$dist <- sqrt(grid3d$i^2 + grid3d$j^2 + grid3d$k^2)
  kernel <- array(as.integer(grid3d$dist <= radius), 
                  dim = c(rep(window_size, 3), 1))
  kernel <- as.cimg(kernel)
  
  # Apply moving average / convolution using imager
  input_cimg <- as.cimg(noise)
  output_cimg <- convolve(input_cimg, kernel)
  
  # Convert back to array
  smooth <- as.array(output_cimg)
  
  # Remove padding
  smooth_trim <- smooth[(radius + 1):(radius + nx),
                        (radius + 1):(radius + ny),
                        (radius + 1):(radius + nz),
                        1]
  
  # Scale to [0, 1]
  smooth_trim <- (smooth_trim - min(smooth_trim)) / 
    (max(smooth_trim) - min(smooth_trim))
  
  return(smooth_trim)
}

#' Convert 3D array to terra raster stack
#' 
#' @param array Array of covariate values
#' @param xmin Minimum x coordinate
#' @param xmax Maximum x coordinate
#' @param ymin Minimum y coordinate
#' @param ymax Maximum y coordinate
#' @param depth Vector of depths (z values)
#' 
#' @return SpatRaster stack with one raster for each array layer
#'
#' @export
#' @importFrom terra rast ext names depth

array_to_stack <- function(array, xmin, xmax, ymin, ymax, depth) {
  raster_list <- list()
  ext <- ext(xmin, xmax, ymin, ymax)
  dims <- dim(array)
  
  # Convert each array slice to a raster layer
  for (k in 1:dims[3]) {
    # Transpose layer to get into raster orientation
    mat <- t(array[,,k])
    
    # Create raster from matrix
    raster_list[[k]] <- rast(nrows = nrow(mat),
                             ncols = ncol(mat),
                             extent = ext,
                             vals = as.vector(mat),
                             crs = "")
  }
  
  # Combine all layers into a stack
  raster_stack <- rast(raster_list)
  names(raster_stack) <- depth
  depth(raster_stack) <- depth
  return(raster_stack)
}

