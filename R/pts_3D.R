#' Function to generate 3D points around a starting coordinate
#' 
#' @param n number of points to generate
#' @param xyz0 starting coordinates (x, y, z)
#' @param dist distributions of points ("uniform" for spatially uniform points, 
#' "gamma" for gamma steps and uniform angles, "gamma+kent" for gamma steps 
#' and kent angles)
#' @param step_par parameters for the step length distribution (needs R for 
#' uniform, shape and scale for gamma)
#' @param angle_par parameters of the kent distribution (needs kappa, rho,
#' hbear0, and vbear0)
#' 
#' @return Data frame of random points and associated movement variables
#' 
#' @export
pts_3D <- function(n,
                   xyz0, 
                   dist = "gamma",
                   step_par,
                   angle_par = NULL) {
  
  if(dist == "uniform") {
    # radius of sphere
    R <- step_par[1]
    
    # get steps lengths
    steps <- runif(n, 0, R^3)^(1/3)
    
    # get angles by: 1) generating points from 3D gaussian
    u <- matrix(rnorm(3 * n), ncol = 3) 
    # 2) scale by length to put angles on the unit sphere
    r <- sqrt(rowSums(u^2)) 
    u <- u / r
    
    # get points in the sphere 
    pts <- u * steps
    
    # get bearings
    hbear <- atan2(pts[,2], pts[,1]) # horizontal
    vbear <- asin(pts[,3] / steps)   # vertical 
  }
  
  if(dist == "gamma") {
    # get steps lengths
    steps <- rgamma(n = n, shape = step_par[1], scale = step_par[2])
    
    # get bearings by: 1) generating points from 3D gaussian
    v <- matrix(rnorm(3 * n), ncol = 3) 
    # 2) scale by length to put angles on the unit sphere
    r <- sqrt(rowSums(v^2)) 
    v <- v / r 
    
    # get points in the sphere 
    pts <- v * steps
    
    # get bearings
    hbear <- atan2(pts[,2], pts[,1]) # horizontal
    vbear <- asin(pts[,3] / steps)   # vertical 
  }
  
  if(dist == "gamma+kent") {
    # get steps lengths
    steps <- rgamma(n = n, shape = step_par[1], scale = step_par[2])
    
    # generate arcs in cartesian coordinates
    u <- rKent(n = n, 
               kappa = angle_par[1], 
               rho = angle_par[2])
    
    # convert u to omega/delta (cartesian to spherical)
    arcs <- get_spherical(u)
    
    # translate arcs to bearings
    bearings <- arcs_to_bearings(omega = arcs$omega, 
                                 delta = arcs$delta, 
                                 hbear0 = angle_par[3], 
                                 vbear0 = angle_par[4])
    
    # convert to cartesian coordinates 
    v <- get_cartesian(pi/2 - bearings$vbear, bearings$hbear)
    
    # get points in the sphere 
    pts <- v * steps
  }
  
  # get xyz in geographic space and then difference between locs
  xyz <- matrix(NA, ncol = 3, nrow = n)
  for(i in 1:length(pts[,1])) {
    xyz[i,] <- pts[i,] + xyz0
  }
  
  # save as df
  if(dist == "uniform" | dist == "gamma") {
    df <- data.frame(x = xyz[,1], 
                     y = xyz[,2], 
                     z = xyz[,3], 
                     d_x = xyz[,1] - xyz0[1], 
                     d_y = xyz[,2] - xyz0[2],
                     d_z = xyz[,3] - xyz0[3],
                     step = steps,
                     hbear = hbear,
                     vbear = vbear)
  }  else {
    df <- data.frame(x = xyz[,1], 
                     y = xyz[,2], 
                     z = xyz[,3], 
                     d_x = xyz[,1] - xyz0[1], 
                     d_y = xyz[,2] - xyz0[2],
                     d_z = xyz[,3] - xyz0[3],
                     step = steps, 
                     hbear = bearings$hbear, 
                     vbear = bearings$vbear, 
                     omega = arcs$omega, 
                     delta = arcs$delta)
  }
  
  return(df)
}