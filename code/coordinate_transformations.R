##' Function to get Cartesian coordinates from spherical coordinates based
##' on the formulas
##' cart1 = sin(coord1)cos(coord2),
##' cart2 = sin(coord1)sin(coord2), 
##' cart3 = cos(coord1)
##' 
##' @param coord1 first spherical coordinate
##' @param coord2 second spherical coordinate
##' @param type whether it is bearings or geodesics

get_cartesian <- function(coord1, coord2, type = "geodesics") {
  
  #initialise matrix 
  cartesian <- matrix(NA, nrow = length(coord1), ncol = 3)
  if(type == "geodesics") {
    cartesian[,1] <- sin(coord1) * cos(coord2)
    cartesian[,2] <- sin(coord1) * sin(coord2)
    cartesian[,3] <- cos(coord1)
  }
  if(type == "bearings") {
    cartesian[,1] <- cos(coord1) * cos(coord2)
    cartesian[,2] <- cos(coord1) * sin(coord2)
    cartesian[,3] <- sin(coord1)
  }
  
  return(cartesian)
}

##' Function to get spherical coordinates of arcs from cartesian coordinates
##' 
##' @param u 3 column matrix of cartesian coordinates of arcs

get_spherical <- function(u) {
  # calculate omega and delta
  omega <- acos(u[,3] / (sqrt(u[,1]^2 + u[,2]^2 + u[,3]^2)))
  delta <- atan2(u[,2], u[,1])
  
  return(data.frame(omega = omega, delta = delta))
}


#' Shift angles to be between -pi and pi
#'
#' @param angles Vector of angles
shift_angle <- function(angles) {
  angles[which(angles < -pi)] <- angles[which(angles < -pi)] + 2 * pi
  angles[which(angles > pi)] <- angles[which(angles > pi)] - 2 * pi
  return(angles)
}

##' Function to transform arcs to bearings
##' 
##' @param omega arc size at time t
##' @param delta arc orientation at time t
##' @param hbear0 previous horizontal bearing (at time t-1)
##' @param vbear0 previous vertical bearing (at time t-1)
##' 

arcs_to_bearings <- function(omega, delta, hbear0, vbear0) {
  
  # calculate current horizontal bearing 
  hbear <- hbear0 + atan2(sin(omega) * cos(delta), 
                          cos(omega) * cos(vbear0) - sin(omega) * sin(delta) * sin(vbear0))
  
  # shift angle to be between -pi, pi
  hbear <- shift_angle(angles = hbear)
  
  # calculate current vertical bearing
  vbear <- asin(sin(omega) * sin(delta) * cos(vbear0) + cos(omega) * sin(vbear0))
  
  return(data.frame(hbear = hbear, vbear = vbear))
}


