#' Get Cartesian coordinates from spherical coordinates
#' 
#' @details The formulas depends on the 'type' argument. If type = "geodesics",
#' coord1 measures the "colatitude" angle between 0 and pi, and the 
#' transformations are:
#' \itemize{
#' \item cart1 = sin(coord1)cos(coord2)
#' \item cart2 = sin(coord1)sin(coord2)
#' \item cart3 = cos(coord1)
#' }
#' If type = "bearings", coord1 is the "latitude" angle between -pi/2 and pi/2,
#' and the transformations are: 
#' \itemize{
#' \item cart1 = cos(coord1)cos(coord2)
#' \item cart2 = cos(coord1)sin(coord2)
#' \item cart3 = sin(coord1)
#' }
#' 
#' @param coord1 first spherical coordinate
#' @param coord2 second spherical coordinate
#' @param type either "bearings" or "geodesics"
#' 
#' @return Matrix with three columns for Cartesian coordinates

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

#' Get spherical coordinates of arcs from cartesian coordinates
#' 
#' @param u 3 column matrix of cartesian coordinates of arcs
#' 
#' @return Data frame with columns 'omega' (geodesic length) and 'delta'
#' (geodesic orientation)

get_spherical <- function(u) {
  omega <- acos(u[,3] / (sqrt(u[,1]^2 + u[,2]^2 + u[,3]^2)))
  delta <- atan2(u[,2], u[,1])
  return(data.frame(omega = omega, delta = delta))
}


#' Shift angles to be between -pi and pi
#'
#' @param angles Vector of angles
#' 
#' @return Vector of shifted angles
shift_angle <- function(angles) {
  angles[which(angles < -pi)] <- angles[which(angles < -pi)] + 2 * pi
  angles[which(angles > pi)] <- angles[which(angles > pi)] - 2 * pi
  return(angles)
}

#' Transform arcs to bearings
#' 
#' @param omega arc size between times t and t+1
#' @param delta arc orientation between times t and t+1
#' @param hbear0 previous horizontal bearing (between times t-1 and t)
#' @param vbear0 previous vertical bearing (between times t-1 and t)
#' 
#' @return Data frame with columns 'hbear' (horizontal bearing) and
#' 'vbear' (vertical bearing)

arcs_to_bearings <- function(omega, delta, hbear0, vbear0) {
  
  # calculate current horizontal bearing 
  hbear <- hbear0 + atan2(sin(omega) * cos(delta), 
                          cos(omega) * cos(vbear0) - 
                            sin(omega) * sin(delta) * sin(vbear0))
  
  # shift angle to be between -pi, pi
  hbear <- shift_angle(angles = hbear)
  
  # calculate current vertical bearing
  vbear <- asin(sin(omega) * sin(delta) * cos(vbear0) + 
                  cos(omega) * sin(vbear0))
  
  return(data.frame(hbear = hbear, vbear = vbear))
}
