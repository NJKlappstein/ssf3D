#' Calculate arc size (omega)
#' 
#' @param vbear vertical bearing
#' @param hbear horizontal bearing
#' 
#' @return Vector of geodesic lengths
#' 
#' @export

calc_omega <- function(vbear, hbear) {
  omega <- acos(sin(vbear[,1]) * sin(vbear[,2]) + 
                  cos(vbear[,1]) * cos(vbear[,2]) * cos(hbear[,2] - hbear[,1]))
  
  return(omega)
}


#' Calculate arc orientation (delta)
#' 
#' @param omega arc size
#' @param vbear vertical bearing
#' @param hbear horizontal bearing
#' 
#' @return Vector of geodesic orientations
#' 
#' @export

calc_delta <- function(omega, vbear, hbear) {
  delta <- atan2(sin(vbear[,2]) - cos(omega) * sin(vbear[,1]), 
                 cos(vbear[,1]) * cos(vbear[,2]) * sin(hbear[,2] - hbear[,1]))
  return(delta)
}

#' Calculate 3D variables 
#' 
#' Get arc size/orientation, bearings, and turning angles from 3D positions
#' 
#' @param data data frame of locs with ID, x,y,z columns
#' 
#' @return Data frame with columns for:
#' \itemize{
#' \item{d_x, d_y, d_z}{3D displacements}
#' \item{step, step_xy}{step lengths in 3D and 2D}
#' \item{vbear, hbear}{vertical and horizontal bearings}
#' \item{vbear_star, hbear_star}{changes in bearings}
#' \item{omega, delta}{geodesic variables}
#' }
#' 
#' @export

var3D <- function(data) {
  #initialise dataframe
  data_all <- NULL
  for(id in unique(data$ID)) {
    # subset data and get coordinates 
    data_sub <- subset(data, ID == id)
    xyz <- as.matrix(data_sub[, c("x", "y", "z")])
    
    # get delta x, y, z
    d_xyz <- matrix(c(diff(xyz[,1]), diff(xyz[,2]), diff(xyz[,3])), 
                    ncol = 3)
    
    # calculate step lengths
    steps <- sqrt(d_xyz[,1]^2 + d_xyz[,2]^2 + d_xyz[,3]^2)
    steps_xy <- sqrt(d_xyz[,1]^2 + d_xyz[,2]^2)
    
    # calculate horixontal and vertical bearings
    hbear <- atan2(d_xyz[,2], d_xyz[,1])
    vbear <- asin(d_xyz[,3] / steps)
    
    # calculate arc size
    vbear_mat <- cbind(vbear[-length(vbear)], vbear[-1])
    hbear_mat <- cbind(hbear[-length(hbear)], hbear[-1])
    omega <- calc_omega(vbear_mat, hbear_mat)
    
    #calculate arc orientation (delta)
    delta <- calc_delta(omega, vbear_mat, hbear_mat)
    
    # calculate horizontal turning angles
    hbear_star <- diff(hbear)
    hbear_star[which(hbear_star < -pi)] <- hbear_star[which(hbear_star < -pi)] + 2 * pi
    hbear_star[which(hbear_star > pi)] <- hbear_star[which(hbear_star > pi)] - 2 * pi
    
    # calculate vertical turning angle vbear
    vbear_star <- diff(vbear) 
    
    arc_data <- data.frame(d_x = c(NA, d_xyz[,1]), 
                           d_y = c(NA, d_xyz[,2]), 
                           d_z = c(NA, d_xyz[,3]), 
                           step = c(NA, steps), 
                           step_xy = c(NA, steps_xy),
                           vbear = c(NA, vbear), 
                           hbear = c(NA, hbear), 
                           vbear_star = c(NA, NA, vbear_star),
                           hbear_star = c(NA, NA, hbear_star),
                           omega = c(NA, NA, omega), 
                           delta = c(NA, NA, delta))
    data_all <- rbind(data_all, arc_data)
  }
  
  data <- cbind(data, data_all)
  return(data)
  
}
