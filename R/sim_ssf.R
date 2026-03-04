#' Simulate a movement track from a 3D SSF
#' 
#' @param xyz0 starting location of the track
#' @param n_zeros number of proposed endpoints
#' @param n_locs number of locations to simulate
#' @param formula SSF formula
#' @param betas SSF parameters (assumes the order beta_step, beta_logstep, 
#' beta_omega, beta_omegadelta)
#' @param R radius of the sphere (in which to generate endpoints, only 
#' needed for uniform)
#' @param pts_dist Distriution of distances (default = "gamma", also supports 
#' "uniform")
#' @param cov_2D named list of rasters of 2D covariate data (optional)
#' @param cov_2D named list of raster stacks of 3D covariate data (optional)
#' @param print whether or not to print progress bar
#' @param log whether or not to use log in the ssf probs
#' 
#' @return Data frame with columns "x", "y", "z", "step", "vbear", 
#' "hbear", "omega", and "delta"
#'
#' @export

sim_ssf <- function(xyz0, 
                    n_zeros, 
                    n_locs, 
                    formula, 
                    betas, 
                    R = NULL, 
                    pts_dist = "gamma", 
                    cov_2D = NULL,
                    cov_3D = NULL,
                    print = TRUE, 
                    log = FALSE) {
  
  # initialise empty matrix
  xyz <- matrix(NA, ncol = 8, nrow = n_locs + 1)
  
  # get first step variables
  step1 <- runif(1, 0, 1^3)^(1/3)
  hbear1 <- runif(1, -pi, pi)
  vbear1 <- runif(1, -pi/2, pi/2)
  
  # get first locations
  xyz[1,1] <-  xyz0[1,1] + step1 * cos(vbear1) * cos(hbear1)
  xyz[1,2] <-  xyz0[1,2] + step1 * cos(vbear1) * sin(hbear1)
  xyz[1,3] <-  xyz0[1,3] + step1 * sin(vbear1)
  
  if(pts_dist == "gamma") {
    # set up parameters 
    scale <- -1 / betas[1]
    shape <- betas[2] + 3
    formula <-  update(formula, ~. - step - log(step))
    betas <- betas[-c(1:2)]
  }
  
  if(pts_dist == "gamma+kent") {
    # set up parameters 
    scale <- -1 / betas[1]
    shape <- betas[2] + 3
    kappa <- betas[3]
    rho <- betas[4]
    formula <-  update(formula, ~. - step - log(step) - cos(omega) - omega_delta)
    betas <- betas[-c(1:4)]
  }
  
  prev_bearings <- c(hbear1, vbear1)
  i <- 1
  
  while(i <= n_locs) {
    if(i%%10 == 0 & print == TRUE) {
      cat("\rSimulating...", round(i/(n_locs-1), 2)*100, "%")            
    }
    
    if(pts_dist == "uniform") {
      # generate uniform points on a sphere
      pts <- pts_3D(n = n_zeros, 
                    dist = "uniform", 
                    step_par = R, 
                    xyz0 = xyz[i, 1:3])
    }
    
    if(pts_dist == "gamma") {
      # generate points in a sphere with gamma step lengths
      pts <- pts_3D(n = n_zeros, 
                    dist = "gamma", 
                    step_par = c(shape, scale),
                    xyz0 = xyz[i, 1:3])
    }
    
    if(pts_dist == "gamma+kent") {
      # generate uniform points on a sphere
      pts <- pts_3D(n = n_zeros, 
                    dist = "gamma+kent", 
                    step_par = c(shape, scale), 
                    angle_par = c(kappa, rho, prev_bearings[1], prev_bearings[2]),
                    xyz0 = xyz[i, 1:3])
    }
    
    # get omegas/deltas
    hbears <- matrix(c(rep(prev_bearings[1], times = n_zeros), pts$hbear), ncol = 2)
    vbears <- matrix(c(rep(prev_bearings[2], times = n_zeros), pts$vbear), ncol = 2)
    pts$omega <- calc_omega(vbear = vbears, hbear = hbears)
    pts$delta <- calc_delta(omega = pts$omega, vbear = vbears, hbear = hbears)
    
    # get covariate for ovalness parameter
    pts$omega_delta <- sin(pts$omega) * sin(pts$omega) * cos(2 * pts$delta)
    
    # get 2D spatial covariates
    if(!is.null(cov_2D)) {
      # create dataframe from movement and habitat covariates 
      pts <- as.data.frame(cov_df(formula, cov_2D, data = pts, dim = 2))
    }
    
    # get 3D spatial covariates
    if(!is.null(cov_3D)) {
      # create dataframe from movement and habitat covariates 
      pts <- cov_df(formula, cov_3D, data = pts, dim = 3)
    }
    
    # if there are remaining covariates other than movement kernel
    if(length(betas) > 0) {
      # get model matrix
      mod_matrix <- model.matrix(formula, pts)
      mod_matrix <- mod_matrix[,!colnames(mod_matrix) == "(Intercept)"]
      
      # get linear predictor
      if(length(betas) == 1) {betas <- as.matrix(betas)}
      linear_pred <- mod_matrix %*% betas
      
      # calculate ssf for each endpoint
      if(log == TRUE) {
        ssf_prob <- exp(linear_pred - logsumexp(linear_pred))
      } else {
        ssf <- exp(linear_pred)
        ssf_prob <- ssf / (sum(ssf))
      }
      
      # sample next location with probabilities prop to SSF
      index <- sample(1:nrow(pts), size=1, prob = ssf_prob) 
    }
    
    # if points are from true movement kernel and there are no other covariates
    if(length(betas) == 0) {
      # sample with equal probability
      index <- sample(1:nrow(pts), size=1)
    }
    
    # get next points as df
    xyz[i+1,] <- unlist(pts[index, c("x", "y", "z", "step", 
                                     "vbear", "hbear", "omega", "delta")])
    
    # set new set of bearings
    prev_bearings <- unlist(pts[index, c("hbear", "vbear")])
    i <- i + 1
  }
  
  colnames(xyz) <- c("x", "y", "z", "step", 
                     "vbear", "hbear", "omega", "delta")
  xyz <- as.data.frame(xyz[-1,])
  return(xyz)
}
