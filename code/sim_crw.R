##' Function to simulate a movement track from a 3D correlated random walk
##' 
##' @param xyz0 starting location of the track (default = c(0, 0, 0))
##' @param n_locs number of locations to simulate
##' @param type type of correlated random walk (options: "rw", "vMF", "kent", "bcrw")
##' @param step_par parameters of step length distribution in format c(shape, scale) 
##' @param kappa concentration parameter 


sim_crw <- function(xyz0 = c(0, 0, 0), 
                    n_locs, 
                    type,
                    step_par,
                    kappa = NULL, 
                    rho = NULL, 
                    kappa2 = NULL) {
  
  # transform shape and scale to betas
  shape <- step_par[1]
  scale <- step_par[2]
  step_betas <- transform_par(shape, scale)
  
  # transform xyz0 into matrix
  xyz0 <- matrix(xyz0, ncol = 3)
  
  if(type == "rw") {
    # simulation set-up
    formula <- ~ step + log(step)
    betas <- c(step_betas[1:2])

    # simulate data 
    xyz <- sim_ssf(xyz0 = xyz0, 
                   n = n_locs, 
                   n_zeros = 1, 
                   formula = formula, 
                   betas = betas, 
                   pts_dist = "gamma")
    xyz$ID <- 1
  }
  
  if(type == "vMF") {
    # simulation set-up
    formula <- ~ step + log(step) + cos(omega) + omega_delta
    betas <- c(step_betas[1:2], kappa, 0)
    
    # simulate data 
    xyz <- sim_ssf(xyz0 = xyz0, 
                   n = n_locs, 
                   n_zeros = 10, 
                   formula = formula, 
                   betas = betas, 
                   pts_dist = "gamma+kent")
    xyz$ID <- 1
  }
  
  if(type == "kent") {
    # simulation set-up
    formula <- ~ step + log(step) + cos(omega) + omega_delta
    betas <- c(step_betas[1:2], kappa, rho)
    
    # simulate data 
    xyz <- sim_ssf(xyz0 = xyz0, 
                   n = n_locs, 
                   n_zeros = 10, 
                   formula = formula, 
                   betas = betas, 
                   pts_dist = "gamma+kent")
    xyz$ID <- 1
  }
  
  if(type == "bcrw") {
    # simulation set-up
    formula <- ~ step + log(step) + cos(omega) + cos(vbear)
    betas <- c(step_betas[1:2], kappa, kappa2)
    
    # simulate data 
    xyz <- sim_ssf(xyz0 = xyz0, 
                   n = n_locs, 
                   n_zeros = 1000, 
                   formula = formula, 
                   betas = betas, 
                   pts_dist = "gamma")
    xyz$ID <- 1
  }
  return(xyz)
}

