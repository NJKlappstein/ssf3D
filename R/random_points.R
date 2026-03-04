#' Simulate random locations for model fitting
#'
#' @param obs Data frame of observations (ID, x, y, z, time)
#' @param n_random Number of random (i.e., integration) points per observation
#' @param distr Name(s) of distribution(s) used to generate control steps.
#' Current support for uniform in a sphere ("uniform") or gamma step lengths 
#' and uniform turning angles ("gamma") or gamma steps and kent arcs ("gamma+kent")
#' @param par parameters of the sampling distribution
#'
#' @return Data frame of observed and random points, including columns for
#' 'stratum' (stratum index), 'obs' (1 if observed point, 0 if random), 
#' and movement variables such as 'step', 'vbear', 'hbear', 'omega' and 'delta'
#'
#' @export

random_points <- function(obs,
                          n_random,
                          distr = "gamma", 
                          par = NULL)
{
  # Calculate movement variables
  obs <- var3D(obs)
  gpar <- c(mean(obs$step, na.rm = T), sd(obs$step, na.rm = T))
  R <- max(obs$step, na.rm = TRUE)
  
  # get step length and arc parameters if needed
  if(is.null(par)) {
    # gamma distribution parameters
    shape <- (gpar[1]/gpar[2])^2
    scale <- 1 / (gpar[1]/gpar[2]^2)
    
    # kent distribution parameters (if needed)
    if(distr == "gamma+kent")  {
      u <- get_cartesian(obs$omega, obs$delta)
      NAs <- which(is.na(u))
      if(length(NAs) > 0) {
        fit_kent <- kent.mle(u[-NAs,]) 
      } else {
        fit_kent <- kent.mle(u)
      }
      kappa <- fit_kent$param["kappa"]
      rho <- fit_kent$param["beta"]
    }
  }
  
  # extract supplied parameters
  if(!is.null(par)) {
    shape <- par[1]
    scale <- par[2]
    
    if(distr == "gamma+kent") {
      kappa <- par[3]
      rho   <- par[4]
    }
  }
  
  # Loop over IDs
  data_all <- NULL
  IDs <- unique(obs$ID)
  for(j in 1:length(IDs)) {
    # Subset to this ID
    sub_obs <- subset(obs, ID == IDs[j])
    
    # Matrix of locations
    xyz <- as.matrix(sub_obs[, c("x", "y", "z")])
    n_obs <- nrow(xyz)
    
    # Loop over observed steps
    data_list <- lapply(3:n_obs, function(i) {
      
      if(distr == "uniform") {
        # generate uniform points on a sphere
        pts <- pts_3D(n = n_random, 
                      dist = "uniform", 
                      step_par = R, 
                      xyz0 = xyz[i-1,])
      }
      
      if(distr == "gamma") {
        # generate points in a sphere with gamma step lengths
        pts <- pts_3D(n = n_random, 
                      dist = "gamma", 
                      step_par = c(shape, scale),
                      xyz0 = xyz[i-1, ])
      }
      
      if(distr == "gamma+kent") {
        # get previous bearings
        prev_bearings <- c(sub_obs$hbear[i-1], sub_obs$vbear[i-1])
        
        # generate uniform points on a sphere
        pts <- pts_3D(n = n_random, 
                      dist = "gamma+kent", 
                      step_par = c(shape, scale), 
                      angle_par = c(kappa, rho, prev_bearings[1], prev_bearings[2]),
                      xyz0 = xyz[i-1,])
        
        # movement metrics
        sim_step  <- pts$step
        sim_omega <- pts$omega
        sim_delta <- pts$delta
      }
      
      # get movement metrics of random points
      if(distr != "gamma+kent") {
        sim_step <- pts$step
        sim_omega <- acos(sin(sub_obs$vbear[i-1]) * sin(pts$vbear) + 
                            cos(sub_obs$vbear[i-1]) * cos(pts$vbear) * 
                            cos(pts$hbear - sub_obs$hbear[i-1]))
        sim_delta <- atan2(sin(pts$vbear) - cos(sim_omega) * sin(sub_obs$vbear[i-1]), 
                           cos(sub_obs$vbear[i-1]) * cos(pts$vbear) * 
                             sin(pts$hbear - sub_obs$hbear[i-1]))
      }
      
      data.frame(ID = IDs[j],
                 stratum = factor(paste(IDs[j], i, sep = "-")),
                 obs = c(1, rep(0, n_random)),
                 x = c(xyz[i, 1], pts[,1]),
                 y = c(xyz[i, 2], pts[,2]),
                 z = c(xyz[i, 3], pts[,3]),
                 d_x = c(sub_obs$d_x[i], pts$d_x), 
                 d_y = c(sub_obs$d_y[i], pts$d_y),
                 d_z = c(sub_obs$d_z[i], pts$d_z),
                 time = as.POSIXct(sub_obs$time[i],
                                   tz = attr(sub_obs$time, "tzone")),
                 step = c(sub_obs$step[i], sim_step),
                 vbear = c(sub_obs$vbear[i], pts$vbear), 
                 hbear = c(sub_obs$hbear[i], pts$hbear),
                 omega = c(sub_obs$omega[i], sim_omega),
                 delta = c(sub_obs$delta[i], sim_delta))
      
      # Add data for this ID to data frame
    })
    data_all <- rbind(data_all, do.call(rbind, data_list))
  }
  
  # add sampling parameters as attributes
  if(distr != "uniform") {
    attr(data_all, "shape") <- shape 
    attr(data_all, "scale") <- scale 
    
    if(distr == "gamma+kent") {
      attr(data_all, "kappa") <- as.numeric(kappa)
      attr(data_all, "rho") <- as.numeric(rho)
    }
  }
  
  return(data_all)
}
