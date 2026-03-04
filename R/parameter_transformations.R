
##' transform betas to the parameters of gamma distribution
##' 
##' @param beta_step coefficient for step
##' @param beta_logstep coefficient for log(step)

transform_betas <- function(beta_step, beta_logstep) {
  
  # calculate shape and scale
  shape <- beta_logstep + 3
  scale <- - 1 / beta_step
  
  # calculate mean and sd
  mean <- shape * scale
  sd <- sqrt(shape * scale^2)
  
  return(c(shape = shape, 
           scale = scale, 
           mean = mean, 
           sd = sd))
}

##' Transform shape and scale into betas and mean/sd
##' 
##' @param shape shape parameter
##' @param scale scale parameter

transform_par <- function(shape, scale) {
  
  # get betas
  beta_step <- - 1 / scale
  beta_logstep <- shape - 3
  
  # get mean/sd
  mean <- shape * scale
  sd <- sqrt(shape * scale^2)
  
  return(c(beta_step = beta_step, 
           beta_logstep = beta_logstep, 
           mean = mean, 
           sd = sd))
}


##' Transform mean/sd into betas and shape/scale
##' 
##' @param mean mean
##' @param sd standard deviation

transform_mean <- function(mean, sd) {
  
  # get shape and scale
  shape <-  (mean / sd)^2
  scale <- 1 / (mean / sd^2)
  
  # get betas
  beta_step <- - 1 / scale
  beta_logstep <- shape - 3
  
  return(c(beta_step = beta_step, 
           beta_logstep = beta_logstep, 
           shape = shape, 
           scale = scale))
}



