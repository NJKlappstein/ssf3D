
#' Simulate from a Kent distribution 
#' 
#' @details This function assumes mean = (0,0,1), 
#' major = (1,0,0), and minor = (0,1,0)
#' 
#' @param n number of samples
#' @param kappa concentration parameter
#' @param rho ovalness parameter
#' 
#' @return Samples from a Kent distribution (Cartesian coordinates)
rKent <- function(n, kappa, rho) {
  
  Gamma <- matrix(c(1, 0, 0,
                    0, 0, 1, 
                    0, 1, 0), ncol = 3, byrow = TRUE)
  # get lambda matrix
  lambda_max <- rho
  lambda_mid <- 0
  lambda_min <- -rho
  Lambda <- diag(c(lambda_max, lambda_mid, lambda_min))
  
  # calculate A
  A <- Gamma %*% Lambda %*% t(Gamma)
  
  # generate sample
  u <- simdd::rFisherBingham(n, mu = kappa * c(0, 0, 1), Aplus = A)
  
  return(u)
}