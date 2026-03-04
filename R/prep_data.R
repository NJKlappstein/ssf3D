
#' Prepare data for fitting 3D CRWs or SSFs
#'
#' @param data data frame of locations with columns: ID, x,y,z, time
#' @param n_random Number of random (i.e., integration) points per observation
#' @param distr Name(s) of distribution(s) used to generate random points.
#' Current support for uniform in a sphere ("uniform") or gamma step lengths 
#' and uniform turning angles ("gamma") or gamma steps and kent arcs ("gamma+kent")
#' @param par parameters of the sampling distribution
#' @param ssf whether or not to prep the data for an SSF (when set to TRUE, 
#' random points will be generated; when set to FALSE, movement variables for the 
#' observed data will be calculated)
#'
#' @return data 
#' @export

prep_data <- function(data, 
                      n_random = 25, 
                      distr = "gamma", 
                      par = NULL, 
                      ssf = TRUE) {
  
  # option for just calculating movement variables of observations
  if(ssf == FALSE) {
    data <- var3D(data)
  }
  
  # option to generate random points
  if(ssf == TRUE) {
    data <- random_points(obs = data, 
                          n_random = n_random, 
                          distr = distr, 
                          par = par)
  }
  
  return(data)
}
