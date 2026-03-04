#' Density of a Kent distribution
#' 
#' @param kappa angular concentration parameter
#' @param rho ovalness parameter
#' @param omega arc size
#' @param delta arc orientation
#' @param log whether to return the log density or not
#' 
#' @return density or log density
#' @importFrom Directional kent.logcon

dKent <- function(kappa, rho, omega, delta, log = FALSE) {
  # calculate log density
  logC <- kent.logcon(k = kappa, b = rho)
  logdens <- -logC + kappa * cos(omega) + rho * sin(omega)^2 * cos(2 * delta)
  
  if(log) return(logdens)
  else return(exp(logdens))
}

#' Plot density of a Kent distribution on a sphere
#' 
#' @param kappa angular concentration parameter
#' @param rho ovalness parameter
#' 
#' @return plot of the Kent density on a unit sphere
#' @export

plot_dKent <- function(kappa, rho) {
  # grid of all points on the sphere
  df <- data.frame(omega = seq(0, pi, length.out = 250), 
                   delta = seq(-pi, pi, length.out = 250))
  df <- expand.grid(df)
  
  # calculate density
  df$density <- dKent(kappa = kappa, 
                      rho = rho, 
                      omega = df$omega, 
                      delta = df$delta)
  
  # get angles on a sphere
  u <- matrix(NA, nrow = nrow(df), ncol = 3)
  u[,1] <- sin(df$omega) * cos(df$delta)
  u[,2] <- sin(df$omega) * sin(df$delta)
  u[,3] <- cos(df$omega)
  
  
  # settings for plotly
  axx <- list(nticks = 4,range = c(-1,1))
  axy <- list(nticks = 4,range = c(-1,1))
  axz <- list(nticks = 4,range = c(-1,1))
  
  u_df <- data.frame(x = u[,1], y = u[,2], z = u[,3])
  u_df$density <- df$density
  
  plot_ly(u_df,
          x = ~x, y = ~y, z = ~z, 
          type = 'scatter3d', 
          mode = 'markers', 
          color = ~density,
          marker = list(#color = "rgba(0, 0, 0, 0.2)", 
            size = 2, symbol = "circle")) |>
    layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                        aspectmode = "manual",
                        aspectratio = list(x = 1, y = 1, z = 1)))
}
