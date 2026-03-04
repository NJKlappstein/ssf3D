#' Function to plot geodesics (3D turning angles) as points on a sphere
#' 
#' @param data data frame with columns 'omega' (geodesic length) and
#' 'delta' (geodesic orientation), OR a matrix of coordinates
#' @param format Format of data (described above). Options are "matrix" or "df"
#' 
#' @export

plot_geodesics <- function(data, format = "df") {
  # grid of all points on the sphere
  border <- data.frame(omega = seq(0, pi, length.out = 250), 
                       delta = seq(-pi, pi, length.out = 250))
  border <- expand.grid(border)
  border <- get_cartesian(border$omega, border$delta)
  border <- data.frame(x = border[,1], y = border[,2], z = border[,3])
  
  # coordinates of data
  if(format == "df") {
    # convert arc into coordinates on a unit sphere
    u <- get_cartesian(data$omega, data$delta)
    u_df <- data.frame(x = u[,1], y = u[,2], z = u[,3])
  } 
  if(format == "matrix") {
    u_df <- data.frame(x = data[,1], y = data[,2], z = data[,3])
  }
  
  # axis stuff
  axx <- list(nticks = 4,range = c(-1,1))
  axy <- list(nticks = 4,range = c(-1,1))
  axz <- list(nticks = 4,range = c(-1,1))
  
  # plot background sphere
  plot_ly(border,
          x = ~x, y = ~y, z = ~z, 
          type = 'scatter3d', 
          mode = 'markers', 
          marker = list(color = "rgba(0, 0, 0, 0.01)", 
                        size = 0.5, symbol = "circle")) |> 
    layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, 
                        aspectmode = "manual",
                        aspectratio = list(x = 1, y = 1, z = 1))) |> 
    add_trace(x = u_df$x, y = u_df$y, z = u_df$z, 
              type = 'scatter3d', 
              marker = list(color = "rgba(0, 0, 0, 0.5)", size = 1.5), 
              showlegend = F) 
}

