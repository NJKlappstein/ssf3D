##' Function to plot 3D location data
##' 
##' @param data data frame with columns: x, y, z, ID
##' @param scale whether the x, y, z limits should all be on the same scale

plot_data <- function(data, scale = FALSE) {
  # define plot limits
  xlim <- c(min(data$x), max(data$x))
  ylim <- c(min(data$y), max(data$y))
  zlim <- c(min(data$z), max(data$z))
  
  if(scale == TRUE) {
    # scale of each dimension
    scale_x <- xlim[2] - xlim[1]
    scale_y <- ylim[2] - ylim[1]
    scale_z <- zlim[2] - zlim[1]
    
    # largest scale
    max_scale <- max(c(scale_x, scale_y, scale_z))
    
    # mid points of each
    mid_x <- xlim[1] + scale_x/2
    mid_y <- ylim[1] + scale_y/2
    mid_z <- zlim[1] + scale_z/2
    
    # scale
    xlim <- c(mid_x - max_scale/2, mid_x + max_scale/2)
    ylim <- c(mid_y - max_scale/2, mid_y + max_scale/2)
    zlim <- c(mid_z - max_scale/2, mid_z + max_scale/2)
  }
  
  # plot geographic locations
  plotly::plot_ly(data,
                  x = ~x, y = ~y, z = ~z, 
                  type = 'scatter3d', 
                  mode = 'markers+lines', 
                  marker = list(size = 1.75), 
                  split = ~ID) %>% 
    layout(scene = list(xaxis = list(range = xlim),
                        yaxis = list(range = ylim),
                        zaxis = list(range = zlim),
                        aspectmode = "manual", 
                        aspectratio = list(x = 1, y = 1, z=1)))
}
