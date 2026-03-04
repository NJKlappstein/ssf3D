# load functions
source("code/util.R")
sourceDir("code/")

library(Directional)
library(dplyr)
library(ggplot2)
library(mgcv)
library(survival)
library(tictoc)
library(miniSSF)
library(terra)
theme_set(theme_bw())

# load raster stack
r_stack <- rast("data/3D_raster.tif")
depth(r_stack) <- 1:nlyr(r_stack)

#  choose step length distribution
shape <- 2
scale <- 2
step_par <- transform_par(shape, scale)

# choose angular distribution
kappa <- 10
rho <- 4.9

# simulation settings
n_zeros <- 1000
n_locs <- 1000
n_tracks <- 10
n_per_track <- n_locs / n_tracks

# ssf formula
formula <- ~ step + log(step) + cos(omega) + omega_delta + cov1
betas <- c(step_par[1:2], kappa, rho, 3)
sampling_par <- c(shape = shape, scale = scale, kappa = kappa, rho = rho)

set.seed(50)
xyz <- NULL
for(track in 1:n_tracks) {
  xyz0 <- matrix(c(runif(2, 200, 300), runif(1, 75, 125)), ncol = 3)
  
  # simulate data
  xyz_sub <- sim_ssf(xyz0, 
                     n_zeros, 
                     n_per_track, 
                     formula, 
                     betas, 
                     print = TRUE, 
                     pts_dist = "gamma+kent", 
                     cov_3D = list(cov1 = r_stack))
  # format data
  xyz_sub$ID <- track
  xyz <- rbind(xyz, xyz_sub)
}

# assign times
xyz$time <- seq(from = as.POSIXct("2024-01-01 12:00"), 
                length.out = n_locs, 
                by = "hour")



head(xyz)
data0 <- data.frame(ID = xyz$ID, 
                    x = xyz$x, y = xyz$y, z = xyz$z, 
                    time = xyz$time)
head(data0)

# function to plot 3D track
plot_data(data0, scale = TRUE)

# testing prep_data
data1 <- prep_data(data0, ssf = FALSE)
head(data1)
data2 <- prep_data(data0)
head(data2)


plot_geodesics(data = data1, format = "df")
plot_bearings(data = data1, format = "df")



write.csv(data0, "data/sim_data.csv", row.names = FALSE)




