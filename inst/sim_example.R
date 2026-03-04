# load functions
source("code/util.R")
sourceDir("code/")
library(terra)
library(lubridate)
library(survival)

# load location data
data <- read.csv("data/sim_data.csv")

# load 3D environmental data
r_stack <- rast("data/3D_raster.tif")
depth(r_stack) <- 1:nlyr(r_stack)

# prepare data for SSF
data$time <- ymd_hms(data$time)
data <- prep_data(data = data, 
                  n_random = 50, 
                  distr = "gamma")

# extract covariates
data$cov <- extract_3d(r_stack, data[,c("x", "y", "z")])
data$omega_delta <- sin(data$omega) * sin(data$omega) * cos(2 * data$delta)

# fit model
fit <- clogit(obs ~ step + log(step) + 
                cos(omega) + omega_delta + 
                cov +
                strata(stratum), 
              data = data)

correct_betas(model = fit, data = data)

