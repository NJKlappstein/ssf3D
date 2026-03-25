# load functions
source("code/util.R")
sourceDir("code/")

library(Directional)
library(ggplot2)
library(survival)
theme_set(theme_bw())

#  choose step length distribution
shape <- 2
scale <- 2


######################
#### random walk ####
######################

# simulate rw data
rw <- sim_crw(n_locs = 1000,  
               type = "rw", 
               step_par = c(shape, scale), 
              print = FALSE)

plot_data(rw)
plot_data(rw, scale = TRUE)
plot_geodesics(rw)
plot_bearings(rw)

######################
###### vMF crw ######
######################
# concentration
kappa <- 10

# simulate vMF data
vMF <- sim_crw(n_locs = 1000,  
               type = "vMF", 
               step_par = c(shape, scale), 
               kappa = kappa)

plot_data(vMF)
plot_data(vMF, scale = TRUE)
plot_geodesics(vMF)
plot_bearings(vMF)


######################
###### kent crw ######
######################
# concentration
kappa <- 10
rho <- 4.9

# simulate vMF data
kent <- sim_crw(n_locs = 1000,  
               type = "kent", 
               step_par = c(shape, scale), 
               kappa = kappa, 
               rho = rho)

plot_data(kent)
plot_data(kent, scale = TRUE)
plot_geodesics(kent)
plot_bearings(kent)


######################
##### biased crw ######
######################
# concentration
kappa <- 10
kappa2 <- 50

# simulate vMF data
bcrw <- sim_crw(n_locs = 1000,  
                type = "bcrw", 
                step_par = c(shape, scale), 
                kappa = kappa, 
                kappa2 = kappa2)

plot_data(bcrw)
plot_data(bcrw, scale = TRUE)
plot_geodesics(bcrw)
plot_bearings(bcrw)




# fit model
kent$time <- seq(from = as.POSIXct("2024-01-01 12:00"), 
                length.out = nrow(kent), 
                by = "hour")
data <- prep_data(kent, distr = "gamma+kent")
data$omega_delta <- sin(data$omega) * sin(data$omega) * cos(2 * data$delta)

# fit model
fit <- clogit(obs ~ step + log(step) + cos(omega) + omega_delta + strata(stratum), 
              data = data)
fit
betas <- correct_betas(fit, data)
transform_betas(betas[1], betas[2])


