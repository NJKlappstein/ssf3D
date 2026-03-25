
#' Function to plot estimated relative selection strength
#' 
#' @param data data frame with columns: x, y, z, ID
#' @param model fitted ssf
#' @param variable which variable to plot
#' @param log whether or not to plot the rss on the log scale
#' 
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line theme_bw xlab ylab geom_histogram


plot_rss <- function(model, data, variable, log = FALSE) {
  
  # idenitfy covariate range from data
  variable_data <- data[which(data$obs == 1), variable]
  
  # get limits for plot
  variable_grid <- seq(min(variable_data), max(variable_data), length.out = 1000)
  
  # get beta and uncertainty
  betas <- data.frame(beta = coefficients(model)[variable], 
                      lower = confint(model, parm = variable)[1], 
                      upper = confint(model, parm = variable)[2])
  
  # predict
  pred_df <- data.frame(variable = variable_grid, 
                        prediction = betas$beta * variable_grid, 
                        lower = betas$lower * variable_grid, 
                        upper = betas$upper * variable_grid)
  
  # plot
  if(log == FALSE) {
    p <- ggplot(pred_df, aes(x = variable, y = exp(prediction))) +
      geom_ribbon(aes(ymin = exp(lower), ymax = exp(upper)), alpha = 0.3) +
      geom_line() +
      theme_bw() +
      xlab(variable) +
      ylab("relative selection strength")
  }
  if(log == TRUE) {
    p <- ggplot(pred_df, aes(x = variable, y = prediction)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      geom_line() +
      theme_bw() +
      xlab(variable) +
      ylab("log relative selection strength")
  }
  return(p)
}



#' Function to plot estimated gamma distribution of step lengths
#' 
#' @param data data frame with columns: x, y, z, ID
#' @param model fitted ssf
#' @param obs whether or not to plot a histogram of the observed data
#' 
#' @export


plot_gamma <- function(model, data, obs_data = TRUE) {
  
  # idenitfy covariate range from data
  obs <- data[which(data$obs == 1),]
  step_data <- obs[, "step"]
  
  # get limits for plot
  step_grid <- seq(min(step_data), max(step_data), length.out = 1000)
  
  # get gamma parameters
  beta <- correct_betas(model = model, data = data)
  gamma_par <- transform_betas(beta_step = beta["step"], beta_logstep = beta["log(step)"])
  
  # predict
  pred_df <- data.frame(step = step_grid, 
                        density = dgamma(x = step_grid, 
                                         shape = gamma_par["shape"], 
                                         scale = gamma_par["scale"]))
  
  if(obs_data == TRUE) {
  p <- ggplot(obs, aes(x = step, y = ..density..)) +
    geom_histogram(fill = "grey75", color = "white") + 
    geom_line(aes(x = step, y = density), data = pred_df) +
    theme_bw() +
    xlab("step length")
  }
  if(obs_data == FALSE) {
    p <- ggplot(pred_df, aes(x = step, y = density)) +
      geom_line() +
      theme_bw() +
      xlab("step length")
  }
  
  return(p)
}
