#' Correct estimated parameters for a gamma step length distribution 
#' and (potentially) a Kent arc distribution
#' 
#' @param model fitted model output, as returned by survival::clogit()
#' @param data data used to fit model
#' 
#' @return Corrected model coefficients
#' 
#' @export

correct_betas <- function(model, data) {
  # estimated betas
  beta_est <- coefficients(fit)
  
  # figure out how points were sampled (i.e., what needs correcting)
  atts <- c(is.null(attributes(data)$shape), 
            is.null(attributes(data)$kappa),
            is.null(attributes(data)$rho)) 
  
  # set up
  if(atts[1] == TRUE & atts[2] == TRUE & atts[3] == TRUE) {
    dist <- "uniform"
  }
  if(atts[1] == FALSE & atts[2] == TRUE & atts[3] == TRUE) {
    dist <- "gamma"
    sampling_par <- c(shape = attributes(data)$shape, 
                      scale =  attributes(data)$scale)
  }
  if(atts[1] == FALSE & atts[2] == FALSE & atts[3] == FALSE) {
    dist <- "gamma+kent"
    sampling_par <- c(shape = attributes(data)$shape, 
                      scale =  attributes(data)$scale, 
                      kappa =  attributes(data)$kappa, 
                      rho =  attributes(data)$rho)
  }
  
  if(dist == "uniform") {
    coef <- beta_est
  }
  
  # correct parametres 
  if(dist == "gamma") {
    # correct betas 
    beta_step <- beta_est["step"] - (1 / sampling_par["scale"])
    beta_logstep <- beta_est["log(step)"] + sampling_par["shape"] - 3
    # return corrected parameters
    ind <- which(names(beta_est) == "step" | names(beta_est) == "log(step)")
    coef <- c(beta_step, beta_logstep, beta_est[-ind])
  }
  
  # correct parametres 
  if(dist == "gamma+kent") {
    # correct betas
    beta_step <- beta_est["step"] - (1 / sampling_par["scale"])
    beta_logstep <- beta_est["log(step)"] + sampling_par["shape"] - 3
    kappa <- beta_est["cos(omega)"] + sampling_par["kappa"]
    rho <- beta_est["omega_delta"] + sampling_par["rho"]
    # return corrected parameters
    ind <- which(names(beta_est) == "step" | names(beta_est) == "log(step)" |
                   names(beta_est) == "cos(omega)" | names(beta_est) == "omega_delta")
    coef <- c(beta_step, beta_logstep, kappa, rho, beta_est[-ind])
  }
  return(coef)
}
