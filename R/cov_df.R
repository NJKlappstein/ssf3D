##' Create covariate dataframe
##' 
##' @param formula model formula with covariate names
##' @param cov_data list of covariate rasters
##' @param data location data (either matrix or dataframe with x, y)
##' @param dim how many dimensions is the covariate data (2 or 3)

cov_df <- function(formula, cov_data, data, dim = 2) { 
  
  if(is.data.frame(data)) {
    xyz <- matrix(c(data$x, data$y, data$z), ncol = 3)
  }
  
  # get non-movement covariates
  pattern <-  paste(c("step", "omega", "omega_delta", "\\^2", "vbear"), collapse="|")
  cov_names <- attr(stats::terms(formula), "term.labels") # covariate names
  cov_names <- cov_names[which(grepl(pattern, cov_names) == F)]
  
  # extract covariates
  if(length(cov_names) >= 1) { 
    
    if(dim == 2) {
      for(j in 1:length(cov_names)){
        cov_layer <- cov_data[[which(names(cov_data) == cov_names[j])]]
        cov_extract <- terra::extract(cov_layer, xyz[,c(1,2)], method = "bilinear")
        names(cov_extract) <- cov_names[j]
        data <- cbind(data, cov_extract)
      }
    }
    
    if(dim == 3) {
      for(j in 1:length(cov_names)){
        cov_layer <- cov_data[[which(names(cov_data) == cov_names[j])]]
        cov_extract <- data.frame(extract_3d(raster_stack = cov_layer, points = xyz))
        names(cov_extract) <- cov_names[j]
        data <- cbind(data, cov_extract)
      }
    }
  } 
  
  if(any(cov_names == "undgrnd")) {
    data$height <- data$z - data$undgrnd
    data$undgrnd <- ifelse(data$height <= 0, 1, 0)
  }
  return(data)
}
