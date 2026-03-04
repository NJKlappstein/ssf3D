#' Calculate difference in time
#' 
#' @param data data frame of locs with ID and time columns
#' @param units units of time (default = "hours")
#' @param dir calculate difference to next or previous loc? (default = "next")
#' 
#' @return Data frame with new 'dt' column
calc_dt <- function(data, units = "hours", dir = "next") {
  #order by time and individual
  data <- data[order(data$ID, data$time),]
  
  if(dir == "next") {
    data$dt <- c(as.numeric(difftime(data$time[2:nrow(data)],
                                     data$time[1:nrow(data)-1], 
                                     units=units)), 
                 NA)
    
    last_locs <- which(data$ID[-nrow(data)] != data$ID[-1])
    data$dt[last_locs] <- NA
  }
  
  if(dir == "prev") {
    data$dt <- c(NA, 
                 as.numeric(difftime(data$time[1:nrow(data)-1],
                                     data$time[2:nrow(data)], 
                                     units = units)))
    
    first_locs <- which(data$ID[-1] != data$ID[-nrow(data)])+1
    data$dt[first_locs] <- NA
  }
  return(data)
}


#' Convert 2D spatial locations to new CRS
#' 
#' @param data data frame of locs with x,y columns
#' @param CRS current CRS of data
#' @param newCRS new CRS
#' 
#' @return Data frame where x and y have been transformed to new CRS
#' 
#' @importFrom sp SpatialPoints spTransform
transformCRS <- function(data, CRS, newCRS) {
  
  #which rows have NAs in the location data
  notNA <- which(!is.na(data$x)) 
  
  #convert location data to spatial points
  coords <- SpatialPoints(coords = data[notNA, c("x", "y")], 
                          proj4string = CRS(CRS))
  
  #transform location data to the new CRS
  coords <- spTransform(x = coords, CRSobj = CRS(newCRS))
  
  #fill in x and y with transformed coordinates
  data$x[notNA] <- attr(coords,"coords")[,1]
  data$y[notNA] <- attr(coords,"coords")[,2]
  
  #return data frame
  return(data)
}


#' Convert SF to data.frame
#'
#' @param data sf (points only) object
#' @param coords vector of X and Y names
#' 
#' @return Data frame with all columns, sf geometry dropped, 
#' and replaced with coord columns 
#'
#' @export
#' @importFrom sf st_geometry_type st_coordinates
sf_to_df <- function(data, coords = c("X","Y")){
  geom_types <- st_geometry_type(data)
  if(all(geom_types == "POINT")){
    df <- st_drop_geometry(data)
    df[,coords] <- st_coordinates(data)
  } else {
    df <- st_coordinates(data) |> as.data.frame()
    names(df)[names(df) %in% c("X","Y")] <- coords
    warning(paste0("All columns except for coordinates are dropped for st ",
                   "geometry types other than 'POINT'."))
  }
  
  return(df)
}


#' Split track at gaps
#' 
#' @param data Data frame with (at least) columns for "ID" and "time"
#' @param max_gap Longest allowed gap, in minutes (track will be split at longer gaps)
#' @param shortest_track Shortest track to keep after splitting, in minutes. Shorter
#' tracks will be removed from the output data set.
#' 
#' @return Data frame with identical structure as input, where ID column
#' has been replaced by new ID for split tracks. Old ID still accessible as
#' ID_old column
split_at_gap <- function(data, max_gap = 60, shortest_track = 0) {
  # Number of tracks
  n_tracks <- length(unique(data$ID))
  
  # Save old ID and reinitialise ID column
  data$ID_old <- data$ID
  data$ID <- character(nrow(data))
  
  # Loop over tracks (i.e., over IDs)
  for(i_track in 1:n_tracks) {
    # Indices for this track
    ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
    track_length <- length(ind_this_track)
    
    # Time intervals in min
    dtimes <- difftime(data$time[ind_this_track[-1]], 
                       data$time[ind_this_track[-track_length]],
                       units = "mins")
    
    # Indices of gaps longer than max_gap
    ind_gap <- c(0, which(dtimes > max_gap), track_length)
    
    # Create new ID based on split track
    subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap))
    data$ID[ind_this_track] <- paste0(data$ID_old[ind_this_track], "-", subtrack_ID)
  }
  
  # Only keep sub-tracks longer than some duration
  track_lengths <- sapply(unique(data$ID), function(id) {
    ind <- which(data$ID == id)
    difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
  })
  ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
  data <- subset(data, ID %in% ID_keep)
  
  return(data)
}

#' Setup sampling of 2D random points
#' 
#' @param distr Name of distribution(s) to generate random points
#' @param obs Data frame of observed positions, used to get empirical
#' distributions of movement variables
#' 
#' @return List of functions and parameters for sampling
setup_samp <- function(distr = "uniform", obs) {
  move_data <- prepData(obs, type = "UTM")
  step <- na.omit(move_data$step)
  angle <- na.omit(move_data$angle)
  
  if(distr[1] == "uniform") {
    # Uniform distribution in 2D (not uniform step lengths!)
    par_step <- c(0, max(step))
    r_step <- function(n, par) {
      sqrt(runif(n, min = par[1], max = par[2]^2))
    }
    d_step <- function(x, par) {
      rep(1, length(x))
    }
  } else if(distr[1] == "gamma") {
    # Gamma distribution of step lengths
    par_step <- c(mean(step), sd(step))
    r_step <- function(n, par) {
      rgamma(n, shape = par[1]^2/par[2]^2, rate = par[1]/par[2]^2)
    }
    d_step <- function(x, par) {
      dgamma(x, shape = par[1]^2/par[2]^2, rate = par[1]/par[2]^2) / x
    }
  } else if(distr[1] == "exp") {
    # Exponential distribution of step lengths
    par_step <- 1/mean(step)
    r_step <- function(n, par){
      rexp(n, rate = par[1])
    }
    d_step <- function(x, par) {
      dexp(x, rate = par[1]) / x
    }
  } else {
    stop("'distr' is not implemented")
  }
  
  if(length(distr) == 1) {
    # By default, uniform distribution of turning angles
    r_angle <- function(n, par) runif(n, -pi, pi)
    d_angle <- function(x, par) 1
    par_angle <- NA
  } else if(length(distr) == 2) {
    if(distr[2] == "vm") {
      par_angle <- unlist(vm.ml(angle))
      r_angle <- function(n, par) {
        rvm(n = n, mean = par[1], k = par[2])
      }
      d_angle <- function(x, par) {
        dvm(theta = x, mu = par[1], kappa = par[2])
      }
    } else if(distr[2] == "wrpcauchy") {
      par_angle <- unlist(wrpcauchy.ml(angle, mu0 = 0, rho0 = 0.5))
      r_angle <- function(n, par) {
        rwrpcauchy(n = n, location = par[1], rho = par[2])
      }
      d_angle <- function(x, par) {
        dwrpcauchy(theta = x, mu = par[1], rho = par[2])
      }
    } else {
      stop("'distr' is not implemented")
    }
  } else {
    step("'distr' should have length 1 or 2")
  }
  
  return(list(r_step = r_step, d_step = d_step, par_step = par_step,
              r_angle = r_angle, d_angle = d_angle, par_angle = par_angle))
}

#' Log-sum-exp transformation
#'
#' @param x vector of values
#'
#' @return log(sum(exp(x)))
logsumexp <- function(x) {
  xmax <- max(x)
  val <- xmax + log(sum(exp(x - xmax)))
  return(val)
}