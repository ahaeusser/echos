
#' @title Create lagged variables of a numeric vector
#' 
#' @description Create lagged variables of a numeric vector, shifting each 
#'   column back by a given number of observations.
#' 
#' @param y Numeric vector with input data.
#' @param lags Integer vector with the number of lags (in units of observations).
#' 
#' @return ylag Numeric matrix with lagged variables of the input vector.
#' @noRd

create_lags <- function(y, lags) {
  
  # Number of observations
  n_obs <- length(y)
  # Number of lags
  n_lags <- length(lags)
  
  # Preallocate empty matrix for lagged variables
  ylag <- matrix(
    data = 0,
    nrow = n_obs,
    ncol = n_lags
  )
  
  # Lag variables
  for (i in seq_along(lags)) {
    ylag[, i] <- c(rep(NA_real_, lags[i]), y)[1:length(y)]
  }
  
  colnames(ylag) <- paste0(
    rep("lag", times = length(lags)),
    "(",
    unlist(lags),
    ")")
  
  return(ylag)
}



#' @title Create lagged variables of a numeric vector for iterative forecasting
#' 
#' @description Create lagged variables of a numeric vector for iterative 
#'   forecasting, shifting each column back by a given number of observations 
#'   and fill with NAs for the updates.
#' 
#' @param y Numeric vector with input data.
#' @param lags Integer vector with the number of lags (in units of observations).
#' @param n_ahead Integer value. The forecast horizon (n-step ahead).
#' 
#' @return ylag Numeric matrix with the lagged variables of the input data for iterative forecasting.
#' @noRd

create_revolved <- function(y,
                            lags,
                            n_ahead) {
  
  # Number of rows
  n_rows <- (max(lags) + n_ahead + 1)
  # Number of columns
  n_lags <- length(lags)
  
  # Preallocate empty output matrix for lagged variables
  ylag <- matrix(
    data = NA_real_,
    nrow = n_rows,
    ncol = n_lags
  )
  
  # Lag variables
  for (i in seq_along(lags)) {
    lag <- c(0, y[c((length(y) - lags[i] + 1):length(y))])
    length(lag) <- n_rows
    ylag[, i] <- lag
  }
  
  colnames(ylag) <- paste0(
    rep("lag", times = length(lags)),
    "(",
    unlist(lags),
    ")")
  
  return(ylag)
}



#' @title Create the input weight matrix
#' 
#' @description Create the random input weight matrix.
#' 
#' @param n_inputs Integer value. The number of input features.
#' @param n_states Integer value. The number of internal states within the reservoir (reservoir size).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' 
#' @return win List containing the input weight matrices.
#' @noRd

create_win <- function(n_inputs,
                       n_states,
                       scale_runif) {
  
  win <- matrix(
    data = runif(
      n = n_states * n_inputs,
      min = scale_runif[1],
      max = scale_runif[2]),
    nrow = n_states,
    ncol = n_inputs
    )
  
  return(win)
}



#' @title Create the reservoir weight matrix
#' 
#' @description Create the random reservoir weight matrix (scaled to spectral 
#'   radius rho).
#' 
#' @param n_states Integer value. The number of internal states within the reservoir (reservoir size).
#' @param rho Numeric value. The spectral radius for scaling the weight matrix.
#' @param density Numeric value. The parameter defines the connectivity of the reservoir weight matrix (dense or sparse).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param symmetric Logical value. If \code{TRUE}, the matrix is symmetric.
#' 
#' @return wres Numeric matrix. The final reservoir weight matrix.
#' @noRd

create_wres <- function(n_states,
                        rho,
                        density,
                        scale_runif,
                        symmetric = FALSE) {
  
  # Create initial random weight matrix for the reservoir
  wres <- matrix(
    data = runif(
      n = n_states * n_states,
      min = scale_runif[1],
      max = scale_runif[2]),
    nrow = n_states,
    ncol = n_states
    )
  
  # Create a random sparse pattern matrix with defined density
  wsparse <- rsparsematrix(
    nrow = n_states,
    ncol = n_states,
    density = density,
    rand.x = NULL,
    symmetric = symmetric
    )
  
  wres <- as.matrix(wres * wsparse)
  
  # Calculate the absolute, maximum eigenvalue of the reservoir weight matrix
  eig <- eigen(wres, symmetric = symmetric, only.values = TRUE)$values
  max_abs_eig <- max(abs(eig))
  
  # Rescale the reservoir weight matrix to spectral radius rho
  wres <- 1 / max_abs_eig * wres * rho
  return(wres)
}



#' @title Helper function to concatenate a string and sequential integers.
#' 
#' @description Concatenate a string \code{x} and sequential integers up 
#'   to \code{n} (e.g. test(1), test(2), ..., test(n)).
#'
#' @param x Character value.
#' @param n Integer value.
#'
#' @return x Character vector.
#' @noRd

paste_names <- function(x, n) {
  x <- paste0(
    x,"(",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"),
    ")")
  
  return(x)
}



#' @title Calculate the nth-difference of a numeric vector
#'
#' @description Takes a numeric vector as input and calculates
#'    the nth-difference.
#'
#' @param y Numeric vector.
#' @param n_diff Integer value. The number of differences.
#'
#' @return yd Numeric vector with the differenced data.
#' @noRd

diff_vec <- function(y, n_diff) {
  
  # Calculate n-th difference
  if (n_diff > 0) {
    yd <- diff(
      x = y,
      differences = n_diff,
      lag = 1L
      )
  } else {
    yd <- y
  }
  
  # Pad vector with leading NAs
  yd <- c(rep(NA_real_, n_diff), yd)
  return(yd)
}



#' @title Integrate differences of a numeric vector ("inverse difference")
#'
#' @description Takes a numeric vector as input and integrates
#'    (non-seasonal) differences ("inverse difference").
#'
#' @param y Numeric vector containing the original data.
#' @param yd Numeric vector containing the differenced data.
#' @param n_diff Integer value. The number of (non-seasonal) differences.
#'
#' @return y_int Numeric vector with the inverse differenced data.
#' @noRd

inv_diff_vec <- function(y,
                         yd,
                         n_diff) {
  
  y <- as.numeric(y)
  yd <- as.numeric(na.omit(yd))
  
  # Forecast horizon
  n_ahead <- length(yd)
  
  # Starting value for integration
  yi <- tail(y, n_diff)
  
  if (n_diff > 0) {
    # Integrate differenced data
    y_int <- diffinv(
      x = yd,
      lag = 1L,
      differences = n_diff,
      xi = yi
      )
  } else {
    # No differences
    y_int <- yd
  }
  
  # Cut y_int to previous length
  y_int <- y_int[c((length(y_int) - n_ahead + 1):length(y_int))]
  
  return(y_int)
}



#' @title Scale a numeric vector
#' 
#' @description Scale a numeric vector to a specific interval.
#' 
#' @param y Numeric vector containing the data to be scaled.
#' @param new_range Numeric vector. The range for scaling (first value represents the replacement for the min value, the second is the substitute for the max value).
#' 
#' @return result List with numeric vector (\code{ys}) scaled to the interval and the old range as reference.
#' @noRd

scale_vec <- function(y,
                      new_range = c(-1, 1)) {
  
  # Number of observations
  n_obs <- length(y)
  
  # Minimum and maximum
  min <- min(y, na.rm = TRUE)
  max <- max(y, na.rm = TRUE)
  old_range <- c(min, max)
  
  # Extract the lower and upper bound from interval
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Scale matrix y column wise to new interval
  ys <- lower + ((y - min) * (upper - lower) / (max - min))
  
  result <- list(
    ys = ys,
    old_range = old_range
  )
  
  return(result)
}



#' @title Rescale (inverse scaling) a numeric vector
#' 
#' @description Rescale (inverse scaling) a numeric vector by applying the 
#'   transformation backwards to original range.
#' 
#' @param ys Numeric vector containing the values to be rescaled.
#' @param old_range Numeric vector with ranges (min and max) of original data.
#' @param new_range Numeric vector with new (scaled) interval.
#' 
#' @return y Numeric vector with rescaled values.
#' @noRd

rescale_vec <- function(ys,
                        old_range,
                        new_range) {
  
  # Number of observations
  n_obs <- length(ys)
  
  # Extract maximum and minimum for inverse scaling
  min <- old_range[1]
  max <- old_range[2]
  
  # Extract lower and upper bound
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Inverse normalization to original interval
  y <- ((ys - lower) * (max - min)) / (upper - lower) + min
  
  return(y)
}
