
#' @title Create lagged variables of a numeric vector
#' 
#' @description Create lagged variables of a matrix, shifting each column
#'   back by a given number of observations.
#' 
#' @param y Numeric vector with input data.
#' @param lags Integer vectors with the number of lags (in units of observations).
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
#' @description Create lagged variables of a matrix for iterative forecasting,
#'   shifting each column back by a given number of observations and fill with NAs for the updates.
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



#' @title Create model specification
#' 
#' @description This function creates the model specification (short summary) as a string.
#'
#' @param model_layers List containing the number of inputs (n_inputs), 
#'   internal states (n_states) and the number of 
#'   outputs (n_outputs).
#'
#' @return model_spec Character value. The model specification as string.
#' @noRd

create_spec <- function(model_layers) {
  
  # Number of input variables
  n_inputs <- model_layers$n_inputs
  # Number of internal states per reservoir
  n_states <- model_layers$n_states
  # Number of output variables
  n_outputs <- model_layers$n_outputs
  
  # Model specification
  model_spec <- paste0(
    "ESN", "(", 
    n_inputs, ",", 
    n_states, ",", 
    n_outputs, 
    ")")
  
  return(model_spec)
}



#' @title Create the input weight matrix
#' 
#' @description This function creates the random input weight matrices.
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
#' @description This function creates the random reservoir weight matrix
#'    (scaled to spectral radius rho).
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



#' @title Helper function to concatenate a string and one number.
#' 
#' @description Helper function to concatenate a string and one number (e.g.
#'   test(1), test(2), ..., test(n)).
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
#' @description This function takes a numeric vector and calculates
#'    the nth-difference.
#'
#' @param y Numeric vector.
#' @param n Integer value. The number of differences.
#'
#' @return yd Numeric vector with the differenced data.
#' @noRd

diff_vec <- function(y, n) {
  
  # Calculate n-th difference
  if (n > 0) {
    yd <- diff(
      x = y,
      differences = n,
      lag = 1L
      )
  } else {
    yd <- y
  }
  
  # Pad vector with leading NAs
  yd <- c(rep(NA_real_, n), yd)
  return(yd)
}



#' @title Integrate differences of a numeric vector ("inverse difference")
#'
#' @description This function takes a numeric vector and integrates
#'    (non-seasonal) differences ("inverse difference").
#'
#' @param y Numeric vector containing the original data.
#' @param y_diff Numeric vector containing the differenced data.
#' @param n_diff Integer value. The number of non-seasonal differences.
#'
#' @return y_int Numeric vector with the inverse differenced data.
#' @noRd

inv_diff_vec <- function(y,
                         y_diff,
                         n_diff) {
  
  y <- as.numeric(y)
  y_diff <- as.numeric(na.omit(y_diff))
  
  # Forecast horizon
  n_ahead <- length(y_diff)
  
  # Starting value for integration
  yi <- tail(y, n_diff)
  
  if (n_diff > 0) {
    # Integrate differenced data
    y_int <- diffinv(
      x = y_diff,
      lag = 1L,
      differences = n_diff,
      xi = yi
      )
  } else {
    # No differences
    y_int <- y_diff
  }
  
  # Cut y_int to previous length
  y_int <- y_int[c((length(y_int) - n_ahead + 1):length(y_int))]
  
  return(y_int)
}



#' @title Scale a numeric matrix
#' 
#' @description Scale the columns of a numeric matrix to a specific interval.
#' 
#' @param y Numeric matrix containing the values to be scaled. Each column is a variable and each row an observation.
#' @param new_range Numeric vector. The range for scaling (first value represents the replacement for the min value, the second is the substitute for the max value).
#' 
#' @return data Numeric matrix with scaled columns.
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
#' @description Rescale (inverse scaling) the columns of a numeric matrix
#'    by applying the transformation backwards to original range.
#' 
#' @param ys Numeric matrix containing the values to be rescaled. Each column is a variable and each row an observation.
#' @param old_range Numeric matrix with ranges (min and max) of original data.
#' @param new_range Numeric vector with new (scaled) interval.
#' 
#' @return y Numeric matrix with rescaled columns.
#' @noRd

rescale_vec <- function(ys,
                        old_range,
                        new_range) {
  
  # Number of observations
  n_obs <- length(y)
  
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
