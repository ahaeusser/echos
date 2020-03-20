
#' Create lagged variables of a matrix
#' 
#' Create lagged variables of a matrix, shifting each column back by a given number of observations
#' 
#' @param data Numeric vector or matrix. Each column is a variable and each row an observation.
#' @param lags List containing vectors with the number of lags (in units of observations) per variable.
#' 
#' @return y_lag A numeric matrix with lagged variables of the input data.

create_lags <- function(data, lags) {
  # Number of input variables
  n_inputs <- ncol(data)
  # Number of observations
  n_obs <- nrow(data)
  # Number of lags in total
  n_lags <- sum(lengths(lags))
  # Number of lags by input variable
  n_lags_inputs <- lengths(lags)
  # Names of output variables
  names_outputs <- colnames(data)
  
  # Preallocate empty matrix for lagged variables
  y_lag <- matrix(
    data = 0,
    nrow = n_obs,
    ncol = n_lags)
  
  # Create matrix with combinations of input variables and lags
  index <- matrix(
    data = c(rep(seq(1, n_inputs),
                 times = n_lags_inputs),
             unlist(lags)),
    ncol = 2)
  
  # Lag variables
  for (i in seq_len(nrow(index))) {
    x <- data[, index[i, 1]]
    k <- index[i, 2]
    y_lag[, i] <- c(rep(NA_real_, k), x)[1:length(x)]
  }
  
  # Names of lagged variables (combination of names_output and lags)
  names_lags <- paste0(
    rep(names_outputs,
        times = n_lags_inputs),
    "(",
    unlist(lags),
    ")")
  
  colnames(y_lag) <- names_lags
  return(y_lag)
}