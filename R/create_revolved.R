
#' Create lagged variables of a matrix for iterative forecasting
#' 
#' Create lagged variables of a matrix for iterative forecasting, shifting each column back by a given number of observations and fill with NAs for the updates.
#' 
#' @param data A numeric matrix with lagged variables for forecasting.
#' @param lags A list containing vectors with the number of lags (in units of observations) per variable.
#' @param n_ahead Forecast horizon (n-step ahead).
#' 
#' @return y_lag A numeric matrix with the lagged variables of the input data for iterative forecasting.

create_revolved <- function(data, lags, n_ahead) {
  # Number of input variables
  n_inputs <- ncol(data)
  # Maximum number of lags (overall)
  max_lag <- max(unlist(lags))
  # Number of rows
  n_rows <- (max_lag + n_ahead + 1)
  # Number of lags in total
  n_lags <- sum(lengths(lags))
  # Number of lags by input variable
  n_lags_inputs <- lengths(lags)
  # Names of output variables
  names_outputs <- colnames(data)
  
  # Preallocate empty output matrix for lagged variables
  y_lag <- matrix(
    data = NA_real_,
    nrow = n_rows,
    ncol = n_lags)
  
  # Create matrix with combinations of input variables and lags
  index <- matrix(
    data = c(rep(seq(1, n_inputs),
                 times = n_lags_inputs),
             unlist(lags)),
    ncol = 2)
  
  for (i in seq_len(nrow(index))) {
    x <- data[, index[i, 1]]
    k <- index[i, 2]
    lag <- c(0, x[c((length(x) - k + 1):length(x))])
    length(lag) <- n_rows
    y_lag[, i] <- lag
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