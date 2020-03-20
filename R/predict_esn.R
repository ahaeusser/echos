
#' Forecast a fitted ESN
#' 
#' Calculate point forecasts for a fitted ESN
#' 
#' @param win Weights matrix for input variables
#' @param wres Weights matrix for reservoir
#' @param wout Weights matrix for output variables (coefficients from ridge regression)
#' @param n_ahead Number of periods for forecasting (forecast horizon)
#' @param alpha Leakage rate (smoothing parameter)
#' @param lags A numeric matrix with lagged variables
#' @param inputs Initialized input features (gets updated during forecasting process)
#' @param states_train Internal states from training (necessary for last values)
#' @param innov Innovations for a simulation (default is NULL)
#' 
#' @return fcst A numeric matrix with point forecasts
#' @return states_fcst A numeric matrix with internal states used for forecasting

predict_esn <- function(win,
                        wres,
                        wout,
                        n_ahead,
                        alpha,
                        lags,
                        inputs,
                        states_train,
                        innov = NULL) {
  
  # Number of output variables and internal states (reservoir size)
  n_outputs <- ncol(wout)
  n_res <- nrow(wres)
  
  # Preallocate empty matrices to store point forecasts and internal states
  fcst <- matrix(
    data = NA_real_,
    nrow = n_ahead,
    ncol = n_outputs,
    dimnames = list(c(), colnames(wout)))
  
  states_fcst_upd <- matrix(
    data = NA_real_,
    nrow = (n_ahead + 1),
    ncol = (n_res),
    dimnames = list(c(), colnames(states_train))
  )
  
  # Create copy and fill first row with last values from states_train
  states_fcst <- states_fcst_upd
  states_fcst[1, ] <- states_train[nrow(states_train), ]
  
  # Number of lags by output variable
  n_lags <- lapply(lags, length)
  # Names of lagged variables as list
  names_lags_list <- lapply(seq_len(n_outputs), function(n) {
    paste(colnames(wout)[n], "(", lags[[n]], ")", sep = "")
  })
  
  # Dynamic forecasting (iterative mode)
  for (t in 2:(n_ahead + 1)) {
    # Calculate new internal states
    states_fcst_upd[t, ] <- t(tanh(win %*% t(inputs[t, , drop = FALSE]) + wres %*% t(states_fcst[(t - 1), , drop = FALSE])))
    states_fcst[t, ] <- alpha * states_fcst_upd[t, , drop = FALSE] + (1 - alpha) * states_fcst[(t - 1), , drop = FALSE]
    
    # Prepare design matrix
    X <- cbind(inputs[t, , drop = FALSE], states_fcst[t, , drop = FALSE])
    
    # Calculate point forecasts and save values
    if (is.null(innov)) {
      fcst[(t - 1), ] <- X %*% wout
    } else {
      fcst[(t - 1), ] <- X %*% wout + innov[(t - 1), , drop = FALSE]
    }
    
    # Update lagged variables in inputs
    for (i in seq_len(n_outputs)) {
      # Column index for block-wise looping and updating (by variable)
      index_col <- names_lags_list[[i]]
      # Row index for block-wise looping and updating (by variable)
      index_row <- inputs[, index_col, drop = FALSE]
      index_row <- as.numeric(apply(index_row, MARGIN = 2, FUN = function(x) min(which(is.na(x)))))
      for (ii in seq_len(n_lags[[i]])) {
        inputs[index_row[ii], index_col[ii]] <- fcst[(t-1), i]
      }
    }
  }
  
  # Store and return results
  result <- list(
    fcst = fcst,
    states_fcst = states_fcst)
  
  return(result)
}