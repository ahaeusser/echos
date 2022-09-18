
#' @title Forecast a trained Echo State Network
#' 
#' @description Forecast an Echo State Network from a trained model.
#' 
#' @param object An object of class \code{ESN}. The result of a call to \code{train_esn()} or \code{auto_esn()}.
#' @param n_ahead Integer value. The number of periods for forecasting (forecast horizon).
#' @param xreg A \code{tsibble} containing exogenous variables.
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{point}: Numeric vector containing the point forecasts.}
#'       \item{\code{method}: A \code{list} containing several objects and meta information of the trained ESN (weight matrices, hyperparameters, model metrics, etc.).}
#'       \item{\code{n_ahead}: Integer value. The number of periods for forecasting (forecast horizon).}
#'       }
#' @export

forecast_esn <- function(object,
                         n_ahead = 12,
                         xreg = NULL) {
  
  # Pre-processing ============================================================
  
  method <- object$method
  
  # Prepared model data
  yt <- method$model_data$yt
  xx <- method$model_data$xx
  yy <- method$model_data$yy
  n_total <- nrow(yy)
  
  # Number of inputs, internal states within the reservoir and outputs
  n_inputs <- method$model_layers$n_inputs
  n_states <- method$model_layers$n_states
  n_outputs <- method$model_layers$n_outputs
  
  # Weight matrices for inputs, reservoir and outputs
  win <- method$model_weights$win
  wres <- method$model_weights$wres
  wout <- method$model_weights$wout
  
  # Arguments for differencing and scaling the time series data
  scale_inputs <- method$scale_inputs
  old_range <- method$model_inputs$old_range
  dy <- method$model_inputs$dy
  dx <- method$model_inputs$dx
  
  # Internal states and leakage rate
  states_train <- object$states_train
  model_object <- method$model_object
  
  # Model inputs
  lags <- method$model_inputs$lags
  fourier <- method$model_inputs$fourier
  
  
  # Create input layer ========================================================
  
  if (!is.null(xreg)) {
    # Convert tsibble to numeric matrix
    xreg <- invoke(cbind, unclass(xreg)[measured_vars(xreg)])
    xreg <- rbind(xx, xreg)
    
    if (is.null(dx)) {dx <- 0}
    
    # Calculate nth-differences of exogenous variables
    xreg <- diff_data(
      data = xreg,
      n_diff = dx
    )
    
    # Scale data to specified interval
    scaled <- scale_data(
      data = xreg,
      new_range = scale_inputs
    )
    
    xreg <- tail(
      x = scaled$data,
      n = n_ahead
    )
    
    pad_na <- matrix(
      data = NA_real_,
      nrow = max(unlist(lags)),
      ncol = ncol(xreg)
    )
    
    xreg <- rbind(0, xreg, pad_na)
    rownames(xreg) <- NULL
  }
  
  # Create lagged variables as matrix ("revolved style")
  if (is.null(lags)) {
    y_lag <- NULL
  } else {
    y_lag <- create_revolved(
      data = yt,
      lags = lags,
      n_ahead = n_ahead
    )
  }
  
  # Create fourier terms as matrix
  if (is.null(fourier)) {
    y_fourier <- NULL
  } else {
    
    # Create numeric matrix of fourier terms
    y_fourier <- create_fourier(
      x = (n_total + 1):(n_total + n_ahead),
      period = fourier[[1]],
      k = fourier[[2]]
    )
    
    pad_na <- matrix(
      data = NA_real_,
      nrow = max(unlist(lags)),
      ncol = ncol(y_fourier)
    )
    
    y_fourier <- rbind(0, y_fourier, pad_na)
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_lag,
    y_fourier,
    xreg
  )
  
  # Predict trained model
  model_fcst <- predict_esn(
    win = win,
    wres = wres,
    wout = wout,
    alpha = alpha,
    n_states = n_states,
    n_ahead = n_ahead,
    lags = lags,
    inputs = inputs,
    states_train = states_train
  )
  
  # Rescaling of point forecasts
  model_fcst <- rescale_data(
    data = model_fcst,
    old_range = old_range,
    new_range = scale_inputs
  )
  
  # Integrate differences
  model_fcst <- inv_diff_data(
    data = yy,
    data_diff = model_fcst,
    n_diff = dy
  )
  
  point <- as.numeric(model_fcst)
  
  # Post-processing ===========================================================
  
  # Output model
  structure(
    list(
      point = point,
      model_fcst = model_fcst,
      method = method,
      n_ahead = n_ahead),
    class = "forecast_esn"
    )
}


#' @title Forecast a trained Echo State Network (internal function)
#' 
#' @description Calculate point forecasts of a trained Echo State Network
#'   (internal function).
#' 
#' @param win Numeric matrix. Weights for the input variables.
#' @param wres Numeric matrix. Weights for the reservoir.
#' @param wout Numeric matrix. Weights for output variables (estimated coefficients from ridge regression).
#' @param alpha Numeric value. The Leakage rate (smoothing parameter).
#' @param n_states Integer value. The number of internal states.
#' @param n_ahead Integer value. The forecast horizon (n-step ahead).
#' @param lags List containing integer vectors with the lags associated with each output variable.
#' @param inputs Numeric matrix. Initialized input features (gets updated during forecasting process).
#' @param states_train Numeric matrix. Internal states from training (necessary due to last values).
#' 
#' @return Numeric matrix containing the forecasts.
#' @noRd

predict_esn <- function(win,
                        wres,
                        wout,
                        alpha,
                        n_states,
                        n_ahead,
                        lags,
                        inputs,
                        states_train) {
  
  # names of predictor variables (excluding intercept term)
  states <- rownames(wout)[-1]
  
  # Preallocate empty matrices to store point forecasts and internal states
  fcst <- matrix(
    data = NA_real_,
    nrow = n_ahead,
    ncol = 1,
    dimnames = list(c(), colnames(wout))
  )
  
  states_fcst_upd <- matrix(
    data = NA_real_,
    nrow = (n_ahead + 1),
    ncol = (n_states),
    dimnames = list(c(), colnames(states_train))
  )
  
  # Create copy and fill first row with last values from states_train
  states_fcst <- states_fcst_upd
  states_fcst[1, ] <- states_train[nrow(states_train), , drop = FALSE]
  
  # Number of lags by output variable
  n_lags <- lapply(lags, length)
  
  # Names of lagged variables as list
  names_lags_list <- lapply(
    seq_len(ncol(wout)),
    function(n) {
      paste(colnames(wout)[n], "(", lags[[n]], ")", sep = "")
    }
  )
  
  # Dynamic forecasting (iterative mode)
  for (t in 2:(n_ahead + 1)) {
    
    # Calculate new internal states
    states_fcst_upd[t, ] <- t(tanh(win %*% t(inputs[t, , drop = FALSE]) + wres %*% t(states_fcst[(t - 1), , drop = FALSE])))
    states_fcst[t, ] <- alpha * states_fcst_upd[t, , drop = FALSE] + (1 - alpha) * states_fcst[(t - 1), , drop = FALSE]
    
    # Prepare design matrix
    Xf <- states_fcst[t, states, drop = FALSE]
    Xf <- cbind(1, Xf)
    
    # Calculate point forecast
    fcst[(t-1), ] <- Xf %*% wout
    
    # Update lagged variables in inputs
    for (i in seq_len(ncol(wout))) {
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
  
  return(fcst)
}
