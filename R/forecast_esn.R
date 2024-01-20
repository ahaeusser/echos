
#' @title Forecast a trained Echo State Network
#' 
#' @description Forecast an Echo State Network from a trained model.
#' 
#' @param object An object of class \code{esn}. The result of a call to \code{train_esn()}.
#' @param n_ahead Integer value. The number of periods for forecasting (i.e. forecast horizon).
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{point}: Numeric vector containing the point forecasts.}
#'       \item{\code{actual}: Numeric vector containing the actual values.}
#'       \item{\code{fitted}: Numeric vector containing the fitted values.}
#'       \item{\code{n_ahead}: Integer value. The number of periods for forecasting (forecast horizon).}
#'       \item{\code{model_spec}: Character value. The model specification as string.}
#'       }
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' xfcst <- forecast_esn(xmodel, n_ahead = 12)
#' plot(xfcst)
#' @export

forecast_esn <- function(object,
                         n_ahead = 18) {
  
  # Pre-processing ============================================================
  
  method <- object$method
  
  # Prepared model data
  yt <- method$model_data$yt
  yy <- method$model_data$yy
  n_total <- length(yy)
  
  # Number of inputs, internal states within the reservoir and outputs
  n_inputs <- method$model_layers$n_inputs
  n_states <- method$model_layers$n_states
  n_outputs <- method$model_layers$n_outputs
  
  # Weight matrices for inputs, reservoir and outputs
  win <- method$model_weights$win
  wres <- method$model_weights$wres
  wout <- method$model_weights$wout
  
  # Extract meta data
  scale_inputs <- method$scale_inputs
  old_range <- method$model_meta$old_range
  lags <- method$model_meta$lags
  n_diff <- method$model_meta$n_diff
  alpha <- method$model_meta$alpha
  model_spec <- method$model_spec
  
  # Internal states and leakage rate
  states_train <- object$states_train
  model_object <- method$model_object
  
  # Create input layer ========================================================
  
  # Create lagged variables as matrix ("revolved style")
  ylag <- create_revolved(
    y = as.numeric(yt),
    lags = lags,
    n_ahead = n_ahead
  )
  
  # Concatenate input matrices
  inputs <- ylag
  
  # Predict trained model
  point <- predict_esn(
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
  
  point <- as.numeric(point)
  
  # Rescaling of point forecasts
  point <- rescale_vec(
    ys = point,
    old_range = old_range,
    new_range = scale_inputs
  )
  
  # Integrate differences
  point <- inv_diff_vec(
    y = yy,
    yd = point,
    n_diff = n_diff
  )
  
  # Extract actual and fitted from object
  actual <- object[["actual"]]
  fitted <- object[["fitted"]]
  
  # Post-processing ===========================================================
  
  # Output model
  structure(
    list(
      point = point,
      actual = actual,
      fitted = fitted,
      n_ahead = n_ahead,
      model_spec = model_spec),
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
  n_lags <- length(lags)
  
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
    for (j in seq_len(n_lags)) {
      i <- min(which(is.na(inputs[, j])))
      inputs[i, j] <- fcst[(t-1), 1]
    }
  }
  
  return(fcst)
}
