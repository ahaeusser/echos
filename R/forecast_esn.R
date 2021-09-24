
#' @title Forecast a trained Echo State Network
#' 
#' @description Forecast an Echo State Network from a trained model.
#' 
#' @param object An object of class \code{ESN}. The result of a call to \code{train_esn()} or \code{auto_esn()}.
#' @param n_ahead Integer value. The number of periods for forecasting (forecast horizon).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
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
                         n_seed = 42,
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
  n_res <- method$model_layers$n_res
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
  alpha <- method$model_pars$alpha
  
  # Model inputs
  lags <- method$model_inputs$lags
  fourier <- method$model_inputs$fourier
  
  model_object <- method$model_object
  
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
  
  # Predict trained models
  model_fcst <- map(
    .x = 1:length(model_object),
    .f = ~{
      predict_esn(
        win = win,
        wres = wres,
        wout = wout,
        model_object = model_object[[.x]],
        n_ahead = n_ahead,
        alpha = alpha,
        lags = lags,
        inputs = inputs,
        states_train = states_train
      )
    }
  )
  
  model_fcst <- do.call(cbind, model_fcst)
  colnames(model_fcst) <- names(model_object)
  point <- as.matrix(rowMeans(model_fcst))
  
  # Rescaling of point forecasts
  point <- rescale_data(
    data = point,
    old_range = old_range,
    new_range = scale_inputs
  )
  
  # Integrate differences
  point <- inv_diff_data(
    data = yy,
    data_diff = point,
    n_diff = dy
  )
  
  point <- as.numeric(point)
  
  # Post-processing ===========================================================
  
  # Output model
  structure(
    list(
      point = point,
      method = method,
      n_ahead = n_ahead),
    class = "forecast_esn"
    )
}
