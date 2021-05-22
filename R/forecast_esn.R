
#' @title Forecast a trained Echo State Network
#' 
#' @description Forecast an Echo State Network from a trained model.
#' 
#' @param object An object of class \code{ESN}. The result of a call to \code{train_esn()} or \code{auto_esn()}.
#' @param n_ahead Integer value. The number of periods for forecasting (forecast horizon).
#' @param n_sim Integer value. The number of simulations (number of future sample paths).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param xreg A \code{tsibble} containing exogenous variables.
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{point}: Numeric vector containing the point forecasts.}
#'       \item{\code{sim}: Numeric matrix containing the simulated future sample path.}
#'       \item{\code{states_fcst}: Numeric matrix containing the internal states.}
#'       \item{\code{method}: A \code{list} containing several objects and meta information of the trained ESN (weight matrices, hyperparameters, model metrics, etc.).}
#'       \item{\code{n_ahead}: Integer value. The number of periods for forecasting (forecast horizon).}
#'       \item{\code{n_sim}: Integer value. The number of simulations (number of future sample paths).}
#'       }
#' @export

forecast_esn <- function(object,
                         n_ahead = 12,
                         n_sim = 100,
                         n_seed = 42,
                         xreg = NULL) {
  
  # Pre-processing ============================================================
  
  method <- object$method
  
  # Prepared model data
  yt <- method$model_data$yt
  yr <- method$model_data$yr
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
  const <- method$model_inputs$const
  trend <- method$model_inputs$trend
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
  
  # Create constant (intercept or bias term) as matrix
  if (const == FALSE | is.null(const)) {
    y_const <- NULL
  } else {
    y_const <- create_const(
      n_obs = n_ahead
      )
    
    pad_na <- matrix(
      data = NA_real_,
      nrow = max(unlist(lags)),
      ncol = 1
      )
    
    y_const <- rbind(0, y_const, pad_na)
  }
  
  
  # Create trend term (centered moving average) as matrix
  if (trend == FALSE) {
    y_trend <- NULL
  } else {
    
    y_trend <- create_trend(
      y = yt,
      period = max(fourier[[1]]),
      n_ahead = n_ahead
    )
    
    y_trend <- matrix(
      data = y_trend$point,
      ncol = 1
    )
    
    colnames(y_trend) <- "trend"
    
    pad_na <- matrix(
      data = NA_real_,
      nrow = max(unlist(lags)),
      ncol = 1
    )
    
    y_trend <- rbind(0, y_trend, pad_na)
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_const,
    y_trend,
    y_lag,
    y_fourier,
    xreg
    )
  
  
  # Point forecasts ===========================================================
  
  # Predict trained ESN
  model_pred <- predict_esn(
    win = win,
    wres = wres,
    wout = wout,
    n_ahead = n_ahead,
    alpha = alpha,
    lags = lags,
    inputs = inputs,
    states_train = states_train,
    innov = NULL
    )
  
  # Extract point forecasts
  fcst <- model_pred$fcst
  
  # Extract internal states
  # (first row is last row of training an can be dropped)
  states_fcst <- model_pred$states_fcst[-c(1), ]
  
  # Rescaling of point forecasts
  fcst <- rescale_data(
    data = fcst,
    old_range = old_range,
    new_range = scale_inputs
    )
  
  # Integrate differences
  fcst <- inv_diff_data(
    data = yy,
    data_diff = fcst,
    n_diff = dy
    )
  
  point <- as.numeric(fcst)
  
  # Simulation ================================================================
  
  if (is.null(n_sim)) {
    simulation <- NULL
  } else {
    # Simulate future sample path
    # Set seed for reproducibility
    set.seed(n_seed)
    
    model_sim <- simulate_esn(
      win = win,
      wres = wres,
      wout = wout,
      n_ahead = n_ahead,
      alpha = alpha,
      lags = lags,
      inputs = inputs,
      states_train = states_train,
      error = yr,
      n_sim = n_sim
      )
    
    # Rescaling of simulated sample path
    sim <- lapply(
      model_sim,
      function(model_sim) {
        rescale_data(
          data = model_sim,
          old_range = old_range,
          new_range = scale_inputs
          )
      })
    
    # Integrate differences
    sim <- lapply(
      sim,
      function(sim) {
        inv_diff_data(
          data = yy,
          data_diff = sim,
          n_diff = dy
          )
      })
    
    # Flatten list column wise to matrix
    sim <- do.call(cbind, sim)
    
    # Names of simulations
    colnames(sim) <- paste0(
      "sim","(",
      formatC(
        x = 1:n_sim,
        width = nchar(max(n_sim)),
        flag = "0"),
      ")")
  }
  
  # Post-processing ===========================================================
  
  # Output model
  structure(
    list(
      point = point,
      sim = sim,
      states_fcst = states_fcst,
      method = method,
      n_ahead = n_ahead,
      n_sim = n_sim),
    class = "forecast_esn"
    )
}
