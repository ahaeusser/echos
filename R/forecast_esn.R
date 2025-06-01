
#' @title Forecast an Echo State Network
#' 
#' @description Forecast an Echo State Network (ESN) from a trained model via
#'    recursive forecasting.
#' 
#' @param object An object of class \code{esn}. The result of a call to \code{train_esn()}.
#' @param n_ahead Integer value. The number of periods for forecasting (i.e. forecast horizon).
#' @param levels Integer vector. The levels of the forecast intervals, e.g., 80\% and 95\%.
#' @param n_sim Integer value. The number of future sample path generated during simulation.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{point}: Numeric vector containing the point forecasts.}
#'       \item{\code{interval}: Numeric matrix containing the forecast intervals.}
#'       \item{\code{sim}: Numeric matrix containing the simulated future sample path.}
#'       \item{\code{levels}: Integer vector. The levels of the forecast intervals.}
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
                         n_ahead = 18,
                         levels = c(80, 95),
                         n_sim = 100,
                         n_seed = 42) {
  
  # Pre-processing ============================================================
  
  method <- object$method
  
  # Prepared model data
  yt <- method$model_data$yt
  yy <- method$model_data$yy
  yr <- method$model_data$yr
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
  
  # Set seed for reproducibility
  set.seed(n_seed)
  
  # Point forecasts -----------------------------------------------------------
  point <- predict_esn(
    win = win,
    wres = wres,
    wout = wout,
    alpha = alpha,
    n_states = n_states,
    n_ahead = n_ahead,
    lags = lags,
    inputs = inputs,
    states_train = states_train,
    innov = NULL
  )
  # Convert matrix to numeric
  point <- as.numeric(point)
  
  # Rescaling point forecasts
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
  
  # Forecast intervals --------------------------------------------------------
  if (is.null(n_sim)) {
    interval <- NULL
    sim <- NULL
  } else {
    # Preallocate empty matrix to store future sample paths
    sim <- matrix(
      data = NA_real_,
      nrow = n_ahead,
      ncol = n_sim
    )
    
    # Demean residuals to remove bias
    yr <- yr - mean(yr, na.rm = TRUE)
    # Determine block size
    n_size <- floor(length(yr)^(1/3))
    
    # Preallocate innovations via moving block bootstrap
    innov <- moving_block(
      x = yr, 
      n_ahead = n_ahead, 
      n_sim = n_sim, 
      n_size = n_size
    )
    
    for(s in seq_len(n_sim)){
      
      # Predict trained model
      path <- predict_esn(
        win = win,
        wres = wres,
        wout = wout,
        alpha = alpha,
        n_states = n_states,
        n_ahead = n_ahead,
        lags = lags,
        inputs = inputs,
        states_train = states_train,
        innov = innov[s, ]
      )
      # Convert matrix to numeric
      path  <- as.numeric(path)
      
      # Rescaling point forecasts
      path <- rescale_vec(
        ys = path,
        old_range = old_range,
        new_range = scale_inputs
      )
      
      # Integrate differences
      path <- inv_diff_vec(
        y = yy,
        yd = path,
        n_diff = n_diff
      )
      sim[, s] <- path
    }
    
    # Convert central levels to lower/upper quantile probabilities
    probs <- sort(unique(c(0.5 - levels/200, 0.5 + levels/200)))
    # Estimate quantiles row-wise and adjust column names
    interval <- rowQuantiles(x = sim, probs = probs)
    
    colnames(interval) <- c(
      sprintf("lower(%02d)", levels), 
      sprintf("upper(%02d)", levels)
    )
  }
  
  # Extract actual and fitted from object
  actual <- object[["actual"]]
  fitted <- object[["fitted"]]
  
  # Post-processing ===========================================================
  
  # Output model
  structure(
    list(
      point = point,
      interval = interval,
      sim = sim,
      levels = levels,
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
#' @param innov Numeric vector. The innovations (i.e., residuals) used for bootstrapping and simulation.
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
                        states_train,
                        innov = NULL) {
  
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
    
    # Calculate point forecast (1-step-ahead prediction)
    mu <- Xf %*% wout
    
    # add bootstrapped innovation if supplied
    if(!is.null(innov)) mu <- mu + innov[t-1]
    
    fcst[t-1, ] <- mu
    
    # propagate WITH shock
    for (j in seq_len(n_lags)) {
      i <- min(which(is.na(inputs[, j])))
      inputs[i, j] <- mu
    }
    
  }
  
  return(fcst)
}
