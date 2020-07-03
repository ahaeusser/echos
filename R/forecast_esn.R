
#' @title Forecast a trained Echo State Network (ESN).
#' 
#' @description Forecast an Echo State Network from a trained model (univariate and multivariate ESNs).
#' 
#' @param object An object of class \code{ESN}. The result of a call to \code{train_esn()} or \code{auto_esn()}.
#' @param n_ahead Integer value. The number of periods for forecasting (forecast horizon).
#' @param n_sim Integer value. The number of simulations (number of future sample paths).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' 
#' @return A list containing:
#' 
#'    \itemize{
#'       \item{\code{forecast}: A \code{tsibble} containing the forecasts.}
#'       \item{\code{simulation}: A \code{tsibble} containing the simulated future sample path.}
#'       \item{\code{actual}: A \code{tsibble} containing the actual values.}
#'       \item{\code{fitted}: A \code{tsibble} containing the fitted values.}
#'       \item{\code{states_train}: A \code{tsibble} containing the internal states.}
#'       \item{\code{method}: A list containing several objects and information of the trained ESN (weight matrices, hyperparameters, model metrics, etc.).}
#'       \item{\code{n_ahead}: Integer value. The number of periods for forecasting (forecast horizon).}
#'       \item{\code{n_sim}: Integer value. The number of simulations (number of future sample paths).}
#'       }
#' 
#' @export

forecast_esn <- function(object,
                         n_ahead = 12,
                         n_sim = 100,
                         n_seed = 42) {
  
  # Pre-processing ============================================================
  
  # Original time series data (wide format tibble)
  data <- object$data
  
  # Extract actual and fitted values and internal states
  actual <- object$actual
  fitted <- object$fitted
  states_train <- object$states_train
  
  # Method (hyperparameters, etc.)
  method <- object$method
  
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
  n_sdiff <- method$diff_inputs$n_sdiff
  n_diff <- method$diff_inputs$n_diff
  
  # Extract residuals
  res <- method$res
  
  # Leakage rate alpha
  alpha <- method$model_pars$alpha
  
  # Model inputs
  const <- method$model_inputs$const
  lags <- method$model_inputs$lags
  n_fourier <- method$model_inputs$n_fourier
  period <- method$model_inputs$period
  
  # Extract (time) index variable
  dttm_index <- data %>% select(index_var(data))
  
  # Create future date or date-time object
  dttm_fcst <- dttm_index %>% 
    append_row(n = n_ahead) %>%
    slice((n() - n_ahead + 1):n())
  
  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(data)[measured_vars(data)])
  # Create copy of numeric matrix y for later usage
  actual <- y
  
  # Names of response variables
  names_outputs <- colnames(y)
  
  # Get reservoir (convert states_train from tsibble to numeric matrix)
  states_train <- states_train %>%
    spread(
      key = ".state",
      value = ".value")
  
  states_train <- invoke(cbind, unclass(states_train)[measured_vars(states_train)])
  
  # Calculate seasonal and non-seasonal differences
  y <- diff_data(
    data = y,
    period = period,
    n_sdiff = n_sdiff,
    n_diff = n_diff)
  
  # Scale data to the specified interval
  scaled <- scale_data(
    data = y,
    new_range = scale_inputs)
  
  y <- scaled$data
  old_range <- scaled$old_range
  
  # Create input layer ========================================================
  
  # Create lagged variables as matrix ("revolved style")
  if (is.null(lags)) {
    y_lag <- NULL
  } else {
    y_lag <- create_revolved(
      data = y,
      lags = lags,
      n_ahead = n_ahead)
  }
  
  # Create season (fourier terms) as matrix
  if (all(n_fourier == 0)) {
    y_seas <- NULL
  } else {
    y_seas <- create_fourier(
      times = (nrow(y) + 1):(nrow(y) + n_ahead),
      n_fourier = n_fourier,
      period = period)
    
    fill_NA <- matrix(
      data = NA_real_,
      nrow = max(unlist(lags)),
      ncol = ncol(y_seas))
    
    y_seas <- rbind(0, y_seas, fill_NA)
  }
  
  # Create constant (intercept or bias term) as matrix
  if (const == FALSE | is.null(const)) {
    y_const <- NULL
  } else {
    y_const <- create_const(
      n_obs = n_ahead)
    
    fill_NA <- matrix(
      data = NA_real_,
      nrow = max(unlist(lags)),
      ncol = 1)
    
    y_const <- rbind(0, y_const, fill_NA)
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_const,
    y_lag,
    y_seas)
  
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
    innov = NULL)
  
  # Extract point forecasts
  fcst <- model_pred$fcst
  # Extract internal states
  # (first row is last row of training an can be dropped)
  states_fcst <- model_pred$states_fcst[-c(1), ]
  
  # Rescaling of point forecasts
  fcst <- rescale_data(
    data = fcst,
    old_range = old_range,
    new_range = scale_inputs)
  
  # Integrate seasonal and non-seasonal differences
  fcst <- inv_diff_data(
    data = actual,
    data_diff = fcst,
    period = period,
    n_sdiff = n_sdiff,
    n_diff = n_diff)
  
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
      error = res,
      n_sim = n_sim)
    
    # Rescaling of simulated sample path
    sim <- lapply(model_sim, function(model_sim) {
      rescale_data(
        data = model_sim,
        old_range = old_range,
        new_range = scale_inputs)
    })
    
    # Integrate seasonal and non-seasonal differences
    sim <- lapply(sim, function(sim) {
      inv_diff_data(
        data = actual,
        data_diff = sim,
        period = period,
        n_sdiff = n_sdiff,
        n_diff = n_diff)
    })
    
    # Names of simulations
    names_sim <- paste0(
      "sim","(", formatC(1:n_sim, width = nchar(max(n_sim)), flag = "0"), ")")
    
    simulation <- lapply(seq_len(n_sim), function(n) {
      bind_cols(
        dttm_fcst,
        as_tibble(sim[[n]])) %>%
        gather(
          key = ".response",
          value = ".mean") %>%
        mutate(.path = names_sim[n]) %>%
        update_tsibble(
          key = c(
            ".response",
            ".path"))
    })
    simulation <- bind_rows(simulation)
  }
  
  # Post-processing ===========================================================
  
  # Convert data from numeric matrix to tsibble
  forecast <- bind_cols(
    dttm_fcst,
    as_tibble(fcst)) %>%
    gather(
      key = ".response",
      value = ".mean") %>%
    update_tsibble(
      key = ".response")
  
  states_fcst <- bind_cols(
    dttm_fcst,
    as_tibble(states_fcst)) %>%
    gather(
      key = ".state",
      value = ".value")
  
  # Output model
  structure(
    list(
      forecast = forecast,
      simulation = simulation,
      actual = actual,
      fitted = fitted,
      states_fcst = states_fcst,
      method = method,
      n_ahead = n_ahead,
      n_sim = n_sim),
    class = "forecast")
}
