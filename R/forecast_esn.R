
#' @title Forecast a trained Echo State Network (ESN).
#' 
#' @description Forecast an Echo State Network from a trained model (univariate and multivariate ESNs).
#' 
#' @param object An object of class "esn". The result of a call to train_esn(...).
#' @param n_ahead Integer value. The number of periods for forecasting (forecast horizon).
#' @param n_sim Integer value. The number of simulations (number of future sample paths).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' 
#' @return forecast A tibble containing the forecast and prediction intervals with the columns time, id, type, variable, and value.
#' @return simulation A tibble containing the simulations with the columns time, id, type (sim(1), sim(2), ...), variable, and value.
#' @return actual A tibble containing the actual values with the columns time, id, type, variable, and value.
#' @return fitted A tibble containing the fitted values with the columns time, id, type, variable, and value.
#' @return states_fcst A tibble containing the internal states with the columns time, id, type, variable, and value.
#' @return method A list containing several objects and information of the trained ESN (weight matrices, hyperparameters, etc.).
#' 
#' @export

forecast_esn <- function(object,
                         n_ahead = 12,
                         n_sim = 100,
                         n_seed = 42) {
  
  # Pre-processing ============================================================
  
  # Check arguments
  # if (class(object) != "esn") {stop("Object should be of class esn")}
  # if (n_ahead < 0) {stop("Forecast horizon n_ahead must be positive")}
  # if (interval == TRUE) {
  #   if (min(level) < 0 || max(level) > 99.99) {stop("Confidence levels out of range")}
  #   if (n_sim < 0) {stop("Number of simulations n_sim must be positive")}
  # }
  
  # n_ahead <- as.integer(n_ahead)
  # n_sim <- as.integer(n_sim)
  
  # Original time series data (wide format tibble)
  data <- object$data
  
  # Extract actual and fitted values and internal states
  actual <- object$actual
  fitted <- object$fitted
  states_train <- object$states_train
  
  # Method (hyperparameters, etc.)
  method <- object$method
  
  # Number of inputs, internal states within the reservoir and outputs
  n_inputs <- method$n_layers$n_inputs
  n_res <- method$n_layers$n_res
  n_outputs <- method$n_layers$n_outputs
  
  # Weight matrices for inputs, reservoir and outputs
  win <- method$weights$win
  wres <- method$weights$wres
  wout <- method$weights$wout
  
  # Arguments for differencing, transforming and scaling the time series data
  scale_inputs <- method$scale_inputs
  trans_inputs <- method$trans_inputs
  diff <- method$diff
  
  # Extract residuals
  res <- method$res
  
  # Leakage rate alpha
  alpha <- method$pars$alpha
  
  # Model inputs
  const <- method$model_inputs$const
  lags <- method$model_inputs$lags
  season <- method$model_inputs$season
  period <- method$model_inputs$period
  
  # Extract (time) index variable
  dttm_index <- data %>% select(index_var(data))
  
  # Create future date or date-time object
  dttm_fcst <- dttm_index %>% 
    append_row(n = n_ahead) %>%
    slice((n() - n_ahead + 1):n())
  
  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(data)[measured_vars(data)])
  names_outputs <- colnames(y)
  
  # Get reservoir (convert states_train from tsibble to numeric matrix)
  
  states_train <- states_train %>%
    spread(
      key = ".state",
      value = ".value")
  
  states_train <- invoke(cbind, unclass(states_train)[measured_vars(states_train)])
  
  
  # Calculate first differences
  if (diff == TRUE) {
    y <- diff_data(
      data = y,
      n_diff = 1,
      na_rm = FALSE)
  }
  
  # Transform data via ORQ transform
  if (trans_inputs == TRUE) {
    transformed <- trans_data(data = y)
    y <- transformed$data
    trans_obj <- transformed$trans_obj
  }
  
  # Scale data to the specified interval
  scaled <- scale_data(data = y, new_range = scale_inputs)
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
  if (is.null(season)) {
    y_seas <- NULL
  } else {
    y_seas <- create_season(
      times = (nrow(y) + 1):(nrow(y) + n_ahead),
      k = season,
      period = period)
    
    fill_NA <- matrix(
      data = NA_real_,
      nrow = max(unlist(lags)),
      ncol = ncol(y_seas))
    
    y_seas <- rbind(0, y_seas, fill_NA)
  }
  
  # Create constant (intercept or bias term) as matrix
  if (const == FALSE) {
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
  
  # Inverse transform point forecasts
  if (trans_inputs == TRUE) {
    fcst <- inv_trans_data(
      object = trans_obj,
      new_data = fcst)
  }
  
  
  
  # Inverse differencing
  if (diff == TRUE) {
    fcst_cumsum <- colCumsums(fcst)
    
    last_value <- matrix(
      data = as.numeric(data[nrow(data), -c(1)]),
      nrow = nrow(fcst),
      ncol = ncol(fcst))
    
    fcst <- last_value + fcst_cumsum
    colnames(fcst) <- names_outputs
  }
  
  
  # Simulation ================================================================
  
  if (is.null(n_sim)) {
    # Coerce matrix fcst to 3d-array (for convenience)
    # fcst <- array(fcst, c(n_ahead, n_outputs, 1))
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
    
    # Inverse transform simulated sample path
    if (trans_inputs == TRUE) {
      sim <- lapply(sim, function(sim) {
        inv_trans_data(
          object = trans_obj,
          new_data = sim)
      })
    }
    
    # Inverse differencing
    if (diff == TRUE) {
      sim_cumsum <- lapply(sim, function(sim) {
        colCumsums(sim)
      })
      
      last_value <- matrix(
        data = as.numeric(data[nrow(data), -c(1)]),
        nrow = nrow(fcst),
        ncol = ncol(fcst))
      
      sim <- lapply(sim_cumsum, function(sim_cumsum) {
        last_value + sim_cumsum
      })
    }
    
    names_sim <- paste("sim", "(", 1:n_sim, ")", sep = "")
    
    simulation <- lapply(seq_len(n_sim), function(n) {
      bind_cols(
        dttm_fcst,
        as_tibble(sim[[n]])) %>%
        gather(
          key = ".variable",
          value = ".value") %>%
        mutate(.sim = n) %>%
        update_tsibble(
          index = "date_time",
          key = c(
            ".variable",
            ".sim"))
    })
    
    simulation <- do.call(rbind, simulation)
  }
  
  
  # Post-processing ===========================================================
  
  # Convert data from numeric matrix to tsibble
  forecast <- bind_cols(
    dttm_fcst,
    as_tibble(fcst)) %>%
    gather(
      key = ".variable",
      value = ".value") %>%
    mutate(.type = "mean") %>%
    update_tsibble(
      key = c(".variable", ".type"))
  
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
