
#' @title Estimate an Echo State Network (ESN).
#' 
#' @description This function estimates an Echo State Network (ESN) to a univariate or multivariate time series.
#' 
#' @param data A tsibble containing the time series data. Must have column "time" with time index (date or date-time).
#' @param lags A list containing integer vectors with the lags associated with each output variable.
#' @param n_terms Integer vector. The number of seasonal cycles per period.
#' @param period Integer vector. The periodicity of the time series (e.g. for monthly data period = c(12), for hourly data period = c(24, 168)).
#' @param const Logical value. If TRUE, a constant term (intercept) is used.
#' @param n_sdiff Integer vector. The number of seasonal differences. 
#' @param n_diff Integer vector. The number of non-seasonal differences.
#' @param n_res Integer value. The number of internal states within the reservoir (hidden layer).
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param rho Numeric value. The spectral radius for scaling the reservoir weight matrix.
#' @param alpha Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.
#' @param lambda Numeric value. The regularization (shrinkage) parameter for ridge regression.
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' 
#' @return data The original input data as tibble.
#' @return actual A tibble containing the actual values with the columns time, id, type, variable, and value.
#' @return fitted A tibble containing the fitted values with the columns time, id, type, variable, and value.
#' @return error A tibble containing the errors (= residuals, i.e. actual - fitted) with the columns time, id, type, variable, and value.
#' @return pct_error A tibble containing the percentage errors (i.e. error / actual * 100) with the columns time, id, type, variable, and value.
#' @return states_train A tibble containing the internal states with the columns time, id, type, variable, and value.
#' @return method A list containing several objects and information of the trained ESN (weight matrices, hyperparameters, error and model metrics, etc.).
#' 
#' @export

estimate_esn <- function(data,
                         lags,
                         n_terms,
                         period,
                         const = TRUE,
                         n_sdiff = 0,
                         n_diff = 0,
                         n_res = 200,
                         n_initial = 10,
                         n_seed = 42,
                         rho = 0.7,
                         alpha = 0.85,
                         lambda = 12,
                         density = 0.1,
                         scale_runif = c(-0.5, 0.5),
                         scale_inputs = c(-1, 1)) {
  
  # Pre-processing ============================================================
  
  # Prepare contants as integers
  n_res <- as.integer(n_res)
  n_initial <- as.integer(n_initial)
  n_seed <- as.integer(n_seed)
  n_outputs <- as.integer(ncol(data))
  
  # Extract (time) index variable
  dttm_index <- data %>% select(index_var(data))
  
  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(data)[measured_vars(data)])
  
  # Nameas of output variables
  names_outputs <- colnames(y)
  # Number of output variables
  n_outputs <- ncol(y)
  # Names of internal states
  names_states <- paste0(
    "state","(", formatC(1:n_res, width = nchar(max(n_res)), flag = "0"), ")")
  
  # # Calculate first differences
  # if (n_diff == 1) {
  #   y <- diff_data(
  #     data = y,
  #     n_diff = 1,
  #     na_rm = FALSE)
  # }
  
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
  
  # Create lagged variables as matrix
  if (is.null(lags)) {
    y_lag <- NULL
  } else {
    y_lag <- create_lags(
      data = y,
      lags = lags)
  }
  
  # Create fourier terms (trigonometric terms) as matrix
  if (all(n_terms == 0)) {
    y_seas <- NULL
  } else {
    y_seas <- create_fourier(
      times = 1:nrow(y),
      n_terms = n_terms,
      period = period)
  }
  
  # Create constant term (intercept term) as matrix
  if (const == FALSE) {
    y_const <- NULL
  } else {
    y_const <- create_const(
      n_obs = nrow(y))
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_const,
    y_lag,
    y_seas)
  
  # Get maximum lag (overall) and prepare row indices for subsetting
  max_lag <- max(unlist(lags))
  # Train index adjusted for maximum lag and inital throw-off
  index_train <- c((1 + max_lag + n_initial):nrow(y))
  # State index adjusted for maximum lag without inital throw-off
  index_states <- c((1 + max_lag):nrow(y))
  
  # Subset input matrix (get rid of NAs)
  inputs <- inputs[index_states, ]
  
  # Create time index for training
  dttm_train <- dttm_index[index_train, ]
  # Create time index for internal states
  dttm_states <- dttm_index[index_states, ]
  
  
  # Create hidden layer (reservoir) ===========================================
  
  # Number of observations (accounted for lag length)
  n_train <- nrow(inputs)
  # Number of observations (accounted for lag length and initial throw-off)
  n_obs <- n_train - n_initial
  # Number of input features per output variable
  n_inputs <- ncol(inputs)
  
  # Set seed for random draws
  set.seed(n_seed)
  
  # Create random weight matrices for the input variables
  win <- create_win(
    n_inputs = n_inputs,
    n_res = n_res,
    scale_runif = scale_runif)
  
  # Create random weight matrix for the reservoir
  wres <- create_wres(
    n_res = n_res,
    rho = rho,
    density = density,
    scale_runif = scale_runif,
    symmetric = FALSE)
  
  # Run reservoir (create internal states)
  states_train <- run_reservoir(
    inputs = inputs,
    win = win,
    wres = wres,
    alpha = alpha)
  
  colnames(states_train) <- names_states
  
  
  # Create output layer (train model) =========================================
  
  # Concatenate inputs and reservoir
  X <- cbind(inputs, states_train)
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- X[((n_initial + 1):nrow(X)), , drop = FALSE]
  yt <- y[((n_initial + 1 + max_lag):nrow(y)), , drop = FALSE]
  
  # Linear observation weights within the interval [1, 2]
  # obs_weights <- (0:(nrow(Xt) - 1)) * (1 / (nrow(Xt) - 1)) + 1
  # Equal observation weights
  obs_weights <- rep(1, nrow(Xt))
  
  # Train linear model via ridge regression
  model <- train_ridge(
    X = Xt,
    y = yt,
    lambda = lambda,
    weights = obs_weights)
  
  # Extract coefficients (output weight matrix)
  wout <- model$wout
  # Extract fitted values (i.e. in-sample 1-step ahead forecasts) and residuals
  yhat <- model$yhat
  res <- model$res
  
  # Adjust column- and rownames of wout
  colnames(wout) <- names_outputs
  rownames(wout) <- colnames(Xt)
  
  # Adjust column names of actuals, fitted and residuals
  colnames(yt) <- names_outputs
  colnames(yhat) <- names_outputs
  colnames(res) <- names_outputs
  
  # Post-processing ===========================================================
  
  # Rename objects for convenience
  actual <- yt
  fitted <- yhat
  
  # Rescale actual and fitted values to original scale
  actual <- rescale_data(
    data = actual,
    old_range = old_range,
    new_range = scale_inputs)
  
  fitted <- rescale_data(
    data = fitted,
    old_range = old_range,
    new_range = scale_inputs)
  
  
  
  # # Inverse differencing
  # if (n_diff == 1) {
  #   actual_cumsum <- matrixStats::colCumsums(actual)
  #   fitted_cumsum <- matrixStats::colCumsums(fitted)
  #   
  #   last_value <- matrix(
  #     data = as.numeric(data[(index_train[1]), -c(1)]),
  #     nrow = nrow(actual),
  #     ncol = ncol(actual))
  #   
  #   actual <- last_value + actual_cumsum
  #   fitted <- actual + fitted
  #   
  #   colnames(actual) <- names_outputs
  #   colnames(fitted) <- names_outputs
  # }
  
  
  # Inverse differencing
  actual <- inv_diff_data(
    data = data,
    data_diff = actual,
    period = period,
    n_sdiff = n_sdiff,
    n_diff = n_diff,
    type = "inner")
  
  fitted <- inv_diff_data(
    data = data,
    data_diff = fitted,
    period = period,
    n_sdiff = n_sdiff,
    n_diff = n_diff,
    type = "inner")
  
  
  # Calculate residuals (errors)
  resid <- actual - fitted
  
  # Convert data from numeric matrix to tsibble
  actual <- bind_cols(
    dttm_train,
    as_tibble(actual)) %>%
    gather(
      key = "variable",
      value = "actual") %>%
    update_tsibble(
      key = "variable")

  fitted <- bind_cols(
    dttm_train,
    as_tibble(fitted)) %>%
    gather(
      key = "variable",
      value = "fitted") %>%
    update_tsibble(
      key = "variable")
  
  resid <- bind_cols(
    dttm_train,
    as_tibble(resid)) %>%
    gather(
      key = "variable",
      value = "resid") %>%
    update_tsibble(
      key = "variable")

  states_train <- bind_cols(
    dttm_states,
    as_tibble(states_train)) %>%
    gather(
      key = "state",
      value = "value")
  
  # Store model metrics
  model_metrics <- tibble(
    df = model$df,
    aic = model$aic,
    bic = model$bic,
    hq = model$hq)
  
  # List with model inputs
  model_inputs <- list(
    const = const,
    lags = lags,
    n_terms = n_terms,
    period = period)
  
  # List with hyperparameters
  pars <- list(
    alpha = alpha,
    rho = rho,
    lambda = lambda,
    density = density)
  
  # List with number of inputs, internal states and outputs
  n_layers <- list(
    n_inputs = n_inputs,
    n_res = n_res,
    n_outputs = n_outputs)
  
  # List with weight matrices for inputs, reservoir and outputs
  weights <- list(
    win = win,
    wres = wres,
    wout = wout)
  
  # Create model specification (short summary)
  model_spec <- create_spec(
    n_layers = n_layers,
    pars = pars,
    n_terms = n_terms,
    period = period)

  # Store results
  method <- list(
    model_inputs = model_inputs,
    model_metrics = model_metrics,
    model_spec = model_spec,
    pars = pars,
    n_layers = n_layers,
    weights = weights,
    n_diff = n_diff,
    scale_inputs = scale_inputs,
    scale_runif = scale_runif,
    res = res)
  
  # Output model
  structure(
    list(
      data = data,
      actual = actual,
      fitted = fitted,
      resid = resid,
      states_train = states_train,
      method = method),
    class = "ESN")
}
