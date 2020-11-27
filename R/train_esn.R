
#' @title Train an Echo State Network (ESN).
#' 
#' @description This function trains an Echo State Network (ESN) to a univariate or multivariate time series.
#' 
#' @param data A \code{tsibble} containing the time series data.
#' @param lags A list containing integer vectors with the lags associated with each output variable.
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles per period).
#' @param period Integer vector. The periodicity of the time series (e.g. for monthly data \code{period = c(12)}, for hourly data \code{period = c(24, 168)}).
#' @param const Logical value. If \code{TRUE}, a constant term (intercept) is used.
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
#' @return A list containing:
#' 
#'    \itemize{
#'       \item{\code{data}: The original input data as \code{tsibble}.}
#'       \item{\code{actual}: A \code{tsibble} containing the actual values in long format.}
#'       \item{\code{fitted}: A \code{tsibble} containing the fitted values in long format.}
#'       \item{\code{error}: A \code{tsibble} containing the errors (= residuals, i.e. actual - fitted) in long format.}
#'       \item{\code{states_train}: A \code{tsibble} containing the internal states in long format.}
#'       \item{\code{method}: A list containing several objects and information of the trained ESN (weight matrices, hyperparameters, model metrics, etc.).}
#'       }
#' 
#' @export

train_esn <- function(data,
                      lags,
                      n_fourier,
                      period,
                      const = TRUE,
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
  
  # Prepare constants as integers
  n_res <- as.integer(n_res)
  n_initial <- as.integer(n_initial)
  n_seed <- as.integer(n_seed)
  n_outputs <- as.integer(ncol(data))
  
  # Extract (time) index variable
  dttm_index <- data %>% select(index_var(data))
  
  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(data)[measured_vars(data)])
  # Create copy of numeric matrix y for later usage
  actual <- y
  
  # Names of response variables
  names_outputs <- colnames(y)
  # Number of response variables
  n_outputs <- ncol(y)
  # Names of internal states
  names_states <- paste0(
    "state","(", formatC(1:n_res, width = nchar(max(n_res)), flag = "0"), ")")
  
  # Calculate seasonal and non-seasonal differences
  y <- diff_data(
    data = y,
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
  if (is.null(n_fourier)) {
    y_seas <- NULL
  } else {
    y_seas <- create_fourier(
      times = 1:nrow(y),
      n_fourier = n_fourier,
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
  
  # Drop NAs for training
  inputs <- inputs[complete.cases(inputs), , drop = FALSE]
  
  # Number of observations (total)
  n_total <- nrow(y)
  # Number of observations (training)
  n_train <- nrow(inputs)
  # Number of observations (accounted for initial throw-off)
  n_obs <- n_train - n_initial
  # Number of input features (constant, lagged variables, etc.)
  n_inputs <- ncol(inputs)
  
  # Maximum lag (overall)
  max_lag <- max(unlist(lags))
  # Train index (with initial throw-off)
  index_train <- c((1 + (n_total - n_train + n_initial)):n_total)
  # Train index (without initial throw-off)
  index_states <- c((1 + (n_total - n_train)):n_total)
  
  # Create time index for training
  dttm_train <- dttm_index[index_train, ]
  # Create time index for internal states
  dttm_states <- dttm_index[index_states, ]
  
  
  # Create hidden layer (reservoir) ===========================================
  
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
  Xt <- cbind(inputs, states_train)
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- Xt[((n_initial + 1):nrow(Xt)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):nrow(y)), , drop = FALSE]
  
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
  
  # Adjust actual values for correct dimension
  actual <- actual[index_train, , drop = FALSE]
  
  # Rescale fitted values
  fitted <- rescale_data(
    data = yhat,
    old_range = old_range,
    new_range = scale_inputs)
  
  # Inverse difference fitted values
  if (n_diff > 0) {
    fitted <- actual + fitted
  }
  
  # Calculate residuals (1-step ahead forecast errors)
  resid <- actual - fitted
  
  # Convert data from numeric matrix to tsibble
  actual <- bind_cols(
    dttm_train,
    as_tibble(actual)) %>%
    gather(
      key = ".response",
      value = ".actual") %>%
    update_tsibble(
      key = ".response")
  
  fitted <- bind_cols(
    dttm_train,
    as_tibble(fitted)) %>%
    gather(
      key = ".response",
      value = ".fitted") %>%
    update_tsibble(
      key = ".response")
  
  resid <- bind_cols(
    dttm_train,
    as_tibble(resid)) %>%
    gather(
      key = ".response",
      value = ".resid") %>%
    update_tsibble(
      key = ".response")
  
  states_train <- bind_cols(
    dttm_states,
    as_tibble(states_train)) %>%
    gather(
      key = ".state",
      value = ".value")
  
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
    n_fourier = n_fourier,
    period = period)
  
  # List with hyperparameters
  model_pars <- list(
    alpha = alpha,
    rho = rho,
    lambda = lambda,
    density = density)
  
  # List with number of inputs, internal states and outputs
  model_layers <- list(
    n_inputs = n_inputs,
    n_res = n_res,
    n_outputs = n_outputs)
  
  # List with weight matrices for inputs, reservoir and outputs
  model_weights <- list(
    win = win,
    wres = wres,
    wout = wout)
  
  # Create model specification (short summary)
  model_spec <- create_spec(
    model_layers = model_layers,
    model_pars = model_pars,
    model_inputs = model_inputs)
  
  diff_inputs <- list(n_diff = n_diff)
  
  # Store results
  method <- list(
    model_inputs = model_inputs,
    model_metrics = model_metrics,
    model_spec = model_spec,
    model_pars = model_pars,
    model_layers = model_layers,
    model_weights = model_weights,
    diff_inputs = diff_inputs,
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
