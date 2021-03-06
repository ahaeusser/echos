
#' @title Train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN) to a
#'   univariate time series.
#' 
#' @param data A \code{tsibble} containing the response variable.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param fourier A \code{list} containing the periods and the number of fourier terms as integer vector.
#' @param const Logical value. If \code{TRUE}, an intercept term is used.
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param dy Integer vector. The nth-differences of the response variable.
#' @param dx Integer vector. The nth-differences of the exogenous variables.
#' @param n_res Integer value. The number of internal states within the reservoir (hidden layer).
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param alpha Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.
#' @param rho Numeric value. The spectral radius for scaling the reservoir weight matrix.
#' @param lambda Numeric value. The regularization (shrinkage) parameter for ridge regression.
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param weights Numeric vector. Observation weights for weighted least squares estimation.
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{actual}: Numeric vector containing the actual values.}
#'       \item{\code{fitted}: Numeric vector containing the fitted values.}
#'       \item{\code{error}: Numeric vector containing the residuals.}
#'       \item{\code{states_train}: Numeric matrix containing the internal states.}
#'       \item{\code{method}: A \code{list} containing several objects and meta information of the trained ESN (weight matrices, hyperparameters, model metrics, etc.).}
#'       }
#' @export

train_esn <- function(data,
                      lags,
                      fourier = NULL,
                      const = TRUE,
                      xreg = NULL,
                      dy = 0,
                      dx = 0,
                      n_res = 200,
                      n_initial = 10,
                      n_seed = 42,
                      alpha = 0.85,
                      rho = 0.7,
                      lambda = 12,
                      density = 0.1,
                      weights = NULL,
                      scale_runif = c(-0.5, 0.5),
                      scale_inputs = c(-1, 1)) {
  
  # Pre-processing ============================================================
  
  # Prepare constants as integers
  n_res <- as.integer(n_res)
  n_initial <- as.integer(n_initial)
  n_seed <- as.integer(n_seed)
  
  # Prepare exogenous variables
  if (is.null(xreg)) {
    xx <- NULL
  } else {
    # Convert tsibble to numeric matrix
    xreg <- invoke(cbind, unclass(xreg)[measured_vars(xreg)])
    # Copy of original data for later usage
    xx <- xreg
    
    # Calculate nth-differences
    if (is.null(dx)) {dx <- 0}
    xreg <- diff_data(
      data = xreg,
      n_diff = dx)
    
    # Scale data to specified interval
    xreg <- scale_data(
      data = xreg,
      new_range = scale_inputs)$data
  }
  
  # Prepare output variable
  # Name of output variable
  name_output <- measured_vars(data)
  # Convert tsibble to numeric matrix
  y <- invoke(cbind, unclass(data)[name_output])
  # Number of outputs
  n_outputs <- ncol(y)
  # Number of total observations
  n_total <- nrow(y)
  # Create copy of original data for later usage
  yy <- y
  
  # Calculate nth-difference of output variable
  y <- diff_data(
    data = y,
    n_diff = dy)
  
  # Scale data to specified interval
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
  if (is.null(fourier)) {
    y_fourier <- NULL
  } else {
    # Create numeric matrix of fourier terms
    y_fourier <- create_fourier(
      x = 1:n_total,
      period = fourier[[1]],
      k = fourier[[2]])
  }
  
  # Create constant term (intercept term) as matrix
  if (const == FALSE) {
    y_const <- NULL
  } else {
    y_const <- create_const(
      n_obs = n_total)
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_const,
    y_lag,
    y_fourier,
    xreg)
  
  # Drop NAs for training
  inputs <- inputs[complete.cases(inputs), , drop = FALSE]
  
  # Number of observations (training)
  n_train <- nrow(inputs)
  # Number of observations (accounted for initial throw-off)
  n_obs <- n_train - n_initial
  # Number of input features (constant, lagged variables, etc.)
  n_inputs <- ncol(inputs)
  # Train index (with initial throw-off)
  index_train <- c((1 + (n_total - n_train + n_initial)):n_total)
  # Train index (without initial throw-off)
  index_states <- c((1 + (n_total - n_train)):n_total)
  
  
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
  
  # Names of internal states
  colnames(states_train) <- paste0(
    "state","(",
    formatC(
      x = 1:n_res,
      width = nchar(max(n_res)),
      flag = "0"),
    ")")
  
  
  # Create output layer (train model) =========================================
  
  # Concatenate inputs and reservoir
  Xt <- cbind(inputs, states_train)
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- Xt[((n_initial + 1):nrow(Xt)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):n_total), , drop = FALSE]
  
  # Observation weights for ridge regression
  if (is.null(weights)) {
    weights <- rep(1, nrow(Xt))
  }
  
  # Train linear model via ridge regression
  model <- train_ridge(
    X = Xt,
    y = yt,
    lambda = lambda,
    weights = weights)
  
  # Extract model components
  wout <- model$wout  # coefficients (output weights)
  yf <- model$yf      # fitted values (scaled)
  yr <- model$yr      # residuals (scaled)
  
  # Adjust column- and row names of wout
  colnames(wout) <- name_output
  rownames(wout) <- colnames(Xt)
  
  # Adjust column names of actual values, fitted and residuals
  colnames(yt) <- name_output
  colnames(yf) <- name_output
  colnames(yr) <- name_output
  
  # Adjust actual values for correct dimension
  actual <- yy[index_train, , drop = FALSE]
  
  # Rescale fitted values
  fitted <- rescale_data(
    data = yf,
    old_range = old_range,
    new_range = scale_inputs)
  
  actual <- as.numeric(actual)
  fitted <- as.numeric(fitted)
  
  # Inverse difference fitted values
  if (dy > 0) {fitted <- actual + fitted}
  
  # Calculate residuals
  resid <- actual - fitted
  
  # Fill NAs in front of vectors (adjust to length of original data)
  actual <- c(rep(NA_real_, n_total - n_obs), actual)
  fitted <- c(rep(NA_real_, n_total - n_obs), fitted)
  resid <- c(rep(NA_real_, n_total - n_obs), resid)
  
  
  # Post-processing ===========================================================
  
  # Store model data for forecasting
  model_data <- list(
    yt = yt,
    yr = yr,
    yy = yy,
    xx = xx)
  
  # Store model metrics
  model_metrics <- tibble(
    dof = model$dof,
    aic = model$aic,
    bic = model$bic,
    hq = model$hq)
  
  # List with model inputs and settings
  model_inputs <- list(
    const = const,
    lags = lags,
    fourier = fourier,
    dy = dy,
    dx = dx,
    old_range = old_range)
  
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
  
  # Store results
  method <- list(
    model_inputs = model_inputs,
    model_data = model_data,
    model_metrics = model_metrics,
    model_spec = model_spec,
    model_pars = model_pars,
    model_layers = model_layers,
    model_weights = model_weights,
    scale_inputs = scale_inputs,
    scale_runif = scale_runif)
  
  # Output model
  structure(
    list(
      actual = actual,
      fitted = fitted,
      resid = resid,
      states_train = states_train,
      method = method),
    class = "esn")
}
