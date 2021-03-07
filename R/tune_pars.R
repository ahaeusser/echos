
#' @title Estimate information criterion for hyperparameter tuning
#' 
#' @description The function \code{tune_pars()} estimates the information criterion for hyperparameter
#'   tuning. The function returns a numeric value (= information criterion), which
#'   is then minimized within a call to \code{optim()} for varying hyperparameters. 
#'
#' @param data A \code{tsibble} containing the response variable.
#' @param pars Numeric vector containing the hyperparameters.
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("aic", "bic", "hq")}.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param fourier A \code{list} containing the periods and the number of fourier terms as integer vector.
#' @param const Logical value. If \code{TRUE}, an intercept term is used.
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param dy Integer vector. The nth-differences of the response variable.
#' @param dx Integer vector. The nth-differences of the exogenous variables.
#' @param n_res Integer value. The number of internal states within the reservoir (hidden layer).
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param rho Numeric value. The spectral radius for scaling the reservoir weight matrix.
#' @param alpha Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.
#' @param lambda Numeric value. The regularization (shrinkage) parameter for ridge regression.
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param weights Numeric vector. Observation weights for weighted least squares estimation.
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#'
#' @return model_value Numeric value. The information criterion to be minimized.
#' @export

tune_pars <- function(data,
                      pars,
                      inf_crit,
                      lags,
                      fourier,
                      const,
                      xreg,
                      dy,
                      dx,
                      n_res,
                      n_initial,
                      n_seed,
                      density,
                      weights,
                      scale_runif,
                      scale_inputs) {
  
  # Pre-processing ============================================================
  
  alpha <- pars[1]
  rho <- pars[2]
  lambda <- pars[3]
  
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
 
  # Extract information criterion
  model_value <- model[[inf_crit]]
  
  return(model_value)
}