
#' @title Train an Autoregressive model
#' 
#' @description This function trains an Autoregressive model (ARX) to a
#'   univariate time series.
#' 
#' @param data A \code{tsibble} containing the response variable.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param fourier A \code{list} containing the periods and the number of fourier terms as integer vector.
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param dy Integer vector. The nth-differences of the response variable.
#' @param dx Integer vector. The nth-differences of the exogenous variables.
#' @param inf_crit Character value. The information criterion used for variable selection \code{inf_crit = c("aic", "aicc", "bic")}.
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{actual}: Numeric vector containing the actual values.}
#'       \item{\code{fitted}: Numeric vector containing the fitted values.}
#'       \item{\code{resid}: Numeric vector containing the residuals.}
#'       \item{\code{method}: A \code{list} containing several objects and meta information of the trained model (coefficients, model metrics, etc.).}
#'       }
#' @export

train_arx <- function(data,
                      lags,
                      fourier = NULL,
                      xreg = NULL,
                      dy = 0,
                      dx = 0,
                      inf_crit = "aic",
                      n_initial = 10,
                      scale_inputs = c(-1, 1)) {
  
  # Pre-processing ============================================================
  
  # Prepare constants as integers
  n_initial <- as.integer(n_initial)
  
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
      n_diff = dx
    )
    
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
    n_diff = dy
  )
  
  # Scale data to specified interval
  scaled <- scale_data(
    data = y,
    new_range = scale_inputs
  )
  
  y <- scaled$data
  old_range <- scaled$old_range
  
  # Create input layer ========================================================
  
  # Create lagged variables as matrix
  if (is.null(lags)) {
    y_lag <- NULL
  } else {
    y_lag <- create_lags(
      data = y,
      lags = lags
    )
  }
  
  # Create fourier terms as matrix
  if (is.null(fourier)) {
    y_fourier <- NULL
  } else {
    # Create numeric matrix of fourier terms
    y_fourier <- create_fourier(
      x = 1:n_total,
      period = fourier[[1]],
      k = fourier[[2]]
    )
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_lag,
    y_fourier,
    xreg
  )
  
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

  
  # Create output layer (train model) =========================================
  
  # Concatenate inputs and reservoir
  Xt <- inputs
  
  # Intercept term as matrix
  const <- matrix(
    data = 1,
    nrow = nrow(Xt),
    ncol = 1,
    dimnames = list(c(), "(Intercept)")
  )
  
  # Bind intercept and predictor variables
  Xt <- cbind(const, Xt)
  
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- Xt[((n_initial + 1):nrow(Xt)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):n_total), , drop = FALSE]
  
  # Estimate models
  model_object <- fit_lm(x = Xt, y = yt)
  
  # Extract model metrics
  model_metrics <- model_object[["metrics"]]
  
  # Extract and prepare coefficients
  wout <- model_object[["wout"]]
  
  # Extract and prepare fitted values
  fitted <- model_object[["fitted"]]
  fitted <- as.matrix(fitted)
  
  # Adjust actual values for correct dimension
  actual <- yy[index_train, , drop = FALSE]
  
  # Rescale fitted values
  fitted <- rescale_data(
    data = fitted,
    old_range = old_range,
    new_range = scale_inputs
  )
  
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
    yy = yy,
    xx = xx
  )
  
  # List with model inputs and settings
  model_inputs <- list(
    lags = lags,
    fourier = fourier,
    dy = dy,
    dx = dx,
    old_range = old_range
  )
  
  # List with number of inputs, internal states and outputs
  model_layers <- list(
    n_inputs = n_inputs,
    n_outputs = n_outputs
  )
  
  # List with weight matrices for inputs, reservoir and outputs
  model_weights <- list(wout = wout)
  
  # Create model specification (short summary)
  # model_spec <- create_spec(model_layers = model_layers)
  model_spec <- "ARX"
  
  # Store results
  method <- list(
    model_data = model_data,
    model_inputs = model_inputs,
    model_metrics = model_metrics,
    model_spec = model_spec,
    model_layers = model_layers,
    model_weights = model_weights,
    model_object = model_object,
    scale_inputs = scale_inputs
  )
  
  # Output model
  structure(
    list(
      actual = actual,
      fitted = fitted,
      resid = resid,
      method = method),
    class = "arx"
    )
}
