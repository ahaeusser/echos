
#' @title Train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN) to a
#'   univariate time series.
#' 
#' @param data A \code{tsibble} containing the response variable.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param fourier A \code{list} containing the periods and the number of fourier terms as integer vector.
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param dy Integer vector. The nth-differences of the response variable.
#' @param dx Integer vector. The nth-differences of the exogenous variables.
#' @param inf_crit Character value. The information criterion used for variable selection \code{inf_crit = c("aic", "aicc", "bic")}.
#' @param n_models Integer value. The maximum number of (random) models to train for model selection.
#' @param n_states Integer value. The number of internal states per reservoir.
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param alpha Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.
#' @param rho Numeric value. The spectral radius for scaling the reservoir weight matrix.
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param lambda Numeric vector. Lower and upper bound of lambda sequence for ridge regression.
#' @param scale_win Numeric value. The lower and upper bound of the uniform distribution for scaling the input weight matrix.
#' @param scale_wres Numeric value. The lower and upper bound of the uniform distribution for scaling the reservoir weight matrix.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{actual}: Numeric vector containing the actual values.}
#'       \item{\code{fitted}: Numeric vector containing the fitted values.}
#'       \item{\code{resid}: Numeric vector containing the residuals.}
#'       \item{\code{states_train}: Numeric matrix containing the internal states.}
#'       \item{\code{method}: A \code{list} containing several objects and meta information of the trained ESN (weight matrices, hyperparameters, model metrics, etc.).}
#'       }
#' @export

train_esn <- function(data,
                      lags,
                      fourier = NULL,
                      xreg = NULL,
                      dy = 0,
                      dx = 0,
                      inf_crit = "aic",
                      n_states = 100,
                      n_models = 200,
                      n_initial = 10,
                      n_seed = 42,
                      alpha = 1,
                      rho = 1,
                      density = 0.1,
                      lambda = c(1e-4, 2),
                      scale_win = 0.5,
                      scale_wres = 0.5,
                      scale_inputs = c(-1, 1)) {
  
  # Pre-processing ============================================================
  
  # Prepare constants as integers
  n_models <- as.integer(n_models)
  n_states <- as.integer(n_states)
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
  # Train index (without initial throw-off)
  index_states <- c((1 + (n_total - n_train)):n_total)
  
  names_states <- paste_names(
    x = "state",
    n = n_states
  )
  
  # Create hidden layer (reservoir) ===========================================
  
  # Set seed for random draws
  set.seed(n_seed)
  
  # Create random weight matrices for the input variables
  win <- create_win(
    n_inputs = n_inputs,
    n_states = n_states,
    scale_runif = c(-scale_win, scale_win)
  )
  
  # Create random weight matrix for each reservoir
  wres <- create_wres(
    n_states = n_states,
    rho = rho,
    density = density,
    scale_runif = c(-scale_wres, scale_wres),
    symmetric = FALSE
  )
  
  # Run reservoirs (create internal states)
  states_train <- run_reservoir(
    inputs = inputs,
    win = win,
    wres = wres,
    alpha = alpha
  )
  
  colnames(states_train) <- names_states
  
  
  # Create output layer (train model) =========================================
  
  # Intercept term as matrix
  const <- matrix(
    data = 1,
    nrow = nrow(states_train),
    ncol = 1,
    dimnames = list(c(), "(Intercept)")
  )
  
  # Bind intercept and predictor variables
  Xt <- cbind(const, states_train)
  
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- Xt[((n_initial + 1):nrow(Xt)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):n_total), , drop = FALSE]
  
  set.seed(n_seed)
  
  lambdas <- runif(
    n = n_models,
    min = lambda[1],
    max = lambda[2]
  )
  
  # Estimate models
  model_object <- map(
    .x = seq_len(n_models),
    .f = ~{
      fit_ridge(
        x = Xt,
        y = yt,
        lambda = lambdas[.x]
      )
    }
  )
  
  model_names <- paste_names(
    x = "model",
    n = n_models
  )
  
  names(model_object) <- model_names
  
  # Extract model metrics
  model_metrics <- map_dfr(
    .x = seq_len(n_models),
    .f = ~{model_object[[.x]][["metrics"]]}
  )
  
  # Order model metrics by information criterion
  model_metrics <- model_metrics %>%
    mutate(
      model = model_names,
      .before = .data$loglik) %>%
    arrange(!!sym(inf_crit))
  
  # Identify best model
  model_best <- model_metrics %>%
    slice_head(n = 1) %>%
    pull(model)
  
  # Reduce to best model
  model_object <- model_object[[model_best]]
  
  # Extract estimated coefficients (output weights)
  wout <- model_object[["wout"]]
  
  # Extract fitted values
  fitted <- model_object[["fitted"]]
  
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
    n_states = n_states,
    n_outputs = n_outputs
  )
  
  # List with weight matrices for inputs, reservoir and outputs
  model_weights <- list(
    win = win,
    wres = wres,
    wout = wout
  )
  
  # Create model specification (short summary)
  model_spec <- create_spec(model_layers = model_layers)
  
  # Store results
  method <- list(
    model_data = model_data,
    model_inputs = model_inputs,
    model_metrics = model_metrics,
    model_spec = model_spec,
    model_layers = model_layers,
    model_weights = model_weights,
    model_object = model_object,
    scale_win = scale_win,
    scale_wres = scale_wres,
    scale_inputs = scale_inputs,
    Xt = Xt,
    yt = yt
  )
  
  # Output model
  structure(
    list(
      actual = actual,
      fitted = fitted,
      resid = resid,
      states_train = states_train,
      method = method),
    class = "esn"
    )
}
