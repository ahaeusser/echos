
#' @title Train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN) to a
#'   univariate time series.
#' 
#' @param y Numeric vector containing the response variable.
#' @param lags Integer vectors with the lags associated with the input variable.
#' @param inf_crit Character value. The information criterion used for variable selection \code{inf_crit = c("aic", "aicc", "bic")}.
#' @param n_diff Integer vector. The nth-differences of the response variable.
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
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' summary(xmodel)
#' @export

train_esn <- function(y,
                      lags = 1,
                      inf_crit = "bic",
                      n_diff = NULL,
                      n_states = NULL,
                      n_models = NULL,
                      n_initial = NULL,
                      n_seed = 42,
                      alpha = 1,
                      rho = 1,
                      density = 0.5,
                      lambda = c(1e-4, 2),
                      scale_win = 0.5,
                      scale_wres = 0.5,
                      scale_inputs = c(-0.5, 0.5)) {
  
  
  # Argument handling =========================================================
  
  # Check input data
  if (is.vector(y) & is.numeric(y)) {
    n_outputs <- 1
  } else {
    abort("train_esn() requires a numeric vector as input.")
  }
  
  if(any(is.na(y))){
    abort("train_esn() does not support missing values.")
  }
  
  # Number of observations
  n_total <- length(y)
  
  if (is.null(n_states)) {
    n_states <- min(floor(n_total * 0.4), 100)
  }
  
  if (is.null(n_models)) {
    n_models <- n_states * 2
  }
  
  # Number of initial observations to drop
  n_initial <- floor(n_total * 0.05)
  
  if (is.null(n_diff)) {
    n_diff <- ndiffs(y)
    if (n_diff > 1) {
      n_diff <- 1
    }
  }
  
  # Train model ===============================================================
  
  # Pre-processing ============================================================
  
  # Create copy of original data for later usage
  yy <- y
  
  # Calculate nth-difference of output variable
  y <- diff_vec(y = y, n_diff = n_diff)
  
  # Scale data to specified interval
  scaled <- scale_vec(y = y, new_range = scale_inputs)
  y <- scaled$ys
  old_range <- scaled$old_range
  
  # Create input layer ========================================================
  ylag <- create_lags(y = y, lags = lags)
  
  y <- as.matrix(y)
  inputs <- ylag
  
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
  
  # Identify best model, lambda and degrees of freedom (extract first row)
  model_best <- model_metrics[["model"]][1]
  lambda <- model_metrics[["lambda"]][1]
  df <- model_metrics[["df"]][1]
  
  # Reduce to best model
  model_object <- model_object[[model_best]]
  
  # Extract estimated coefficients (output weights)
  wout <- model_object[["wout"]]
  
  # Extract fitted values
  fitted <- as.numeric(model_object[["fitted"]])
  
  # Adjust actual values for correct dimension
  actual <- yy[index_train]
  
  # Rescale fitted values
  fitted <- rescale_vec(
    ys = fitted,
    old_range = old_range,
    new_range = scale_inputs
  )
  
  # Inverse difference fitted values
  if (n_diff > 0) {fitted <- yy[(index_train-1)] + fitted}
  
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
    yy = yy
  )
  
  # List with model inputs and settings
  model_meta <- list(
    lags = lags,
    n_diff = n_diff,
    n_models = n_models,
    old_range = old_range,
    alpha = alpha,
    rho = rho,
    density = density,
    lambda = lambda,
    df = df
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
  model_spec <- paste0(
    "ESN", "(",
      "{",
        n_total, ", ", 
        n_states, ", ", 
        n_models, 
      "}, ",
      "{",
        round(df, 2), ", ",
        round(lambda, 4),
      "}",
    ")")
  
  # Store results
  method <- list(
    model_data = model_data,
    model_meta = model_meta,
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
