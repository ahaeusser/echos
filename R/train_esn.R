
#' @title Train an Echo State Network
#' 
#' @description Train an Echo State Network (ESN) to a univariate time series.
#'    The function automatically manages data pre-processing, reservoir
#'    generation (i.e., internal states) and model estimation and selection.
#' 
#' @param y Numeric vector containing the response variable.
#' @param lags Integer vector with the lag(s) associated with the input variable.
#' @param inf_crit Character value. The information criterion used for variable selection \code{inf_crit = c("aic", "aicc", "bic", "hqc")}.
#' @param n_diff Integer vector. The nth-differences of the response variable.
#' @param n_models Integer value. The maximum number of (random) models to train for model selection. If \code{n_models = NULL}, the number of models is defined as \code{n_states*2}.
#' @param n_states Integer value. The number of internal states of the reservoir. If \code{n_states = NULL}, the reservoir size is determined by \code{tau*n_total}, where \code{n_total} is the time series length.
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off). If \code{n_initial = NULL}, the throw-off is defined as \code{n_total*0.05}, where \code{n_total} is the time series length.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param alpha Numeric value. The leakage rate (smoothing parameter) applied to the reservoir (value greater than 0 and less than or equal to 1).
#' @param rho Numeric value. The spectral radius for scaling the reservoir weight matrix (value often between 0 and 1, but values above 1 are possible).
#' @param tau Numeric value. The reservoir scaling parameter to determine the reservoir size based on the time series length (value greater than 0 and less than or equal to 1).
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse) (value greater than 0 and less than or equal to 1).
#' @param lambda Numeric vector. Lower and upper bound of lambda sequence for ridge regression (numeric vector of length 2 with both values greater than 0 and \code{lambda[1]} < \code{lambda[2]}).
#' @param scale_win Numeric value. The lower and upper bound of the uniform distribution for scaling the input weight matrix (value greater than 0, weights are sampled from U(-\code{scale_win}, \code{scale_win})).
#' @param scale_wres Numeric value. The lower and upper bound of the uniform distribution for scaling the reservoir weight matrix (value greater than 0, weights are sampled from U(-\code{scale_res}, \code{scale_res}) before applying \code{rho} and \code{density}).
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data (numeric vector of length 2 with \code{scale_inputs[1]} < \code{scale_inputs[2]} (often symmetric, e.g., \code{c(-0.5, 0.5)} or \code{c(-1, 1)}).
#' 
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{actual}: Numeric vector containing the actual values.}
#'       \item{\code{fitted}: Numeric vector containing the fitted values.}
#'       \item{\code{resid}: Numeric vector containing the residuals.}
#'       \item{\code{states_train}: Numeric matrix containing the internal states.}
#'       \item{\code{method}: A \code{list} containing several objects and meta information of the trained ESN (weight matrices, hyperparameters, model metrics, etc.).}
#'       }
#'  
#' @family base functions
#' 
#' @references 
#'    \itemize{
#'       \item{Häußer, A. (2026). Echo State Networks for Time Series Forecasting: Hyperparameter Sweep and Benchmarking. arXiv preprint arXiv:2602.03912, 2026. \url{https://arxiv.org/abs/2602.03912}}
#'       \item{Jaeger, H. (2001). The “echo state” approach to analysing and training recurrent neural networks with an erratum note. Bonn, Germany: German National Research Center for Information Technology GMD Technical Report, 148(34):13.}
#'       \item{Jaeger, H. (2002). Tutorial on training recurrent neural networks, covering BPPT, RTRL, EKF and the "echo state network" approach.}
#'       \item{Lukosevicius, M. (2012). A practical guide to applying echo state networks. In Neural Networks: Tricks of the Trade: Second Edition, pages 659–686. Springer.}
#'       \item{Lukosevicius, M. and Jaeger, H. (2009). Reservoir computing approaches to recurrent neural network training. Computer Science Review, 3(3):127–149.}
#'    }
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' summary(xmodel)
#' 
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
                      tau = 0.4,
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
    stop("train_esn() requires a numeric vector as input.")
  }
  
  if(any(is.na(y))){
    stop("train_esn() does not support missing values.")
  }
  
  # Number of observations
  n_total <- length(y)
  
  if (is.null(n_states)) {
    n_states <- min(floor(n_total * tau), 200)
  }
  
  if (is.null(n_models)) {
    n_models <- n_states * 2
  }
  
  # Number of initial observations to drop
  n_initial <- floor(n_total * 0.05)
  
  # Number of differences required to achieve stationarity
  if (is.null(n_diff)) {
    n_diff <- estimate_ndiff(y)
  }
  
  # Set seed for reproducibility
  set.seed(n_seed)
  
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
  
  # Create random weight matrices for the input variables
  win <- create_win(
    n_inputs = n_inputs,
    n_states = n_states,
    scale_runif = c(-scale_win, scale_win)
  )
  
  # Create random weight matrix
  wres <- create_wres(
    n_states = n_states,
    rho = rho,
    density = density,
    scale_runif = c(-scale_wres, scale_wres)
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
  
  lambdas <- runif(
    n = n_models,
    min = lambda[1],
    max = lambda[2]
  )
  
  # Pre-allocate an empty list to store fitted models and model metrics
  model_object <- vector("list", n_models)
  model_metrics <- vector("list", n_models)
  
  for (i in seq_len(n_models)) {
    # Estimate models
    model_fit <- fit_ridge(
      x = Xt,
      y = yt,
      lambda = lambdas[i]
    )
    # Store model object
    model_object[[i]] <- model_fit
    # Store model metrics
    model_metrics[[i]] <- model_fit[["metrics"]]
  }
  
  model_names <- paste_names(
    x = "model",
    n = n_models
  )
  
  names(model_object) <- model_names
  
  # Row-bind into a single data frame
  model_metrics <- do.call(rbind, model_metrics)
  
  # Order model metrics by information criterion
  model_metrics <- model_metrics %>%
    mutate(
      model = model_names,
      .before = "loglik") %>%
    arrange(!!sym(inf_crit))
  
  # Alternative base R code
  # model_metrics <- cbind(model = model_names, model_metrics)
  # model_metrics <- model_metrics[ order(model_metrics[[inf_crit]]) , ]
  
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
  
  # Calculate differenced and scaled residuals
  yr <- yt - fitted
  
  # Rescale fitted values
  fitted <- rescale_vec(
    ys = fitted,
    old_range = old_range,
    new_range = scale_inputs
  )
  
  # Inverse difference fitted values
  if (n_diff > 0) {fitted <- yy[(index_train-1)] + fitted}
  
  # Calculate final residuals (rescaled and inverse differenced)
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
    yr = yr
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
    n_states, ", ",
    round(alpha, 2), ", ", 
    round(rho, 2), 
    "}, ",
    "{",
    n_models, ", ",
    round(df, 2),
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
