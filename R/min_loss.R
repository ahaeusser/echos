
#' @title Loss function for minimization.
#' 
#' @description This function calculates the loss (information criterion) for minimization. 
#'
#' @param data A \code{tsibble} containing the time series data.
#' @param par Numeric vector containing the hyperparameters.
#' @param lags A list containing integer vectors with the lags associated with each output variable.
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles per period).
#' @param period Integer vector. The periodicity of the time series (e.g. for monthly data \code{period = c(12)}, for hourly data \code{period = c(24, 168)}).
#' @param const Logical value. If \code{TRUE}, a constant term (intercept) is used.
#' @param n_diff Integer value. The number of differences.
#' @param n_res Integer value. The number of internal states within the reservoir (hidden layer).
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("AIC", "BIC", "HQ")}.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#'
#' @return loss Numeric value to be minimized.
#' @export

min_loss <- function(data,
                     par,
                     lags,
                     n_fourier,
                     period,
                     const = TRUE,
                     n_diff = 0,
                     n_res = 200,
                     n_initial = 10,
                     n_seed = 42,
                     density = 0.1,
                     inf_crit = "HQ",
                     scale_inputs = c(-1, 1)) {
  
  # Pre-processing ============================================================
  
  alpha <- par[1]
  rho <- par[2]
  lambda <- par[3]
  scale_runif <- c(-par[4], par[4])
  
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
  if (all(n_fourier == 0)) {
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
  inputs <- inputs[complete.cases(inputs), ]
  
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
  X <- cbind(inputs, states_train)
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- X[((n_initial + 1):nrow(X)), , drop = FALSE]
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
 
  # Information criteria
  aic <- model$aic
  bic <- model$bic
  hq <- model$hq
  
  if (inf_crit == "AIC") {
    loss <- model$aic
  } else if (inf_crit == "BIC") {
    loss <- model$bic
  } else if (inf_crit == "HQ") {
    loss <- model$hq
  }
  
  return(loss)
}