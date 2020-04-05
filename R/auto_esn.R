
#' auto_esn
#' 
#' auto_esn ...
#'
#' @param data A tsibble containing the time series data. Must have column "time" with time index (date or date-time).
#' @param n_sample Integer value. The number of samples for the random grid.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param max_lag Integer value. The maximum number of non-seasonal lags.
#' @param max_cycle Integer vector. The maximum number of trigonometric terms per season.
#' @param diff Logical value. If \code{TRUE}, the time series is modeled in first difference.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#'
#' @return An object of class \code{ESN}.
#' 
#' @export

auto_esn <- function(data,
                     n_res = 100,
                     n_sample = 200,
                     max_lag = 6,
                     max_trig = c(3, 3),
                     n_diff = 1) {
  
  # Preparation =================================================================
  
  # Fixed parameters
  n_seed <- 42
  n_initial <- 10
  const <- TRUE
  scale_inputs <- c(-1, 1)
  scale_runif <- c(-0.5, 0.5)
  
  # Number of response variables (outputs)
  n_outputs <- length(measured_vars(data))
  # Number of observations
  n_obs <- nrow(data)
  
  # Identify common periods
  period <- common_periods(data)
  # Exclude periods which are longer then sample size
  period <- period[period < n_obs]
  # Sort periods ascending
  period <- sort(as.numeric(period))
  # Number of periods
  n_period <- length(period)
  
  
  # Span grid with random parameters ============================================
  
  # Set seed for reproducibility
  set.seed(n_seed)
  
  # Hyperparameters
  lambda <- runif(n = n_sample, min = 0.00001, max = 10)
  alpha <- runif(n = n_sample, min = 0.5, max = 1)
  rho <- runif(n = n_sample, min = 0.5, max = 1.5)
  density <- sample(c(0.1, 0.2), n_sample, replace = TRUE)
  
  # Create lags
  rnd_seas_lags <- replicate(
    (n_period + 1),
    sample(
      c(period, NA_integer_),
      n_sample,
      replace = TRUE))
  
  # Prepare non-seasonal lags
  rnd_lags <- sample(
    max_lag,
    size = n_sample,
    replace = TRUE)
  
  # Combine random lags as list
  lags <- lapply(seq_len(n_sample), function(n) {
    non_seas_lags <- c(seq(from = 1, to = rnd_lags[n], by = 1))
    seas_lags <- na.omit(rnd_seas_lags[n, ])
    seas_lags <- seas_lags[!duplicated(seas_lags)]
    seas_lags <- sort(seas_lags)
    rep(list(c(non_seas_lags, seas_lags)), n_outputs)
  })
  
  # Create trigonometric terms
  rnd_n_trig <- matrix(
    data = NA_integer_,
    nrow = n_sample,
    ncol = n_period)
  
  
  for (n in seq_len(n_period)) {
    rnd_n_trig[, n] <- sample(
      x = seq(0, max_trig[n]),
      size = n_sample,
      replace = TRUE)
  }
  
  n_trig <- lapply(seq_len(n_sample), function(n) {
    rnd_n_trig[n, ]
  })
  
  # Prepare random grid for training
  train_grid <- tibble(
    n_sample = seq(1, n_sample, 1),
    lambda = lambda,
    alpha = alpha,
    rho = rho,
    density = density,
    lags = lags,
    n_trig = n_trig)
  
  # Train ESNs on random grid .................................................
  
  models <- map_dfr(seq_len(n_sample), function(n) {
    # Train ESN on random grid parameters
    train_esn(
      data = data,
      # lags = train_grid$lags[[n]],
      lags = list(seq(1, 24, 1)),
      n_trig = train_grid$n_trig[[n]],
      period = period,
      const = const,
      n_diff = n_diff,
      n_res = n_res,
      n_initial = n_initial,
      n_seed = n_seed,
      rho = train_grid$rho[n],
      alpha = train_grid$alpha[n],
      lambda = train_grid$lambda[n],
      density = train_grid$density[n],
      # density = 1,
      scale_inputs = scale_inputs,
      scale_runif = scale_runif)$method$model_metrics
  })
  
  train_grid <- bind_cols(
    train_grid,
    models)
  
  # Train ESN on optimal parameters ...........................................
  
  # n_opt <- train_grid %>%
  #   filter(rss == min(rss)) %>%
  #   pull(n_sample)
  
  n_opt <- train_grid %>%
    filter(aic == min(aic)) %>%
    pull(n_sample)
  
  fit_esn <- train_esn(
    data = data,
    # lags = train_grid$lags[[n_opt]],
    lags = list(seq(1, 24, 1)),
    n_trig = train_grid$n_trig[[n_opt]],
    period = period,
    const = const,
    n_diff = n_diff,
    n_res = n_res,
    n_initial = n_initial,
    n_seed = n_seed,
    rho = train_grid$rho[n_opt],
    alpha = train_grid$alpha[n_opt],
    lambda = train_grid$lambda[n_opt],
    density = train_grid$density[n_opt],
    # density = 1,
    scale_inputs = scale_inputs,
    scale_runif = scale_runif)
  
  list(
    fit_esn = fit_esn,
    train_grid = train_grid)
  
  # return(fit_esn)
}