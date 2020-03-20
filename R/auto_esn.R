
#' auto_esn
#' 
#' auto_esn ...
#'
#' @param data A tsibble containing the time series data. Must have column "time" with time index (date or date-time).
#' @param n_samples Integer value. The number of samples for the random grid.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param max_lag Integer value. The maximum number of non-seasonal lags.
#' @param max_cycle Integer vector. The maximum number of trigonometric terms per season.
#' @param diff Logical value. If \code{TRUE}, the time series is modeled in first difference.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param trans_inputs Logical value. If TRUE, the data are transformed using ordered quantile normalizing transformation (OQR).
#'
#' @return An object of class \code{esn}.
#' 
#' @export

auto_esn <- function(data,
                     n_res = 200,
                     n_initial = 10,
                     n_samples = 200,
                     n_seed = 42,
                     max_lag = 18,
                     max_cycles = c(12, 12),
                     diff = FALSE,
                     scale_inputs = c(-2, 2),
                     scale_runif = c(-2, 2),
                     trans_inputs = FALSE) {
  
  # Span grid with random parameters ............................................
  
  # For reproducibility
  set.seed(n_seed)
  # Number of response variables (outputs)
  n_outputs <- length(measured_vars(data))
  
  # Hyperparameters
  lambda <- runif(n = n_samples, min = 0, max = 2)
  alpha <- runif(n = n_samples, min = 0.1, max = 1)
  rho <- runif(n = n_samples, min = 0.1, max = 1.5)
  density <- sample(c(0.1, 0.2), n_samples, replace = TRUE)
  
  # Create lags
  # Prepare seasonal lags
  seas_lags <- common_periods(data)
  n_obs <- nrow(data)
  seas_lags <- sort(as.numeric(seas_lags[seas_lags < n_obs]))
  n_periods <- length(seas_lags)
  rnd_seas_lags <- replicate(
    (n_periods + 1),
    sample(c(seas_lags, NA_integer_),
           n_samples,
           replace = TRUE))
  
  # Prepare non-seasonal lags
  rnd_lags <- sample(
    max_lag,
    size = n_samples,
    replace = TRUE)
  
  # Combine random lags as list
  lags <- lapply(seq_len(n_samples), function(n) {
    non_seas_lags <- c(seq(from = 1, to = rnd_lags[n], by = 1))
    seas_lags <- na.omit(rnd_seas_lags[n, ])
    seas_lags <- seas_lags[!duplicated(seas_lags)]
    seas_lags <- sort(seas_lags)
    rep(list(c(non_seas_lags, seas_lags)), n_outputs)
  })
  
  
  # Create seasonal terms
  rnd_seas_terms <- matrix(
    data = NA_integer_,
    nrow = n_samples,
    ncol = n_periods)
  
  for (n in seq_len(n_periods)) {
    rnd_seas_terms[, n] <- sample(max_cycles[n], n_samples, replace = TRUE)
  }
  
  seas_terms <- lapply(seq_len(n_samples), function(n) {
    rnd_seas_terms[n, ]
  })
  
  
  # Intercept term (constant)
  const <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  
  
  # Scale inputs
  scale_inputs_min = runif(n = n_samples, min = scale_inputs[1], max = 0)
  scale_inputs_max = runif(n = n_samples, min = 0, max = scale_inputs[2])
  
  scale_inputs <- lapply(seq_len(n_samples), function(n) {
    c(scale_inputs_min[n], scale_inputs_max[n])
  })
  
  
  # Scale uniform distribution
  scale_runif_min = runif(n = n_samples, min = scale_runif[1], max = 0)
  scale_runif_max = runif(n = n_samples, min = 0, max = scale_runif[2])
  
  scale_runif <- lapply(seq_len(n_samples), function(n) {
    c(scale_runif_min[n], scale_runif_max[n])
  })
  
  
  # Prepare random grid for training
  train_grid <- tibble(
    n_samples = seq(1, n_samples, 1),
    lambda = lambda,
    alpha = alpha,
    rho = rho,
    density = density,
    lags = lags,
    seas_terms = seas_terms,
    const = const,
    scale_inputs = scale_inputs,
    scale_runif = scale_runif)
  
  # Train ESNs on random grid .................................................
  
  models <- map_dfr(seq_len(n_samples), function(n) {
    # Train ESN on random grid parameters
    train_esn(
      data = data,
      lags = train_grid$lags[[n]],
      season = train_grid$seas_terms[[n]],
      period = seas_lags,
      const = train_grid$const[n],
      diff = diff,
      n_res = n_res,
      n_initial = n_initial,
      n_seed = n_seed,
      rho = train_grid$rho[n],
      alpha = train_grid$alpha[n],
      lambda = train_grid$lambda[n],
      density = train_grid$density[n],
      scale_inputs = train_grid$scale_inputs[[n]],
      scale_runif = train_grid$scale_runif[[n]],
      trans_inputs = trans_inputs)$method$model_metrics
  })
  
  train_grid <- bind_cols(
    train_grid,
    models)
  
  # Train ESN on optimal parameters ...........................................
  
  n_opt <- train_grid %>%
    filter(rss == min(rss)) %>%
    pull(n_samples)
  
  fit_esn <- train_esn(
    data = data,
    lags = train_grid$lags[[n_opt]],
    season = train_grid$seas_terms[[n_opt]],
    period = seas_lags,
    const = train_grid$const[n_opt],
    diff = diff,
    n_res = n_res,
    n_initial = n_initial,
    n_seed = n_seed,
    rho = train_grid$rho[n_opt],
    alpha = train_grid$alpha[n_opt],
    lambda = train_grid$lambda[n_opt],
    density = train_grid$density[n_opt],
    scale_inputs = train_grid$scale_inputs[[n_opt]],
    scale_runif = train_grid$scale_runif[[n_opt]],
    trans_inputs = trans_inputs)
  
  return(fit_esn)
}