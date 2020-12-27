
#' @title Automatic train an Echo State Network (ESN)
#' 
#' @description This function trains automatically an Echo State Network (ESN)
#'    to a univariate or multivariate time series.
#'
#' @param .data A \code{tsibble} containing the time series data.
#' @param period Integer vector. The periodicity of the time series (e.g. for monthly data \code{period = c(12)}, for hourly data \code{period = c(24, 168)}).
#' @param max_lag Integer value. Maximum number of non-seasonal lags.
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles per period).
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_res Integer value. The number of internal states within the reservoir (hidden layer).
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("aic", "bic", "hq")}.
#' @param n_sample Integer value. The number of random samples for random search.
#'
#' @return An object of class \code{ESN}.
#' 
#' @export

auto_esn <- function(data,
                     period,
                     max_lag = 6,
                     n_fourier = NULL,
                     n_initial = 10,
                     n_res = 200,
                     n_seed = 42,
                     density = 0.1,
                     scale_inputs = c(-1, 1),
                     scale_runif = c(-0.5, 0.5),
                     inf_crit = "bic",
                     n_sample = 5000) {
  
  # Set intercept term
  const <- TRUE
  
  # Maximum seasonal period which is feasible
  n_obs <- nrow(data)
  period <- common_periods(data)
  period <- sort(as.numeric(period[period < n_obs]))
  
  # Check stationarity of time series
  ur <- check_unitroots(
    .data = data,
    alpha = 0.05)
  
  n_diff <- ur$n_diff
  
  lags <- list(c(seq(1:max_lag), period))
  
  model_inputs <- select_inputs(
    data = data,
    lags = lags,
    n_fourier = n_fourier,
    period = period,
    n_diff = n_diff,
    n_initial = n_initial,
    scale_inputs = scale_inputs,
    inf_crit = inf_crit,
    n_sample = n_sample,
    n_seed = n_seed)
  
  const <- model_inputs$const
  lags <- model_inputs$lags
  n_fourier <- model_inputs$n_fourier
  
  # Ensemble modeling =========================================================
  
  # Expand a grid with varying hyperparameters
  pars_alpha <- seq(0, 1, 0.25)
  pars_rho <- seq(0.25, 2.5, 0.25)
  pars_lambda <- c(1, 5, 10)
  pars_density <- 0.1
  
  model_grid <- expand_grid(
    alpha = pars_alpha,
    rho = pars_rho,
    lambda = pars_lambda,
    density = pars_density)
  
  # Train models for varying hyperparameters
  model_fit <- map(
    .x = seq_len(nrow(model_grid)),
    .f = ~train_esn(
      data = data,
      lags = lags,
      n_fourier = n_fourier,
      period = period,
      const = const,
      n_diff = n_diff,
      n_res = n_res,
      n_initial = n_initial,
      n_seed = n_seed,
      alpha = model_grid$alpha[.x],
      rho = model_grid$rho[.x],
      lambda = model_grid$lambda[.x],
      density = model_grid$density[.x],
      scale_inputs = scale_inputs,
      scale_runif = scale_runif)
  )
  
  model_names <- paste0("model","(", formatC(seq_len(nrow(model_grid)), width = nchar(max(seq_len(nrow(model_grid)))), flag = "0"), ")")
  names(model_fit) <- model_names
  
  return(model_fit)
}
