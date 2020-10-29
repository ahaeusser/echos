
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
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("AIC", "BIC", "HQ")}.
#'
#' @return An object of class \code{ESN}.
#' 
#' @export

auto_esn <- function(data,
                     period,
                     max_lag = 6,
                     n_fourier = c(3, 3),
                     n_initial = 10,
                     n_res = 200,
                     density = 0.1,
                     scale_inputs = c(-1, 1),
                     inf_crit = "BIC") {
  
  # Set seed for reproducibility
  n_seed <- 42
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
  
  # lags <- list(c(seq(1:max_lag), period))
  lags <- list(c(seq(1:max_lag), period, 48, 72))
  
  model_inputs <- select_inputs(
    data = data,
    lags = lags,
    n_fourier = n_fourier,
    period = period,
    const = const,
    n_diff = n_diff,
    n_initial = n_initial,
    scale_inputs = scale_inputs,
    inf_crit = inf_crit)
  
  const <- model_inputs$const
  lags <- model_inputs$lags
  n_fourier <- model_inputs$n_fourier
  
  # Hyperparameter optimization ===============================================
  
  # Starting values and lower and upper bounds (box constraints)
  par <- c(
    alpha = 0.8,
    rho = 1,
    lambda = 1,
    scale_runif = 0.5)
  
  lower <- c(
    alpha = 0,
    rho = 0.1, # 0.25, 0.5
    lambda = 0.1,
    scale_runif = 1e-8)
  
  upper <- c(
    alpha = 1,
    rho = 2, # 1.5
    lambda = 100,
    scale_runif = 5)
  
  # Find optimal hyperparameters
  opt <- optim(
    par = par,
    fn = min_loss,
    lower = lower,
    upper = upper,
    method = "L-BFGS-B",
    data = data,
    lags = lags,
    const = const,
    n_fourier = n_fourier,
    period = period,
    n_diff = n_diff,
    density = density,
    n_res = n_res,
    inf_crit = inf_crit,
    scale_inputs = scale_inputs
  )
  
  # Train final model =========================================================
  
  model_fit <- train_esn(
    data = data,
    lags = lags,
    n_fourier = n_fourier,
    period = period,
    const = const,
    n_diff = n_diff,
    n_res = n_res,
    n_initial = n_initial,
    n_seed = n_seed,
    alpha = opt$par[1],
    rho = opt$par[2],
    lambda = opt$par[3],
    density = density,
    scale_inputs = scale_inputs,
    scale_runif = c(-opt$par[4], opt$par[4])
  )
  return(model_fit)
}
