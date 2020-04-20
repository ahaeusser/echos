
#' @title Train an Echo State Network (ESN).
#' 
#' @description Train an Echo State Network (ESN).
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_esn}.
#' @param ... Further arguments passed to \code{estimate_esn(...)}.
#'
#' @return An object of class \code{ESN}.

train_esn <- function(.data,
                      specials,
                      n_diff = NULL,
                      n_initial = 10,
                      n_seed = 42,
                      density = 0.1,
                      scale_inputs = c(-1, 1),
                      inf_crit = "HQ",
                      ...) {
  
  # Number of response variables
  n_outputs <- length(tsibble::measured_vars(.data))
  
  # Number of observations
  n_obs <- nrow(.data)
  
  if (n_outputs > 1) {
    abort("Only univariate responses are supported by ESN.")
  }
  
  if(any(is.na(.data))){
    abort("ESN does not support missing values.")
  }
  
  
  # Extract specials ==========================================================
  
  # Intercept term
  if ("const" %in% names(specials)) {
    const <- TRUE
  } else {
    const <- FALSE
  }
  
  # Autoregressive terms
  if ("ar" %in% names(specials)) {
    lags <- specials$ar[[1]]
  } else {
    period <- common_periods(.data)
    period <- sort(as.numeric(period[period < n_obs]))
    
    lags <- seq(1, min(period), 1)
    lags <- rep(list(lags), n_outputs)
  }
  
  # Fourier terms
  if ("fourier" %in% names(specials)) {
    n_terms <- specials$fourier[[1]]$n_terms
    period <- specials$fourier[[1]]$period
  } else {
    n_terms <- period <- NULL
  }
  
  # Reservoir size (number of internal states)
  n_res <- specials$states[[1]]

  # Check stationarity
  if (is.null(n_diff)) {
    
    value <- .data %>% 
      pull(!!measured_vars(.data))
    
    n_diff <- unitroot_ndiffs(
      x = value,
      alpha = 0.05,
      unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],
      differences = 0:1)
  }
  
  # Hyperparameter optimization ===============================================
  
  # Starting values and lower and upper bounds (box constraints)
  # (alpha, rho, lambda, scale_runif)
  par <- c(0.8, 1, 0.1, 0.5)
  lower <- c(0, 0.5, 0.001, 1e-8)
  upper <- c(1, 1.5, 100, 1)
  
  # Find optimal hyperparameters
  opt <- optim(
    par = par,
    fn = min_loss,
    lower = lower,
    upper = upper,
    method = "L-BFGS-B",
    data = .data,
    lags = lags,
    const = const,
    n_terms = n_terms,
    period = period,
    n_diff = n_diff,
    density = density,
    n_res = n_res,
    inf_crit = inf_crit,
    scale_inputs = scale_inputs
    )
  
  # Estimate model ============================================================
  
  model_fit <- estimate_esn(
    data = .data,
    lags = lags,
    n_terms = n_terms,
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
  
  # Extract actual values and fitted values
  fitted <- model_fit$fitted[["fitted"]]
  resid <- model_fit$resid[["resid"]]
  # Get length of time series and fitted values
  n_total <- nrow(.data)
  n_fitted <- length(fitted)
  
  # Fill NAs in front of fitted values (adjust to equal length of actual values) 
  fitted <- c(rep(NA_real_, n_total - n_fitted), fitted)
  resid <- c(rep(NA_real_, n_total - n_fitted), resid)
  
  # Model specification
  model_spec <- model_fit$method$model_spec
  
  # Return model
  structure(
    list(
      model = model_fit,
      est = list(
        .fitted = fitted,
        .resid = resid),
      spec = model_spec),
    class = "ESN")
}


specials_esn <- new_specials(
  
  const = function() {},
  ar = function(p = list(c(1, 2))) {p},
  states = function(n = 150) {n},
  fourier = function(period, n_terms) {
    list(
      period = period,
      n_terms = n_terms)
  },
  .required_specials = c("states")
)


#' @title Automatic training of ESNs.
#' 
#' @description Automatic training of ESNs.
#'
#' @param formula Model specification (see "Specials" section, currently not in use...)
#' @param ... Further arguments passed to \code{echos::auto_esn(...)}.
#'
#' @return esn_model An object of class \code{ESN}.
#' @export

ESN <- function(formula, ...){
  esn_model <- new_model_class(
    model = "ESN",
    train = train_esn,
    specials = specials_esn)
  
  new_model_definition(
    esn_model,
    !!enquo(formula),
    ...)
}





#' @title Extract fitted values from a trained ESN.
#' 
#' @description Extract fitted values from a trained ESN.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Additional arguments passed to ...
#'
#' @return
#' @export

fitted.ESN <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a trained ESN.
#' 
#' @description Extract residuals from a trained ESN.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Additional arguments passed to ...
#'
#' @return
#' @export

residuals.ESN <- function(object, ...){
  object$est[[".resid"]]
}


#' @title Provide a succinct summary of a trained ESN.
#' 
#' @description Provide a succinct summary of a trained ESN.
#'
#' @param object The ESN to summarize.
#'
#' @return
#' @export

model_sum.ESN <- function(x){
  x$spec
}





#' @title Forecast a trained ESN
#' 
#' @description Forecast a trained ESN
#' 
#' @param object Trained model
#' @param new_data Forecast horizon
#' @param specials NULL
#' @param ... Further arguments passed toi bla
#' 
#' @return A fable
#' @export

forecast.ESN <- function(object,
                         new_data,
                         specials = NULL,
                         n_sim = 100,
                         ...) {
  # Extract model
  model_fit <- object$model
  
  # Forecast model
  fcst <- forecast_esn(
    object = model_fit,
    n_ahead = nrow(new_data))
  
  # Extract point forecasts
  fcst_mean <- fcst$forecast[["fcst"]]
  
  # Extract simulations
  sim <- fcst$simulation %>%
    select(-variable) %>%
    spread(
      key = path,
      value = sim)
  
  sim <- invoke(cbind, unclass(sim)[measured_vars(sim)])
  fcst_sd <- rowSds(sim, na.rm = TRUE)
  
  # Return forecast
  construct_fc(
    point = fcst_mean,
    sd = fcst_sd,
    dist = dist_normal(
      mean = fcst_mean,
      sd = fcst_sd))
}
