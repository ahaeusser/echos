
#' @title Train an Echo State Network (ESN).
#' 
#' @description Train an Echo State Network (ESN).
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_esn}.
#' @param ... Further arguments passed to \code{train_esn()}.
#'
#' @return An object of class \code{ESN}.

fbl_train_esn <- function(.data,
                          specials,
                          n_initial = 10,
                          n_res = 200,
                          n_fourier = NULL,
                          n_seed = 42,
                          density = 0.1,
                          scale_inputs = c(-1, 1),
                          inf_crit = "BIC",
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
  
  # Maximum seasonal period which is feasable
  period <- common_periods(.data)
  period <- sort(as.numeric(period[period < n_obs]))
  
  # # Extract specials ==========================================================
  # 
  # # Intercept term
  # if ("const" %in% names(specials)) {
  #   const <- TRUE
  # } else {
  #   const <- FALSE
  # }
  # 
  # # Autoregressive terms
  # if ("ar" %in% names(specials)) {
  #   lags <- specials$ar[[1]]
  # } else {
  #   period <- common_periods(.data)
  #   period <- sort(as.numeric(period[period < n_obs]))
  #   
  #   lags <- seq(1, min(period), 1)
  #   lags <- rep(list(lags), n_outputs)
  # }
  # 
  # # Fourier terms
  # if ("fourier" %in% names(specials)) {
  #   n_fourier <- specials$fourier[[1]]$n_fourier
  #   # period <- specials$fourier[[1]]$period
  # } else {
  #   n_fourier <- NULL
  # }
  # 
  # # Reservoir size (number of internal states)
  # n_res <- specials$states[[1]]

  
  model_fit <- auto_esn(
    data = .data,
    period = period,
    n_fourier =n_fourier,
    n_initial = n_initial,
    n_res = n_res,
    density = density,
    scale_inputs = scale_inputs,
    inf_crit = inf_crit
  )
  
  # Extract actual values and fitted values
  fitted <- model_fit$fitted[[".fitted"]]
  resid <- model_fit$resid[[".resid"]]
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


# specials_esn <- new_specials(
# 
#   const = function() {},
#   ar = function(p = list(c(1, 2))) {p},
#   states = function(n = 150) {n},
#   fourier = function(period, n_terms) {
#     list(
#       period = period,
#       n_terms = n_terms)
#   },
#   .required_specials = c("states")
# )


specials_esn <- new_specials()


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
    train = fbl_train_esn,
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
  model_fcst <- forecast_esn(
    object = model_fit,
    n_ahead = nrow(new_data))
  
  # Extract point forecasts
  fcst_mean <- model_fcst$forecast[[".mean"]]
  
  # Extract simulations
  sim <- model_fcst$simulation %>%
    select(-c(.response)) %>%
    spread(
      key = .path,
      value = .mean)
  
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




#' @export
report.ESN <- function(object, ...) {
  
  method <- object$model$method
  
  n_inputs <- method$model_layer$n_inputs
  n_res <- method$model_layer$n_res
  n_outputs <- method$model_layer$n_outputs
  
  const <- method$model_inputs$const
  lags <- unlist(method$model_inputs$lags)
  
  n_sdiff <- as.numeric(method$diff_inputs$n_sdiff)
  n_diff <- as.numeric(method$diff_inputs$n_diff)
  
  alpha <- round(method$model_pars$alpha, 2)
  rho <- round(method$model_pars$rho, 2)
  lambda <- round(method$model_pars$lambda, 2)
  density <- round(method$model_pars$density, 2)
  
  df <- round(method$model_metrics$df, 2)
  aic <- round(method$model_metrics$aic, 2)
  bic <- round(method$model_metrics$bic, 2)
  hq <- round(method$model_metrics$hq, 2)
  
  scale_inputs <- round(method$scale_inputs, 2)
  scale_runif <- round(method$scale_runif, 2)
  
  cat(
    "\nNetwork size:", "\n",
    "Inputs        =", n_inputs, "\n",
    "Reservoir     =", n_res, "\n",
    "Outputs       =", n_outputs, "\n"
  )
  
  cat(
    "\nModel inputs:", "\n",
    "Constant =", const, "\n",
    "Lags     =", lags, "\n"
  )
  
  cat(
    "\nDifferences: \n",
    "Seasonal     = ", n_sdiff, "\n",
    "Non-seasonal = ", n_diff, "\n"
  )
  
  cat(
    "\nScaling: \n",
    " Inputs         = ", "(", scale_inputs[1], ", ", scale_inputs[2], ")", "\n",
    " Random uniform = ", "(", scale_runif[1], ", ", scale_runif[2], ")", "\n",
    sep = ""
  )
  
  cat(
    "\nHyperparameters:", "\n",
    "alpha   =", alpha, "\n",
    "rho     =", rho, "\n",
    "lambda  =", lambda, "\n",
    "density =", density, "\n"
  )
  
  cat(
    "\nMetrics:", "\n",
    "df  =", df, "\n",
    "AIC =", aic, "\n",
    "BIC =", bic, "\n",
    "HQ  =", hq, "\n"
  )
}

