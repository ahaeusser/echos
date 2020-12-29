
#' @title Train an Echo State Network (ESN)
#' 
#' @description Train an Echo State Network (ESN).
#'
#' @param .data Input data as \code{tsibble}.
#' @param specials Specials as list defined in \code{specials_esn}.
#' @param max_lag Integer value. Maximum number of non-seasonal lags.
#' @param ... Further arguments passed to \code{train_esn()}.
#'
#' @return An object of class \code{ESN}.

fbl_train_esn <- function(.data,
                          specials,
                          const = TRUE,
                          lags = NULL,
                          n_fourier = NULL,
                          n_initial = 10,
                          n_res = 200,
                          n_seed = 42,
                          n_sample = 5000,
                          alpha = seq(0, 1, 0.25),
                          rho = seq(0.25, 2.5, 0.25),
                          lambda = c(1, 5, 10),
                          density = 0.1,
                          scale_inputs = c(-1, 1),
                          scale_runif = c(-0.5, 0.5),
                          inf_crit = "bic",
                          type = "mode",
                          ...) {
  
  # Preparation ===============================================================
  
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
  
  # Maximum seasonal period which is feasible
  period <- common_periods(.data)
  period <- sort(as.numeric(period[period < n_obs]))
  
  # Check stationarity of time series
  n_diff <- check_unitroots(
    .data = .data,
    alpha = 0.05)$n_diff
  
  
  # Select model inputs (lags, fourier terms) =================================
  
  # # Default maximum non-seasonal lag to minimum seasonal period minus one
  # if (is.null(max_lag)) {
  #   max_lag <- min(period) - 1
  # }
  # 
  # # Lags as list
  # lags <- list(c(seq(1:max_lag), period))
  # 
  # model_inputs <- select_inputs(
  #   data = .data,
  #   lags = lags,
  #   n_fourier = n_fourier,
  #   period = period,
  #   n_diff = n_diff,
  #   n_initial = n_initial,
  #   scale_inputs = scale_inputs,
  #   inf_crit = inf_crit,
  #   n_sample = n_sample,
  #   n_seed = n_seed)
  # 
  # const <- model_inputs$const
  # lags <- model_inputs$lags
  # n_fourier <- model_inputs$n_fourier
  
  
  if (is.null(lags)) {
    lags <- list(c(1, period))
  }
  
  n_inputs <- lengths(lags)
  
  # Ensemble modeling =========================================================
  
  # Expand a grid with varying hyperparameters
  model_grid <- expand_grid(
    alpha = alpha,
    rho = rho,
    lambda = lambda,
    density = density)
  
  # Train models for varying hyperparameters
  model_fit <- map(
    .x = seq_len(nrow(model_grid)),
    .f = ~train_esn(
      data = .data,
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
  
  # Prepare fitted values (extract fitted values of all models and estimate ensemble)
  fitted <- model_fit %>%
    map(.f = `[[`, "fitted") %>%
    map_dfc(.f = `[[`, ".fitted") %>%
    mutate(fitted = pmap_dbl(., function(...) estimate_center(c(...), type = type))) %>%
    pull(fitted)
  
  # Prepare actual values
  actual <- invoke(cbind, unclass(.data)[measured_vars(.data)])
  actual <- tail(x = as.numeric(actual), n = length(fitted))
  
  # Calculate residuals
  resid <- actual - fitted
  
  # Fill NAs in front of fitted values (adjust to equal length of actual values) 
  fitted <- c(rep(NA_real_, n_obs - length(fitted)), fitted)
  resid <- c(rep(NA_real_, n_obs - length(resid)), resid)
  
  # Model specification (inputs, reservoir size and outputs)
  model_spec <- paste0(
    "ESN",
    "(", 
    n_inputs, ",",
    n_res, ",",
    n_outputs,
    ")"
  )
  
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
#' @param ... Further arguments passed to bla
#' 
#' @return A fable
#' @export

forecast.ESN <- function(object,
                         new_data,
                         specials = NULL,
                         n_sim = NULL,
                         n_seed = 42,
                         type = "mode",
                         ...) {
  
  # Forecast fitted models
  model_fcst <- map_dfc(
    .x = object$model,
    .f = ~forecast_esn(
      object = .x,
      n_ahead = nrow(new_data),
      n_sim = NULL)$forecast[[".mean"]]
  )
  
  # Estimate point forecasts of forecast distribution
  fcst_point <- model_fcst %>%
    mutate(point = pmap_dbl(., function(...) estimate_center(c(...), type = type))) %>%
    pull(point)
  
  # Estimate standard deviation of forecast distribution
  fcst_std <- model_fcst %>%
    mutate(std = pmap_dbl(., function(...) sd(c(...)))) %>%
    pull(std)
  
  # Return forecast
  dist_normal(fcst_point, fcst_std)
}




#' @export
report.ESN <- function(object, ...) {
  
  method <- object$model$method
  
  n_inputs <- method$model_layer$n_inputs
  n_res <- method$model_layer$n_res
  n_outputs <- method$model_layer$n_outputs
  
  const <- method$model_inputs$const
  lags <- unlist(method$model_inputs$lags)
  
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

