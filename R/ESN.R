
#' @title Train an Echo State Network (ESN).
#' 
#' @description Train an Echo State Network (ESN).
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_esn}.
#' @param ... Further arguments passed to \code{echos::auto_esn(...)}.
#'
#' @return An object of class \code{ESN}.

train <- function(.data, specials, ...){
  
  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by ESN")
  }
  
  # Prepare data for modelling
  model_data <- .data
  
  if(any(is.na(model_data))){
    abort("ESN does not support missing values.")
  }
  
  # Train model
  mdl <- echos::auto_esn(data = model_data, ...)
  
  # Extract length of actual values and fitted values
  fitted <- mdl$fitted$.value
  resid <- mdl$resid$.value
  
  n_total <- nrow(model_data)
  n_fitted <- length(fitted)
  
  # Fill NAs in front of fitted values (adjust to equal length of actual values) 
  fitted <- c(rep(NA_real_, n_total - n_fitted), fitted)
  resid <- c(rep(NA_real_, n_total - n_fitted), resid)
  
  # Model specification
  model_spec <- mdl$method$model_spec
  
  # Return model
  structure(
    list(
      model = mdl,
      est = list(
        .fitted = fitted,
        .resid = resid),
      spec = model_spec),
    class = "ESN")
}


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
    train = train,
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
                         ...) {
  # Extract model
  mdl <- object$model
  
  # Forecast model
  fcst <- forecast_esn(
    object = mdl,
    n_ahead = nrow(new_data))
  
  # Extract point forecasts
  mean <- fcst$forecast$.value
  
  # Extract simulations
  sim <- fcst$simulation %>%
    select(-.variable) %>%
    spread(
      key = .sim,
      value = .value)
  
  sim <- invoke(cbind, unclass(sim)[measured_vars(sim)])
  sd <- rowSds(sim, na.rm = TRUE)
  
  # Return forecast
  construct_fc(
    point = mean,
    sd = sd,
    dist = dist_normal(
      mean = mean,
      sd = sd))
}






