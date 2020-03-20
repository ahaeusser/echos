#' @title Train ESN.
#' 
#' @description Train an Echo State Network (ESN).
#'
#' @param .data Input data as tsibble.
#' @param specials Specials as list defined in \code{specials_esn}.
#' @param ... Further arguments passed to \code{echos::auto_esn(...)}.
#'
#' @return An object of class \code{fbl_esn}.

train <- function(.data, specials, ...){
  
  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by ESN")
  }
  
  # Prepare data for modelling
  model_data <- .data
  
  if(any(is.na(model_data))){
    abort("ELM does not support missing values.")
  }
  
  # Train model
  mdl <- echos::auto_esn(data = model_data, ...)
  
  
  # Extract length of actual values and fitted values
  fitted <- mdl$fitted$.value
  resid <- mdl$resid$.value
  
  n_total <- nrow(model_data)
  n_fitted <- length(fitted)
  
  # alpha <- round(mdl$method$pars$alpha, 2)
  # rho <- round(mdl$method$pars$rho, 2)
  # lambda <- round(mdl$method$pars$lambda, 2)
  # density <- round(mdl$method$pars$density, 2)
  # pars <- c(alpha, rho, lambda, density)
  
  pars <- round(unlist(mdl$method$pars), 2)
  
  # Fill NAs in front of fitted values (adjust to equal length of actual values) 
  fitted <- c(rep(NA_real_, n_total - n_fitted), fitted)
  resid <- c(rep(NA_real_, n_total - n_fitted), resid)
  
  
  
  # Return model
  structure(
    list(
      model = mdl,
      est = list(
        .fitted = fitted,
        .resid = resid),
      spec = pars),
    class = "fbl_esn")
}




specials_esn <- new_specials()

#' @title Automatic training of ESNs
#'
#' @param formula Model specification (see "Specials" section, currently not in use...)
#' @param ... Further arguments passed to \code{echos::auto_esn(...)}.
#'
#' @return esn_model An object of class \code{fbl_esn}.
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


#' @title Forecast a ELM.
#'
#' @description Forecast a Extreme Learning Machine (ELM).
#'
#' @param object The time series model used to produce the forecasts.
#' @param new_data A tsibble containing future information used to forecast.
#' @param specials Specials are currently not in use for MLPs.
#' @param times The number of sample paths to use in estimating the forecast distribution when \code{bootstrap = TRUE}.
#' @param ... Additional arguments for forecast model method.
#'
#' @return An object of class "fable".
#' @export

forecast.fbl_esn <- function(object,
                             new_data,
                             specials = NULL,
                             times = 1000,
                             ...){
  # Extract model
  mdl <- object$model
  # Forecast model
  fcst <- forecast_esn(mdl, h = nrow(new_data))

  # Extract point forecast and simulations
  mean <- fcst$mean
  sim <- fcst$all.mean


  # Return forecasts
  construct_fc(
    point = mean,
    sd = rowSds(sim),
    dist = dist_normal(mean = mean, sd = rowSds(sim)))
}


#' @title Extract fitted values from a ELM.
#' 
#' @description Extract fitted values from a ELM.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Additional arguments passed to ...
#'
#' @return
#' @export

fitted.fbl_esn <- function(object, ...){
  object$est[[".fitted"]]
}


#' @title Extract residuals from a MLP.
#' 
#' @description Extract residuals from a MLP.
#'
#' @param object The time series model used to produce the forecast.
#' @param ... Additional arguments passed to ...
#'
#' @return
#' @export

residuals.fbl_esn <- function(object, ...){
  object$est[[".resid"]]
}

#' @title Provide a succinct summary of the MLP.
#' 
#' @description Provide a succinct summary of the MLP.
#'
#' @param object The MLP to summarize.
#'
#' @return
#' @export

model_sum.fbl_esn <- function(x){
  paste("ESN(", "{", x$spec[1], ",", x$spec[2], ",", x$spec[3], "}", ")", sep = "")
}
