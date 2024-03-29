
#' @title Automatic train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN)
#'   to a univariate time series.
#'
#' @param .data A \code{tsibble} containing the time series data.
#' @param specials Currently not is use.
#' @param ... Further arguments passed to \code{train_esn()}.
#'
#' @return An object of class \code{ESN}.
#' @export

auto_esn <- function(.data,
                     specials,
                     ...) {
  
  # Number of response variables
  n_outputs <- length(tsibble::measured_vars(.data))

  if (n_outputs > 1) {
    abort("Only univariate responses are supported by ESN.")
  }

  if(any(is.na(.data))){
    abort("ESN does not support missing values.")
  }
  
  # Train model ===============================================================
  
  # Prepare data as numeric vector
  y <- unclass(.data)[[measured_vars(.data)]]
  
  # Fit model
  model_fit <- train_esn(y = y, ...)
  
  # Return model and components
  structure(
    list(
      model = model_fit,
      est = list(
        .fitted = model_fit[["fitted"]],
        .resid = model_fit[["resid"]]),
      spec = model_fit$method[["model_spec"]]),
    class = "ESN"
    )
}



specials_esn <- new_specials()



#' @title Automatic train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN)
#'   to a univariate time series.
#'
#' @param formula Model specification (currently not in use).
#' @param ... Further arguments passed to \code{auto_esn()}.
#'
#' @return An object of class \code{ESN}.
#' @export

ESN <- function(formula, ...){
  esn_model <- new_model_class(
    model = "ESN",
    train = auto_esn,
    specials = specials_esn)
  
  new_model_definition(
    esn_model,
    !!enquo(formula),
    ...)
}



#' @title Forecast a trained ESN
#' 
#' @description Forecast a trained ESN.
#' 
#' @param object An object of class \code{ESN}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Currently not in use
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param ... Currently not in use.
#' 
#' @return An object of class \code{fable}.
#' @export

forecast.ESN <- function(object,
                         new_data,
                         specials = NULL,
                         xreg = NULL,
                         ...) {
  

  # Forecast models
  model_fcst <- forecast_esn(
    object = object[["model"]],
    n_ahead = NROW(new_data)
  )
  
  point <- model_fcst[["point"]]
  
  # Return forecast
  dist_normal(
    mu = point,
    sigma = NA_real_
    )
}



#' @title Extract fitted values from a trained ESN
#' 
#' @description Extract fitted values from a trained ESN.
#'
#' @param object An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return Fitted values extracted from the object.
#' @export

fitted.ESN <- function(object, ...){
  object$est[[".fitted"]]
}



#' @title Extract residuals from a trained ESN
#' 
#' @description Extract residuals from a trained ESN.
#'
#' @param object An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return Residuals extracted from the object.
#' @export

residuals.ESN <- function(object, ...){
  object$est[[".resid"]]
}



#' @title Provide a succinct summary of a trained ESN
#' 
#' @description Provide a succinct summary of a trained ESN.
#'
#' @param x An object of class \code{ESN}.
#'
#' @return Model summary extracted from the object.
#' @export

model_sum.ESN <- function(x){
  x[["spec"]]
}



#' @title Estimated coefficients
#' 
#' @description Return the estimated coefficients from a trained ESN as tibble.
#'
#' @param x An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return Coefficients extracted from the object.
#' @export

tidy.ESN <- function(x, ...) {
  
  wout <- x$model$method$model_weights$wout
  
  tibble(
    term = rownames(wout),
    estimate = as.numeric(wout)
  )
  
}



#' @title Summary of model fit
#' 
#' @description Return summary statistics from a trained ESN as tibble.
#'  \itemize{
#'    \item{\code{df}: Effective degrees of freedom.}
#'    \item{\code{aic}: Akaike information criterion.}
#'    \item{\code{bic}: Bayesian information criterion.}
#'    \item{\code{hqc}: Hannan-Quinn information criterion.}
#'       }
#'
#' @param x An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return Summary statistics extracted from the object.
#' @export

glance.ESN <- function(x, ...) {
  x$model$method$model_metrics
}



#' @title Provide a detailed summary of the trained ESN model
#' 
#' @description Provide a detailed summary of the trained ESN model.
#'
#' @param object An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return Print detailed model summary.
#' @export

report.ESN <- function(object, ...) {
  summary(object[["model"]])
}



#' @title Return the reservoir from a trained ESN as tibble
#' 
#' @description Return the reservoir (internal states) from a
#'   trained ESN as tibble. The function works only if the
#'   model within the \code{mdl_df} is of class \code{ESN}.
#'
#' @param object An object of class \code{mdl_df}.
#'
#' @return A tibble containing the reservoir (internal states).
#' @export

reservoir <- function(object) {
  UseMethod("reservoir")
}


#' @export
reservoir.mdl_df <- function(object) {
  
  object <- object %>%
    pivot_longer(
      cols = mable_vars(object),
      names_to = ".model",
      values_to = ".spec"
    )
  
  key_tbl <- object %>%
    as_tibble() %>%
    select(-c(.data$.spec))
  
  # Extract states_train
  object <- map(
    .x = seq_len(nrow(key_tbl)),
    .f = ~{
      lst_to_df(
        x = object[[".spec"]][[.x]]$fit$model$states_train,
        lst_name = "reservoir",
        col_name = "state"
      )
    }
  )
  
  # Add columns with key variables
  object <- map(
    .x = seq_len(nrow(key_tbl)),
    .f = ~{
      bind_cols(
        key_tbl[.x, ],
        object[[.x]])
    })
  
  # Flatten list row-wise
  object <- bind_rows(object)
  
  return(object)
}
