
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
#' @noRd

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
#' @param ... Further arguments passed to \code{train_esn()}.
#'
#' @return An object of class \code{ESN}.
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value))
#' 
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



#' @title Provide a succinct summary of a trained ESN
#' 
#' @description Provide a succinct summary of a trained ESN.
#'
#' @param x An object of class \code{ESN}.
#'
#' @return Model summary extracted from the object.
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value))
#' 
#' @export

model_sum.ESN <- function(x){
  x[["spec"]]
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
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' forecast(h = 18)
#' 
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
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' fitted()
#' 
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
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' residuals()
#' 
#' @export

residuals.ESN <- function(object, ...){
  object$est[[".resid"]]
}



#' @title Estimated coefficients
#' 
#' @description Return the estimated coefficients from a trained ESN as tibble.
#'
#' @param x An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return Coefficients extracted from the object.
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' tidy()
#' 
#' @export

tidy.ESN <- function(x, ...) {
  
  wout <- x$model$method$model_weights$wout
  
  tibble(
    term = rownames(wout),
    estimate = as.numeric(wout)
  )
  
}



#' @title Summary of trained models during random search
#' 
#' @description Return summary statistics from trained ESN models during random 
#'   search as tibble.
#'  \itemize{
#'    \item{\code{model}: Model identifier.}
#'    \item{\code{loglik}: Log-likelihood.}
#'    \item{\code{nobs}: Number of observations.}
#'    \item{\code{df}: Effective degrees of freedom.}
#'    \item{\code{lambda}: Regularization parameter.}
#'    \item{\code{aic}: Akaike Information Criterion.}
#'    \item{\code{aicc}: Corrected Akaike Information Criterion.}
#'    \item{\code{bic}: Bayesian Information Criterion.}
#'    \item{\code{hqc}: Hannan-Quinn Information Criterion.}
#'    \item{\code{mse}: Mean Squared Error.}
#'    \item{\code{mae}: Mean Absolute Error.}
#'       }
#'
#' @param x An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return Summary statistics extracted from the object.
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' glance()
#' 
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
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' report()
#' 
#' @export

report.ESN <- function(object, ...) {
  summary(object[["model"]])
}



#' @title Return the reservoir from a trained ESN as tibble
#' 
#' @description Return the reservoir (internal states) from a
#'   trained ESN as tibble. The function works only for models
#'   of class \code{ESN}.
#'
#' @param object An object of class \code{ESN}.
#'
#' @return A tibble containing the reservoir (internal states).
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' reservoir()
#' 
#' @export

reservoir <- function(object) {
  UseMethod("reservoir")
}


#' @export
reservoir.mdl_df <- function(object) {
  
  object <- object %>%
    pivot_longer(
      cols = mable_vars(object),
      names_to = "model",
      values_to = ".spec"
    )
  
  key_tbl <- object %>%
    as_tibble() %>%
    select(-c(.data$.spec))
  
  # Extract states_train
  object <- map(
    .x = seq_len(nrow(key_tbl)),
    .f = ~{
      
      # Extract model object
      xmodel <- object[[".spec"]][[.x]]$fit$model
      
      # Check if model is of class "esn"
      # Extract internal states and prepare as tibble
      if (is.esn(xmodel)) {
        xstates <- xmodel[["states_train"]] %>%
          as_tibble() %>%
          mutate(index = row_number()) %>%
          pivot_longer(
            cols = -index,
            names_to = "state",
            values_to = "value") %>%
          arrange(state, index)
        
        xstates <- bind_cols(key_tbl[.x, ], xstates)
      } else {
        xstates <- NULL
      }
    }
  )
  
  # Flatten list row-wise
  object <- bind_rows(object)
  
  return(object)
}
