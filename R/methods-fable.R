
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
    stop("Only univariate responses are supported by ESN.")
  }

  if(any(is.na(.data))){
    stop("ESN does not support missing values.")
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



#' @title Train an Echo State Network
#' 
#' @description Train an Echo State Network (ESN) to a univariate time series. 
#'    The function automatically manages data pre-processing, reservoir
#'    generation (i.e., internal states) and model estimation and selection. 
#'    The function is a wrapper for \code{train_esn()} and intended to be used 
#'    in combination with \code{fabletools::model()}.
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



#' @title Model specification of a trained ESN model
#' 
#' @description Provides a compact overview of the model specification in the 
#'    format \code{ESN({n_states, alpha, rho}, {n_models, df})}.
#'
#' @param x An object of class \code{mdl_df}, containing an ESN model.
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



#' @title Forecast an Echo State Network
#' 
#' @description Forecast an Echo State Network (ESN) from a trained model via
#'    recursive forecasting. Forecast intervals are generated by simulating
#'    future sample path based on a moving block bootstrap of the residuals and
#'    estimating the quantiles from the simulations. The function is a wrapper
#'    for \code{forecast_esn()} and intended to be used in combination with
#'    \code{fabletools::model()}.
#' 
#' @param object An object of class \code{mdl_df}, containing an ESN model.
#' @param new_data Forecast horizon (n-step ahead forecast).
#' @param normal Logical value. If \code{TRUE}, dist_normal() is used, otherwise dist_sample().
#' @param n_sim Integer value. The number of future sample path generated during simulation.
#' @param specials Currently not in use.
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param ... Currently not in use.
#' 
#' @return An object of class \code{fbl_ts} ("fable").
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
                         normal = TRUE,
                         n_sim = 200,
                         specials = NULL,
                         xreg = NULL,
                         ...) {
  
  # Forecast model
  model_fcst <- forecast_esn(
    object = object[["model"]],
    n_ahead = NROW(new_data),
    n_sim = n_sim
  )
  
  if (normal == TRUE) {
    # Extract point forecasts
    point <- model_fcst[["point"]]
    # Extract standard deviations
    std <- model_fcst[["std"]]
    # Create normal distribution
    dist_normal(mu = point, sigma = std)
    
  } else {
    # Extract simulated future sample path
    sim <- model_fcst[["sim"]]
    # Split matrix row-wise into list
    sim <- split(sim, row(sim))
    # Create sample distribution
    dist_sample(x = sim)
  }
}



#' @title Extract fitted values from a trained ESN
#' 
#' @description Extract fitted values from a trained ESN as \code{tsibble}.
#'
#' @param object An object of class \code{mdl_df}, containing an ESN model.
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
#' @description Extract residuals from a trained ESN as \code{tsibble}.
#'
#' @param object An object of class \code{mdl_df}, containing an ESN model.
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
#' @description Return the estimated coefficients from a trained ESN as 
#'    \code{tibble}.
#'
#' @param x An object of class \code{mdl_df}, containing an ESN model.
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
#'   search as \code{tibble}.
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
#' @param x An object of class \code{mdl_df}, containing an ESN model.
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
#' @description Provide a detailed summary of the trained ESN model. The 
#'    function is a wrapper for \code{summary.esn()}.
#'
#' @param object An object of class \code{mdl_df}, containing an ESN model.
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



#' @title Filter ESN models
#' 
#' @description Filter an object of class \code{mdl_df} ("mable") to include 
#'   ESN models only, i.e., other models like ARIMA or ETS are excluded from
#'   the mable.
#'
#' @param object An object of class \code{mdl_df}, containing an ESN model.
#'
#' @return An object of class \code{mdl_df} in long-format.
#' 
#' @examples
#' library(tsibble)
#' library(fable)
#' AirPassengers %>%
#' as_tsibble() %>%
#' model("ESN" = ESN(value)) %>%
#' filter_esn()
#' 
#' @export

filter_esn <- function(object) {
  UseMethod("filter_esn")
}


#' @export
filter_esn.mdl_df <- function(object) {
  object <- object %>%
    pivot_longer(
      cols = mable_vars(object),
      names_to = "model",
      values_to = ".spec") %>%
    mutate(ESN = NA)
  
  for (i in seq_len(nrow(object))) {
    object[["ESN"]][i] <- is.esn(object[[".spec"]][[i]][["fit"]][["model"]])
  }
  
  object <- object %>%
    filter(ESN == TRUE) %>%
    select(-ESN)
  
  return(object)
}


#' @title Return the reservoir from a trained ESN as tibble
#' 
#' @description Return the reservoir (internal states) from a
#'   trained ESN as tibble. The function works only for models
#'   of class \code{ESN}.
#'
#' @param object An object of class \code{mdl_df}, containing an ESN model.
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
  
  # Extract ESN models
  object <- object %>%
    filter_esn()
  
  # Extract key variables
  key_tbl <- object %>%
    as_tibble() %>%
    select(-c(.data$.spec))
  
  # Pre-allocate empty list to store results
  states <- vector("list", nrow(object))
  
  for (i in seq_len(nrow(object))) {
    
    # Extract the internal states per model
    xstates <- object[[".spec"]][[i]][["fit"]][["model"]][["states_train"]]
    
    # Add index and reshape to long-format tibble
    xstates <- xstates %>%
      as_tibble() %>%
      mutate(index = seq_len(nrow(xstates))) %>%
      pivot_longer(
        cols = -index,
        names_to = "state",
        values_to = "value") %>% 
      arrange(.data$state, .data$index)
    
    states[[i]] <- bind_cols(key_tbl[i, ], xstates)
  }
  
  # Flatten list row-wise
  states <- bind_rows(states)
  
  return(states)
}
