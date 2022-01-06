
#' @title Automatic train an Autoregressive model
#' 
#' @description This function trains an Autoregressive model (ARX)
#'   to a univariate time series.
#'
#' @param .data A \code{tsibble} containing the time series data.
#' @param specials Currently not is use.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @inheritParams train_arx
#'
#' @return An object of class \code{ARX}.
#' @export

auto_arx <- function(.data,
                     specials,
                     lags = NULL,
                     fourier = NULL,
                     xreg = NULL,
                     dy = 0,
                     dx = 0,
                     inf_crit = "aic",
                     n_models = 500,
                     n_seed = 42,
                     scale_inputs = c(-1, 1)) {
  
  # Number of response variables
  n_outputs <- length(tsibble::measured_vars(.data))
  # Number of observations
  n_obs <- nrow(.data)
  # Initial throw-off (drop observations at the beginning)
  n_initial <- floor(n_obs * 0.05)
  
  if (is.null(dy)) {
    dy <- ndiffs(as.ts(.data))
  }
  
  if (n_outputs > 1) {
    abort("Only univariate responses are supported by ARX.")
  }
  
  if(any(is.na(.data))){
    abort("ARX does not support missing values.")
  }
  
  # # Maximum seasonal period which is feasible
  # period <- common_periods(.data)
  # period <- sort(as.numeric(period[period < n_obs]))
  
  # Tune model inputs (lags and fourier terms) ================================
  
  model_inputs <- tune_inputs(
    data = .data,
    lags = lags,
    fourier = fourier,
    xreg = xreg,
    dy = dy,
    dx = dx,
    n_initial = n_initial,
    scale_inputs = scale_inputs,
    inf_crit = inf_crit,
    n_models = n_models,
    n_seed = n_seed
  )
  
  lags <- model_inputs$lags
  fourier <- model_inputs$fourier
  
  # Train final model =========================================================
  
  model_fit <- train_arx(
    data = .data,
    lags = lags,
    fourier = fourier,
    xreg = xreg,
    dy = dy,
    dx = dx,
    inf_crit = inf_crit,
    n_initial = n_initial,
    scale_inputs = scale_inputs
  )
  
  # Return model and components
  structure(
    list(
      model = model_fit,
      est = list(
        .fitted = model_fit[["fitted"]],
        .resid = model_fit[["resid"]]),
      spec = model_fit$method[["model_spec"]]),
    class = "ARX"
    )
}





specials_arx <- new_specials()


#' @title Automatic train an Autoregressive model
#' 
#' @description This function trains an Autoregressive model (ARX)
#'   to a univariate time series.
#'
#' @param formula Model specification (currently not in use).
#' @param ... Further arguments passed to \code{auto_arx()}.
#'
#' @return An object of class \code{ARX}.
#' @export

ARX <- function(formula, ...){
  arx_model <- new_model_class(
    model = "ARX",
    train = auto_arx,
    specials = specials_arx)
  
  new_model_definition(
    arx_model,
    !!enquo(formula),
    ...)
}





#' @title Forecast a trained ARX
#' 
#' @description Forecast a trained ARX.
#' 
#' @param object An object of class \code{ARX}.
#' @param new_data Forecast horizon (n-step ahead forecast)
#' @param specials Currently not in use
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param ... Currently not in use.
#' 
#' @return An object of class \code{fable}.
#' @export

forecast.ARX <- function(object,
                         new_data,
                         specials = NULL,
                         xreg = NULL,
                         ...) {
  
  # Forecast model
  model_fcst <- forecast_arx(
    object = object$model,
    n_ahead = nrow(new_data),
    xreg = xreg
    )
  
  # Return forecast
  dist_normal(
    mu = model_fcst$point,
    sigma = NA_real_
    )
}



#' @title Extract fitted values from a trained ARX
#' 
#' @description Extract fitted values from a trained ARX.
#'
#' @param object An object of class \code{ARX}.
#' @param ... Currently not in use.
#'
#' @return Fitted values extracted from the object.
#' @export

fitted.ARX <- function(object, ...){
  object$est[[".fitted"]]
}



#' @title Extract residuals from a trained ARX
#' 
#' @description Extract residuals from a trained ARX.
#'
#' @param object An object of class \code{ARX}.
#' @param ... Currently not in use.
#'
#' @return Residuals extracted from the object.
#' @export

residuals.ARX <- function(object, ...){
  object$est[[".resid"]]
}



#' @title Provide a succinct summary of a trained ARX
#' 
#' @description Provide a succinct summary of a trained ARX.
#'
#' @param object An object of class \code{ARX}.
#'
#' @return Model summary extracted from the object.
#' @export

model_sum.ARX <- function(object){
  object$spec
}



#' @title Estimated coefficients
#' 
#' @description Return the estimated coefficients from a trained ARX as tibble.
#'
#' @param object An object of class \code{ARX}.
#'
#' @return Coefficients extracted from the object.
#' @export

tidy.ARX <- function(object) {
  
  wout <- object$model$method$model_weights$wout
  
  tibble(
    term = rownames(wout),
    estimate = as.numeric(wout)
  )
  
}



#' @title Summary of model fit
#' 
#' @description Return summary statistics from a trained ARX as tibble.
#'  \itemize{
#'    \item{\code{df}: Effective degrees of freedom.}
#'    \item{\code{aic}: Akaike information criterion.}
#'    \item{\code{bic}: Bayesian information criterion.}
#'    \item{\code{hqc}: Hannan-Quinn information criterion.}
#'       }
#'
#' @param object An object of class \code{ARX}.
#'
#' @return Summary statistics extracted from the object.
#' @export

glance.ARX <- function(object) {
  object$model$method$model_metrics
}




#' @title Provide a detailed summary of a trained ARX
#' 
#' @description Provide a detailed summary of a trained ARX.
#'
#' @param object An object of class \code{ARX}.
#'
#' @return Print detailed model summary.
#' @export

report.ARX <- function(object) {
  
  method <- object$model$method
  
  # Layers
  n_inputs <- method$model_layer$n_inputs
  n_outputs <- method$model_layer$n_outputs
  
  # Inputs
  lags <- unlist(method$model_inputs$lags)
  fourier <- method$model_inputs$fourier
  
  if (is.null(fourier)) {
    fourier <- NA
  } else {
    fourier <- paste(
      "{",
      paste("(", fourier[[1]], "-", fourier[[2]], ")",
            collapse = ",",
            sep = ""),
      "}",
      sep = "")
  }
  
  # Differences
  dy <- as.numeric(method$model_inputs$dy)
  
  if (is.null(method$model_data$xx)) {
    xreg <- NA
    dx <- NA
  } else {
    xreg <- colnames(method$model_data$xx)
    dx <- as.numeric(method$model_inputs$dx)
  }
  
  # Scaling
  scale_inputs <- method$scale_inputs
  
  cat(
    "\n--- Layers -----------------------------------------------------", "\n",
    "n_inputs  = ", n_inputs, "\n",
    "n_outputs = ", n_outputs, "\n"
  )
  
  cat(
    "\n--- Inputs -----------------------------------------------------", "\n",
    "lags    = ", paste0("(", paste0(lags, collapse = ", "), ")"), "\n",
    "fourier = ", fourier, "\n",
    "xreg    = ", xreg, "\n"
  )
  
  cat(
    "\n--- Differences ------------------------------------------------", "\n",
    "dy = ", dy, "\n",
    "dx = ", dx, "\n"
  )
  
  cat(
    "\n--- Scaling ----------------------------------------------------", "\n",
    "scale_inputs = ", "[", scale_inputs[1], ", ", scale_inputs[2], "]", "\n",
    sep = ""
  )
  
}



#' @title Extract and return values from a trained ARX as tibble
#' 
#' @description Extract and return values from a trained ARX
#'   as tibble. The function works only if the
#'   model within the \code{mdl_df} is of class \code{ARX}.
#'   The extracted values are are stored in a tibble with
#'   the following columns:
#'   
#'   \itemize{
#'     \item{\code{spec}: Character value. Succinct summary of model specifications.}
#'     \item{\code{n_inputs}: Integer value. The number of model inputs.}
#'     \item{\code{n_outputs}: Integer value. The number of model outputs.}
#'     \item{\code{lags}: A \code{list} containing integer vectors with the lags associated with each input variable.}
#'     \item{\code{fourier}: A \code{list} containing the fourier terms.}
#'     \item{\code{dy}: Integer vector. The nth-differences of the response variable.}
#'     \item{\code{dx}: Integer vector. The nth-differences of the exogenous variable.}
#'     }
#'
#' @param object An object of class \code{mdl_df}.
#'
#' @return A tibble containing the hyper-parameters.
#' @export

extract_arx <- function(object) {
  UseMethod("extract_esn")
}


#' @export
extract_arx.mdl_df <- function(object) {
  
  object <- object %>%
    pivot_longer(
      cols = mable_vars(object),
      names_to = ".model",
      values_to = ".spec"
    )
  
  key_tbl <- object %>%
    as_tibble() %>%
    select(-c(.data$.spec))
  
  # Extract model details
  mdl_tbl <- map(
    .x = seq_len(nrow(key_tbl)),
    .f = ~{
      
      lst_mdl <- object[[".spec"]][[.x]]$fit$model$method
      
      tibble(
        spec       = lst_mdl[["model_spec"]],
        n_inputs   = lst_mdl[["model_layers"]][["n_inputs"]],
        n_res      = lst_mdl[["model_layers"]][["n_res"]],
        n_states   = lst_mdl[["model_layers"]][["n_states"]],
        n_outputs  = lst_mdl[["model_layers"]][["n_outputs"]],
        n_models   = lst_mdl[["model_ensemble"]][["n_models"]],
        n_best     = lst_mdl[["model_ensemble"]][["n_best"]],
        n_vars     = lst_mdl[["model_ensemble"]][["n_vars"]],
        lags       = list(lst_mdl[["model_inputs"]][["lags"]]),
        fourier    = list(lst_mdl[["model_inputs"]][["fourier"]]),
        dy         = lst_mdl[["model_inputs"]][["dy"]],
        dx         = lst_mdl[["model_inputs"]][["dx"]]
      )
    })
  
  # Add columns with key variables
  object <- map(
    .x = seq_len(nrow(key_tbl)),
    .f = ~{
      bind_cols(
        key_tbl[.x, ],
        mdl_tbl[[.x]]
      )
    })
  
  # Flatten list row-wise
  object <- bind_rows(object)
  
  return(object)
}

