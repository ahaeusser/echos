
#' @title Automatic train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN)
#'   to a univariate time series.
#'
#' @param .data A \code{tsibble} containing the time series data.
#' @param specials Currently not is use.
#' @inheritParams train_esn
#'
#' @return An object of class \code{ESN}.
#' @export

auto_esn <- function(.data,
                     specials,
                     lags = NULL,
                     fourier = NULL,
                     xreg = NULL,
                     dy = 0,
                     dx = 0,
                     n_models = 500,
                     inf_crit = "aic",
                     max_states = 30,
                     n_best = 50,
                     n_initial = 100,
                     n_res = 200,
                     n_seed = 42,
                     alpha = 0.8,
                     rho = 1,
                     density = 0.1,
                     scale_win = 0.1,
                     scale_wres = 0.5,
                     scale_inputs = c(-1, 1)) {
  
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
  
  # Train final model =========================================================
  
  model_fit <- train_esn(
    data = .data,
    lags = lags,
    fourier = fourier,
    xreg = xreg,
    dy = dy,
    dx = dx,
    n_models = n_models,
    inf_crit = inf_crit,
    max_states = max_states,
    n_best = n_best,
    n_res = n_res,
    n_initial = n_initial,
    n_seed = n_seed,
    alpha = alpha,
    rho = rho,
    density = density,
    scale_win = scale_win,
    scale_wres = scale_wres,
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
#' @param n_sim Integer value. The number of future sample path to generate.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param ... Currently not in use.
#' 
#' @return An object of class \code{fable}.
#' @export

forecast.ESN <- function(object,
                         new_data,
                         specials = NULL,
                         n_seed = 42,
                         xreg = NULL,
                         ...) {
  
  # Forecast model
  model_fcst <- forecast_esn(
    object = object$model,
    n_ahead = nrow(new_data),
    n_seed = n_seed,
    xreg = xreg
    )
  
  # Return forecast
  dist_normal(
    mu = model_fcst$point,
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
#' @param object An object of class \code{ESN}.
#'
#' @return Model summary extracted from the object.
#' @export

model_sum.ESN <- function(object){
  object$spec
}



#' @title Estimated coefficients
#' 
#' @description Return the estimated coefficients from a trained ESN as tibble.
#'
#' @param object An object of class \code{ESN}.
#'
#' @return Coefficients extracted from the object.
#' @export

tidy.ESN <- function(object) {
  
  wout <- object$model$method$model_weights$wout
  
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
#' @param object An object of class \code{ESN}.
#'
#' @return Summary statistics extracted from the object.
#' @export

glance.ESN <- function(object) {
  object$model$method$model_metrics
}




#' @title Provide a detailed summary of a trained ESN
#' 
#' @description Provide a detailed summary of a trained ESN.
#'
#' @param object An object of class \code{ESN}.
#'
#' @return Print detailed model summary.
#' @export

report.ESN <- function(object) {
  
  method <- object$model$method
  
  n_inputs <- method$model_layer$n_inputs
  n_res <- method$model_layer$n_res
  n_outputs <- method$model_layer$n_outputs
  
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
  
  dy <- as.numeric(method$model_inputs$dy)
  
  if (is.null(method$model_data$xx)) {
    xreg <- NA
    dx <- NA
  } else {
    xreg <- colnames(method$model_data$xx)
    dx <- as.numeric(method$model_inputs$dx)
  }
  
  alpha <- round(method$model_pars$alpha, 3)
  rho <- round(method$model_pars$rho, 3)
  lambda <- round(method$model_pars$lambda, 3)
  density <- round(method$model_pars$density, 3)
  
  dof <- round(method$model_metrics$dof, 3)
  aic <- round(method$model_metrics$aic, 3)
  bic <- round(method$model_metrics$bic, 3)
  hqc <- round(method$model_metrics$hqc, 3)
  
  scale_win <- round(method$scale_win, 3)
  scale_wres <- round(method$scale_wres, 3)
  scale_inputs <- round(method$scale_inputs, 3)
  
  cat(
    "\nNetwork size:", "\n",
    "Inputs    = ", n_inputs, "\n",
    "Reservoir = ", n_res, "\n",
    "Outputs   = ", n_outputs, "\n"
  )
  
  cat(
    "\nModel inputs:", "\n",
    "lags    = ", lags, "\n",
    "fourier = ", fourier, "\n",
    "xreg    = ", xreg, "\n"
  )
  
  cat(
    "\nDifferences:", "\n",
    "dy = ", dy, "\n",
    "dx = ", dx, "\n"
  )
  
  cat(
    "\nScaling (input data): \n",
    "scale_inputs = ", "(", scale_inputs[1], ", ", scale_inputs[2], ")", "\n",
    sep = ""
  )
  
  cat(
    "\nScaling (weights): \n",
    "scale_win  = ", "(", -scale_win, ", ", scale_win, ")", "\n",
    "scale_wres = ", "(", -scale_wres, ", ", scale_wres, ")", "\n",
    sep = ""
  )
  
  cat(
    "\nHyperparameters:", "\n",
    "alpha   = ", alpha, "\n",
    "rho     = ", rho, "\n",
    "lambda  = ", lambda, "\n",
    "density = ", density, "\n"
  )
  
  cat(
    "\nMetrics:", "\n",
    "dof  = ", dof, "\n",
    "aic  = ", aic, "\n",
    "bic  = ", bic, "\n",
    "hqc  = ", hqc, "\n"
  )
  
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
      object[[".spec"]][[.x]]$fit$model$states_train %>%
        as_tibble() %>%
        mutate(idx = row_number()) %>%
        relocate(.data$idx) %>%
        pivot_longer(
          cols = -.data$idx,
          names_to = "state",
          values_to = "value") %>%
        arrange(.data$state)
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




#' @title Extract and return values from a trained ESN as tibble
#' 
#' @description Extract and return values from a trained ESN
#'   as tibble. The function works only if the
#'   model within the \code{mdl_df} is of class \code{ESN}.
#'   The extracted values are are stored in a tibble with
#'   the following columns:
#'   
#'   \itemize{
#'     \item{\code{spec}: Character value. Succinct summary of model specifications.}
#'     \item{\code{n_inputs}: Integer value. The number of model inputs.}
#'     \item{\code{n_res}: Integer value. The number of internal states within the reservoir (hidden layer).}
#'     \item{\code{n_outputs}: Integer value. The number of model outputs.}
#'     \item{\code{lags}: A \code{list} containing integer vectors with the lags associated with each input variable.}
#'     \item{\code{fourier}: A \code{list} containing the fourier terms.}
#'     \item{\code{dy}: Integer vector. The nth-differences of the response variable.}
#'     \item{\code{dx}: Integer vector. The nth-differences of the exogenous variable.}
#'     \item{\code{scale_win}: Numeric value. The lower and upper bound of the uniform distribution for scaling the input weight matrix.}
#'     \item{\code{scale_wres}: Numeric value. The lower and upper bound of the uniform distribution for scaling the reservoir weight matrix.}
#'     \item{\code{alpha}: Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.}
#'     \item{\code{rho}: Numeric value. The spectral radius for scaling the reservoir weight matrix.}
#'     \item{\code{lambda}: Numeric value. The regularization (shrinkage) parameter for ridge regression.}
#'     \item{\code{density}: Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).}
#'     \item{\code{df}: Numeric value. The effective degree of freedom.}
#'     \item{\code{aic}: Numeric value. The Akaike information criterion.}
#'     \item{\code{bic}: Numeric value. The Bayesian information criterion.}
#'     \item{\code{hqc}: Numeric value. The Hannan-Quinn criterion.}
#'     }
#'
#' @param object An object of class \code{mdl_df}.
#'
#' @return A tibble containing the hyper-parameters.
#' @export

extract_esn <- function(object) {
  UseMethod("extract_esn")
}


#' @export
extract_esn.mdl_df <- function(object) {
  
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
        n_outputs  = lst_mdl[["model_layers"]][["n_outputs"]],
        lags       = list(lst_mdl[["model_inputs"]][["lags"]]),
        fourier    = list(lst_mdl[["model_inputs"]][["fourier"]]),
        dy         = lst_mdl[["model_inputs"]][["dy"]],
        dx         = lst_mdl[["model_inputs"]][["dx"]],
        scale_win  = lst_mdl[["scale_win"]],
        scale_wres = lst_mdl[["scale_wres"]],
        alpha      = lst_mdl[["model_pars"]][["alpha"]],
        rho        = lst_mdl[["model_pars"]][["rho"]],
        lambda     = lst_mdl[["model_pars"]][["lambda"]],
        density    = lst_mdl[["model_pars"]][["density"]],
        dof        = lst_mdl[["model_metrics"]][["dof"]],
        aic        = lst_mdl[["model_metrics"]][["aic"]],
        bic        = lst_mdl[["model_metrics"]][["bic"]],
        hqc        = lst_mdl[["model_metrics"]][["hqc"]]
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

