
#' @title Automatic train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN)
#'   to a univariate time series.
#'
#' @param .data A \code{tsibble} containing the time series data.
#' @param specials Currently not is use.
#' @param const Logical value. If \code{TRUE}, a constant term (intercept) is used.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles per period).
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_res Integer value. The number of internal states within the reservoir (hidden layer).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param alpha Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.
#' @param rho Numeric value. The spectral radius for scaling the reservoir weight matrix.
#' @param lambda Numeric value. The regularization (shrinkage) parameter for ridge regression.
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("aic", "bic", "hq")}.
#' @param control_inputs A \code{list} containing control values for the automatic selection of model inputs:
#'  \itemize{
#'    \item{\code{tune}: Logical value. If \code{TRUE}, the model inputs are tuned, otherwise model inputs are used as defined.}
#'    \item{\code{n_sample}: Integer value. The number of samples for the random grid.}
#'       }
#' @param control_pars A \code{list} containing control values for the hyperparameter tuning:
#'  \itemize{
#'    \item{\code{tune}: Logical value. If \code{TRUE}, the hyperparameters are tuned, otherwise hyperparameters are used as defined.}
#'    \item{\code{lower}: Numeric vector. The lower bounds for \code{alpha}, \code{rho} and \code{lambda} used in the optimization.}
#'    \item{\code{upper}: Numeric vector. The upper bounds for \code{alpha}, \code{rho} and \code{lambda} used in the optimization.}
#'       }
#' @param ... Further arguments passed to \code{stats::optim()}
#'
#' @return An object of class \code{ESN}.
#' @export

auto_esn <- function(.data,
                     specials,
                     const = TRUE,
                     lags = NULL,
                     n_fourier = NULL,
                     n_initial = 10,
                     n_res = 200,
                     n_seed = 42,
                     alpha = 0.8,
                     rho = 1,
                     lambda = 1,
                     density = 0.1,
                     scale_inputs = c(-1, 1),
                     scale_runif = c(-0.5, 0.5),
                     inf_crit = "bic",
                     control_inputs = list(
                       tune = TRUE,
                       n_sample = 1000),
                     control_pars = list(
                       tune = TRUE,
                       lower = c(0, 0.1, 0.1),
                       upper = c(1, 5, 10)),
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
  
  # Maximum seasonal period which is feasible
  period <- common_periods(.data)
  period <- sort(as.numeric(period[period < n_obs]))
  
  # Check stationarity of time series
  n_diff <- check_unitroots(
    .data = .data,
    alpha = 0.05)$n_diff
  
  
  # Select model inputs (lags, fourier terms) =================================
  
  if (is.null(lags)) {
    lags <- list(c(1, period))
  }
  
  if (control_inputs$tune == TRUE) {
    model_inputs <- tune_inputs(
      data = .data,
      lags = lags,
      n_fourier = n_fourier,
      period = period,
      n_diff = n_diff,
      n_initial = n_initial,
      scale_inputs = scale_inputs,
      inf_crit = inf_crit,
      n_sample = control_inputs$n_sample,
      n_seed = n_seed)
    
    const <- model_inputs$const
    lags <- model_inputs$lags
    n_fourier <- model_inputs$n_fourier
  }
  
  
  # Hyperparameter optimization ===============================================
  
  # Starting values
  pars <- c(alpha, rho, lambda)
  
  # Tune hyperparameters via L-BFGS-B
  if (control_pars$tune == TRUE) {
    
    # Lower and upper bounds (box constraints)
    lower <- control_pars$lower
    upper <- control_pars$upper
    
    model_pars <- optim(
      par = pars,
      fn = tune_pars,
      lower = lower,
      upper = upper,
      method = "L-BFGS-B",
      data = .data,
      lags = lags,
      const = const,
      n_fourier = n_fourier,
      period = period,
      n_diff = n_diff,
      density = density,
      n_initial = n_initial,
      n_res = n_res,
      inf_crit = inf_crit,
      scale_inputs = scale_inputs,
      scale_runif = scale_runif,
      n_seed = n_seed,
      ...)
    
    # Vector with optimized hyperparameters
    pars <- c(
      model_pars$par[1],
      model_pars$par[2],
      model_pars$par[3])
  }
  
  
  # Train final model =========================================================
  
  model_fit <- train_esn(
    data = .data,
    lags = lags,
    n_fourier = n_fourier,
    period = period,
    const = const,
    n_diff = n_diff,
    n_res = n_res,
    n_initial = n_initial,
    n_seed = n_seed,
    alpha = pars[1],
    rho = pars[2],
    lambda = pars[3],
    density = density,
    scale_inputs = scale_inputs,
    scale_runif = scale_runif)
  
  # Extract actual values and fitted values
  fitted <- model_fit$fitted[[".fitted"]]
  resid <- model_fit$resid[[".resid"]]
  # Get length of time series and fitted values
  n_total <- nrow(.data)
  n_fitted <- length(fitted)
  
  # Fill NAs in front of fitted values (adjust to equal length of actual values) 
  fitted <- c(rep(NA_real_, n_obs - length(fitted)), fitted)
  resid <- c(rep(NA_real_, n_obs - length(resid)), resid)
  
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





#' @title Extract fitted values from a trained ESN
#' 
#' @description Extract fitted values from a trained ESN.
#'
#' @param object An object of class \code{ESN}.
#' @param ... Currently not in use.
#'
#' @return
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
#' @return
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
#' @return
#' @export

model_sum.ESN <- function(object){
  object$spec
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
#' @param ... Currently not in use.
#' 
#' @return An object of class \code{fable}.
#' @export

forecast.ESN <- function(object,
                         new_data,
                         specials = NULL,
                         n_sim = 100,
                         n_seed = 42,
                         ...) {
  # Extract model
  model_fit <- object$model
  
  # Forecast model
  model_fcst <- forecast_esn(
    object = model_fit,
    n_ahead = nrow(new_data),
    n_sim = n_sim,
    n_seed = n_seed)
  
  # Extract point forecasts
  fcst_point <- model_fcst$forecast[[".mean"]]
  
  # Extract simulations
  sim <- model_fcst$simulation %>%
    select(-c(.response)) %>%
    spread(
      key = .path,
      value = .mean)
  
  sim <- invoke(cbind, unclass(sim)[measured_vars(sim)])
  fcst_std <- rowSds(sim, na.rm = TRUE)
  
  # Return forecast
  dist_normal(fcst_point, fcst_std)
  
}





#' @title Provide a detailed summary of a trained ESN
#' 
#' @description Provide a detailed summary of a trained ESN.
#'
#' @param object An object of class \code{ESN}.
#'
#' @return
#' @export

report.ESN <- function(object) {
  
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
    "Inputs        = ", n_inputs, "\n",
    "Reservoir     = ", n_res, "\n",
    "Outputs       = ", n_outputs, "\n"
  )
  
  cat(
    "\nModel inputs:", "\n",
    "Constant = ", const, "\n",
    "Lags     = ", lags, "\n"
  )
  
  cat(
    "\nDifferences = ", n_diff, "\n"
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
    "aic =", aic, "\n",
    "bic =", bic, "\n",
    "hq  =", hq, "\n"
  )
}






#' @title Estimated coefficients
#' 
#' @description Return the estimated coefficients from a trained ESN as tibble.
#'
#' @param object An object of class \code{ESN}.
#'
#' @return A tibble containing the estimated coefficients.
#' @export

tidy.ESN <- function(object) {
  
  tibble(
    term = rownames(object$model$method$model_weights$wout),
    estimate = as.numeric(object$model$method$model_weights$wout)
  )
  
}





#' @title Summary of model fit
#' 
#' @description Return summary statistics from a trained ESN as tibble.
#'  \itemize{
#'    \item{\code{df}: Effective degrees of freedom.}
#'    \item{\code{aic}: Akaike information criterion.}
#'    \item{\code{bic}: Bayesian information criterion.}
#'    \item{\code{hq}: Hannan-Quinn information criterion.}
#'       }
#'
#' @param object An object of class \code{ESN}.
#'
#' @return A tibble containing the summary statistics.
#' @export

glance.ESN <- function(object) {
  
  object$model$method$model_metrics
  
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
      values_to = ".spec")
  
  key_tbl <- object %>%
    as_tibble() %>%
    select(-c(.spec))
  
  # Extract states_train
  object <- map(
    .x = seq_len(nrow(key_tbl)),
    .f = ~{as_tibble(object[[".spec"]][[.x]]$fit$model$states_train)})
  
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
#'     \item{\code{n_diff}: Integer value. The number of non-seasonal differences.}
#'     \item{\code{const}: Logical value. If \code{TRUE}, a constant term (intercept) is used.}
#'     \item{\code{lags}: A \code{list} containing integer vectors with the lags associated with each input variable.}
#'     \item{\code{period}: A \code{list} containing the periodicity as integer vector.}
#'     \item{\code{alpha}: Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.}
#'     \item{\code{rho}: Numeric value. The spectral radius for scaling the reservoir weight matrix.}
#'     \item{\code{lambda}: Numeric value. The regularization (shrinkage) parameter for ridge regression.}
#'     \item{\code{density}: Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).}
#'     \item{\code{df}: Numeric value. The effective degree of freedom.}
#'     \item{\code{aic}: Numeric value. The Akaike information criterion.}
#'     \item{\code{bic}: Numeric value. The Bayesian information criterion.}
#'     \item{\code{hq}: Numeric value. The Hannan-Quinn information criterion.}
#'     }
#'
#' @param object An object of class \code{mdl_df}.
#'
#' @return A tibble containing the hyper-parameters.
#' @export

extract_esn <- function(object, ...) {
  UseMethod("extract_esn")
}


#' @export
extract_esn.mdl_df <- function(object, ...) {
  
  object <- object %>%
    pivot_longer(
      cols = mable_vars(object),
      names_to = ".model",
      values_to = ".spec")
  
  key_tbl <- object %>%
    as_tibble() %>%
    select(-c(.spec))
  
  # Extract model details
  mdl_tbl <- map(
    .x = seq_len(nrow(key_tbl)),
    .f = ~{
      
      lst_mdl <- object[[".spec"]][[.x]]$fit$model$method
      
      tibble(
        spec      = lst_mdl[["model_spec"]],
        n_inputs  = lst_mdl[["model_layers"]][["n_inputs"]],
        n_res     = lst_mdl[["model_layers"]][["n_res"]],
        n_outputs = lst_mdl[["model_layers"]][["n_outputs"]],
        n_diff    = lst_mdl[["diff_inputs"]][["n_diff"]],
        const     = lst_mdl[["model_inputs"]][["const"]],
        lags      = lst_mdl[["model_inputs"]][["lags"]],
        period    = list(lst_mdl[["model_inputs"]][["period"]]),
        alpha     = lst_mdl[["model_pars"]][["alpha"]],
        rho       = lst_mdl[["model_pars"]][["rho"]],
        lambda    = lst_mdl[["model_pars"]][["lambda"]],
        density   = lst_mdl[["model_pars"]][["density"]],
        df        = lst_mdl[["model_metrics"]][["df"]],
        aic       = lst_mdl[["model_metrics"]][["aic"]],
        bic       = lst_mdl[["model_metrics"]][["bic"]],
        hq        = lst_mdl[["model_metrics"]][["hq"]]
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

