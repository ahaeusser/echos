
#' @title Automatic train an Echo State Network
#' 
#' @description This function trains an Echo State Network (ESN)
#'   to a univariate time series.
#'
#' @param .data A \code{tsibble} containing the time series data.
#' @param specials Currently not is use.
#' @param const Logical value. If \code{TRUE}, a constant term (intercept) is used.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param fourier A \code{list} containing the periods and the number of fourier terms as integer vector.
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param dy Integer vector. The nth-differences of the response variable.
#' @param dx Integer vector. The nth-differences of the exogenous variables.
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param n_res Integer value. The number of internal states within the reservoir (hidden layer).
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#' @param alpha Numeric value. The leakage rate (smoothing parameter) applied to the reservoir.
#' @param rho Numeric value. The spectral radius for scaling the reservoir weight matrix.
#' @param lambda Numeric value. The regularization (shrinkage) parameter for ridge regression.
#' @param density Numeric value. The connectivity of the reservoir weight matrix (dense or sparse).
#' @param type Numeric value. The elastic net mixing parameter.
#' @param weights Numeric vector. Observation weights for weighted least squares estimation.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param control_tuning A \code{list} containing control values for the automatic tuning of model inputs and hyperparameters:
#'  \itemize{
#'    \item{\code{inf_crit}: Character value. The information criterion used for tuning \code{inf_crit = c("aic", "bic", "hq")}.}
#'    \item{\code{inputs_tune}: Logical value. If \code{TRUE}, the model inputs are tuned, otherwise model inputs are used as defined.}
#'    \item{\code{n_sample}: Integer value. The number of samples for the random grid.}
#'    \item{\code{pars_tune}: Logical value. If \code{TRUE}, the hyperparameters are tuned, otherwise hyperparameters are used as defined.}
#'    \item{\code{lower}: Numeric vector. The lower bounds for \code{alpha}, \code{rho} and \code{lambda} used in the optimization.}
#'    \item{\code{upper}: Numeric vector. The upper bounds for \code{alpha}, \code{rho} and \code{lambda} used in the optimization.}
#'  }
#' @param ... Further arguments passed to \code{stats::optim()}
#'
#' @return An object of class \code{ESN}.
#' @export

auto_esn <- function(.data,
                     specials,
                     const = FALSE,
                     lags = NULL,
                     fourier = NULL,
                     xreg = NULL,
                     dy = 0,
                     dx = 0,
                     n_initial = 100,
                     n_res = 200,
                     n_seed = 42,
                     alpha = 0.8,
                     rho = 1,
                     lambda = 1e-4,
                     density = 0.1,
                     type = 1,
                     weights = NULL,
                     scale_inputs = c(-1, 1),
                     scale_runif = c(-0.5, 0.5),
                     control_tuning = list(
                       inf_crit = "aic",
                       inputs_tune = FALSE,
                       n_sample = 1000,
                       pars_tune = TRUE,
                       lower = c(1e-4, 1e-2, 1e-8),
                       upper = c(0.9999, 2.5, 1e-3)),
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
  if (is.null(dy)) {
    dy <- unitroot_ndiffs(
      x = .data[[measured_vars(.data)]],
      alpha = 0.05,
      unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],
      differences = 0:1)
  }
  
  
  # Select model inputs (lags, fourier terms) =================================
  
  if (is.null(lags)) {
    lags <- list(c(1, period))
  }
  
  if (control_tuning$inputs_tune == TRUE) {
    model_inputs <- tune_inputs(
      data = .data,
      lags = lags,
      fourier = fourier,
      xreg = xreg,
      dy = dy,
      dx = dx,
      n_initial = n_initial,
      weights = weights,
      scale_inputs = scale_inputs,
      inf_crit = control_tuning$inf_crit,
      n_sample = control_tuning$n_sample,
      n_seed = n_seed)
    
    const <- model_inputs$const
    lags <- model_inputs$lags
    fourier <- model_inputs$fourier
  }
  
  
  # Hyperparameter optimization ===============================================
  
  # Starting values
  pars <- c(alpha, rho, lambda)
  
  # Tune hyperparameters via L-BFGS-B
  if (control_tuning$pars_tune == TRUE) {
    
    model_pars <- optim(
      par = pars,
      fn = tune_pars,
      lower = control_tuning$lower,
      upper = control_tuning$upper,
      method = "L-BFGS-B",
      data = .data,
      inf_crit = control_tuning$inf_crit,
      lags = lags,
      fourier = fourier,
      xreg = xreg,
      const = const,
      dy = dy,
      dx = dx,
      n_res = n_res,
      n_initial = n_initial,
      n_seed = n_seed,
      density = density,
      type = type,
      weights = weights,
      scale_runif = scale_runif,
      scale_inputs = scale_inputs
    )
    
    # Vector with optimized hyperparameters
    pars <- c(
      model_pars$par[1],
      model_pars$par[2],
      model_pars$par[3]
    )
  }
  
  
  # Train final model =========================================================
  
  model_fit <- train_esn(
    data = .data,
    lags = lags,
    fourier = fourier,
    xreg = xreg,
    const = const,
    dy = dy,
    dx = dx,
    n_res = n_res,
    n_initial = n_initial,
    n_seed = n_seed,
    alpha = pars[1],
    rho = pars[2],
    lambda = pars[3],
    density = density,
    type = type,
    weights = weights,
    scale_runif = scale_runif,
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
                         n_sim = 100,
                         n_seed = 42,
                         xreg = NULL,
                         ...) {
  # Extract model
  model_fit <- object$model
  
  # Forecast model
  model_fcst <- forecast_esn(
    object = model_fit,
    n_ahead = nrow(new_data),
    n_sim = n_sim,
    n_seed = n_seed,
    xreg = xreg)
  
  # Extract point forecasts
  fcst_point <- model_fcst$point
  # Extract simulations
  fcst_std <- rowSds(model_fcst$sim, na.rm = TRUE)
  # Return forecast
  dist_normal(fcst_point, fcst_std)
  
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
  
  const <- method$model_inputs$const
  lags <- unlist(method$model_inputs$lags)
  
  if (is.null(method$model_inputs$fourier)) {
    period <- "none"
    k <- "none"
  } else {
    period <- unlist(method$model_inputs$fourier[[1]])
    k <- unlist(method$model_inputs$fourier[[2]])
  }
  
  if (is.null(method$model_data$xx)) {
    xreg <- "none"
    dx <- "none"
  } else {
    xreg <- colnames(method$model_data$xx)
    dx <- as.numeric(method$model_inputs$dx)
  }
  
  dy <- as.numeric(method$model_inputs$dy)
  
  alpha <- round(method$model_pars$alpha, 2)
  rho <- round(method$model_pars$rho, 2)
  lambda <- round(method$model_pars$lambda, 2)
  density <- round(method$model_pars$density, 2)
  
  dof <- round(method$model_metrics$dof, 2)
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
    "const    = ", const, "\n",
    "lags     = ", lags, "\n",
    "period   = ", period, "\n",
    "k        = ", k, "\n",
    "xreg     = ", xreg, "\n"
  )
  
  cat(
    "\nDifferences:", "\n",
    "dy = ", dy, "\n",
    "dx = ", dx, "\n"
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
    "dof  =", dof, "\n",
    "aic  =", aic, "\n",
    "bic  =", bic, "\n",
    "hq   =", hq, "\n"
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
      values_to = ".spec")
  
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

extract_esn <- function(object) {
  UseMethod("extract_esn")
}


#' @export
extract_esn.mdl_df <- function(object) {
  
  object <- object %>%
    pivot_longer(
      cols = mable_vars(object),
      names_to = ".model",
      values_to = ".spec")
  
  key_tbl <- object %>%
    as_tibble() %>%
    select(-c(.data$.spec))
  
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
        const     = lst_mdl[["model_inputs"]][["const"]],
        lags      = list(lst_mdl[["model_inputs"]][["lags"]]),
        fourier   = list(lst_mdl[["model_inputs"]][["fourier"]]),
        dy        = lst_mdl[["model_inputs"]][["dy"]],
        dx        = lst_mdl[["model_inputs"]][["dx"]],
        alpha     = lst_mdl[["model_pars"]][["alpha"]],
        rho       = lst_mdl[["model_pars"]][["rho"]],
        lambda    = lst_mdl[["model_pars"]][["lambda"]],
        density   = lst_mdl[["model_pars"]][["density"]],
        dof       = lst_mdl[["model_metrics"]][["dof"]],
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

