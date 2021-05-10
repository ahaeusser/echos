
#' @title Estimate information criterion for hyperparameter tuning
#' 
#' @description The function \code{tune_pars()} estimates the information criterion for hyperparameter
#'   tuning. The function returns a numeric value (= information criterion), which
#'   is then minimized within a call to \code{optim()} for varying hyperparameters.
#'   
#' @param pars Numeric vector containing the hyperparameters.
#' @param inf_crit Character value. The information criterion used for tuning \code{inf_crit = c("aic", "bic", "hq")}.
#' @inheritParams train_esn
#' 
#' @return model_value Numeric value. The information criterion to be minimized.
#' @export

tune_pars <- function(data,
                      pars,
                      inf_crit,
                      lags,
                      fourier,
                      const,
                      xreg,
                      dy,
                      dx,
                      n_res,
                      n_initial,
                      n_seed,
                      density,
                      type,
                      weights,
                      penalty,
                      # scale_win,
                      scale_wres,
                      # scale_runif,
                      scale_inputs) {
  
  # Train model
  model_fit <- train_esn(
    data = data,
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
    penalty = penalty,
    scale_win = pars[4],
    scale_wres = scale_wres,
    # scale_runif = scale_runif,
    scale_inputs = scale_inputs
  )
  
  # Extract and return information criterion
  model_value <- model_fit$method$model_metric[[inf_crit]]
  return(model_value)
  
}
