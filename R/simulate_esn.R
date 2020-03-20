
#' Simulate a fitted ESN
#' 
#' Simulate future sample path from a fitted ESN.
#' 
#' @param win Weights matrix for input variables
#' @param wres Weights matrix for reservoir
#' @param wout Weights matrix for output variables (coefficients from ridge regression)
#' @param n_ahead Number of periods for forecasting (forecast horizon)
#' @param alpha Leakage rate (smoothing parameter)
#' @param lags A numeric matrix with lagged variables
#' @param inputs Initialized input features (gets updated during forecasting process)
#' @param states_train Internal states from training (necessary for last values)
#' @param error Residuals from fitted model
#' @param n_sim Number of simulations
#' 
#' @return sim An array with simulated future sample paths

simulate_esn <- function(win,
                         wres,
                         wout,
                         n_ahead,
                         alpha,
                         lags,
                         inputs,
                         states_train,
                         error,
                         n_sim) {
  
  # Number of response variables
  n_outputs <- ncol(wout)
  
  # Create list of matrices sampeled from residuals
  innov <- lapply(seq_len(n_sim), function(n_sim) {
    sapply(seq_len(n_outputs), function(n_outputs) {
      sample(error[, n_outputs], size = n_ahead, replace = TRUE)
    })
  })
  
  # Simulate future sample path with normal distributed innovations
  sim <- lapply(innov, function(innov) {
    predict_esn(
      win = win,
      wres = wres,
      wout = wout,
      n_ahead = n_ahead,
      alpha = alpha,
      lags = lags,
      inputs = inputs,
      states_train = states_train,
      innov = innov)$fcst
  })
  return(sim)
}