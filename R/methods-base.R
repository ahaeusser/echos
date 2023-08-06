
#' @title Checks if object is of class "esn"
#'
#' @description Returns \code{TRUE} if the object is of class "esn".
#'
#' @param x object to be tested.
#' @export

is.esn <- function(object) {
  inherits(object, "esn")
}


#' @title Provide a detailed summary of the trained ESN model
#' 
#' @description Provide a detailed summary of the trained ESN model.
#'
#' @param object An object of class \code{esn}.
#'
#' @return Print detailed model summary.
#' @export

summary.esn <- function(object) {
  
  # Extract method from object
  method <- object[["method"]]
  
  # Layers (number of inputs, internal states and outputs)
  n_inputs <- method$model_layer$n_inputs
  n_states <- method$model_layer$n_states
  n_outputs <- method$model_layer$n_outputs
  
  # Meta data (lags, differences and number of models)
  lags <- method$model_meta$lags
  n_diff <- method$model_meta$n_diff
  n_models <- method$model_meta$n_models
  
  # Hyperparameters (leakage rate, spectral radius and density)
  alpha <- method$model_meta$alpha
  rho <- method$model_meta$rho
  density <- method$model_meta$density
  
  # Scaling
  scale_win <- method$scale_win
  scale_wres <- method$scale_wres
  scale_inputs <- method$scale_inputs
  
  cat(
    "\n--- Layers -----------------------------------------------------", "\n",
    "n_inputs  = ", n_inputs, "\n",
    "n_states  = ", n_states, "\n",
    "n_outputs = ", n_outputs, "\n"
  )
  
  cat(
    "\n--- Meta ---------------------------------------------------", "\n",
    "lags     = ", lags, "\n",
    "n_diff   = ", n_diff, "\n",
    "n_models = ", n_models, "\n"
  )
  
  cat(
    "\n--- Scaling ----------------------------------------------------", "\n",
    "scale_inputs = ", "[", scale_inputs[1], ", ", scale_inputs[2], "]", "\n",
    "scale_win    = ", "[", -scale_win, ", ", scale_win, "]", "\n",
    "scale_wres   = ", "[", -scale_wres, ", ", scale_wres, "]", "\n",
    sep = ""
  )
  
  cat(
    "\n--- Hyperparameters --------------------------------------------", "\n",
    "alpha   = ", alpha, "\n",
    "rho     = ", rho, "\n",
    "density = ", density, "\n"
  )
}


#' @title Plot actuals, fitted and residuals of a trained ESN model.
#' 
#' @description Provide standard plots of a trained ESN model.
#'
#' @param object An object of class \code{esn}.
#'
#' @return Standard plots.
#' @export

plot.esn <- function(object) {
  
  # Extract actual, fitted and residuals from model object
  actual <- object[["actual"]]
  fitted <- object[["fitted"]]
  resid <- object[["resid"]]
  model_spec <- object[["method"]][["model_spec"]]
  
  par(mfrow = c(2, 2))
  
  # Plot 1: Actual vs. Fitted -------------------------------------------------
  plot(
    x = actual, 
    col = "black", 
    type = "l", 
    main = "Actual and Fitted",
    xlab = "Index",
    ylab = "Value"
  )
  
  lines(
    fitted, 
    col = "steelblue",
    lwd = 2
  )
  
  # Add a legend
  legend(
    "topleft",
    legend = c("Actual", "Fitted"),
    col = c("black", "steelblue"),
    lty = 1,
    lwd = c(1, 2),
    cex = 0.8
  )
  
  # Plot 2: Residuals vs. Fitted ----------------------------------------------
  plot(
    x = fitted,
    y = resid,
    main = "Residuals vs. Fitted",
    xlab = "Fitted",
    ylab = "Residuals"
  )
  
  abline(
    h = 0,
    lty = 2
  )
  
  abline(
    lm(resid ~ fitted), 
    col = "steelblue",
    lwd = 2
  )
  
  # Add a legend
  legend(
    "topleft",
    legend = c("Baseline", "Linear Regression"),
    col = c("black", "steelblue"),
    lty = c(2, 1),
    lwd = c(1, 2),
    cex = 0.8
  )
  
  # Plot 3: Histogram of Residuals --------------------------------------------
  
  # x-axis grid
  x <- seq(
    from = min(resid, na.rm = TRUE),
    to = max(resid, na.rm = TRUE),
    length = 40
  )
  
  # Normal curve
  fun <- dnorm(
    x = x,
    mean = mean(resid, na.rm = TRUE),
    sd = sd(resid, na.rm = TRUE)
  )
  
  # Basic histogram
  hist(
    x = resid,
    prob = TRUE,
    col = "white",
    main = "Hisogram of Residuals",
    xlab = "Residuals",
    ylab = "Frequency",
    ylim = c(0, max(fun)*1.1))
  
  # Add normal distribution
  lines(
    x = x,
    y = fun,
    col = "black",
    lty = 2,
    lwd = 1
  )
  
  # Add density estimate
  lines(
    density(na.omit(resid)),
    col = "steelblue",
    lwd = 2
  )
  
  # Add a legend
  legend(
    "topleft",
    legend = c("Normal", "Density"),
    col = c("black", "steelblue"),
    lty = c(2, 1),
    lwd = c(1, 2),
    cex = 0.8
  )
  
  # Plot 4: Autocorrelation of Residuals --------------------------------------
  acf(
    x = na.omit(resid),
    plot = TRUE,
    main = "Autocorrelation of Residuals",
    ci.col = "steelblue"
  )
}
