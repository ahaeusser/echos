
#' @title Checks if object is of class "esn"
#'
#' @description Returns \code{TRUE} if the object is of class "esn".
#'
#' @param x object to be tested.
#' @export

is.esn <- function(object) {
  inherits(object, "esn")
}


#' @title Checks if object is of class "forecast_esn"
#'
#' @description Returns \code{TRUE} if the object is of class "forecast_esn".
#'
#' @param x object to be tested.
#' @export

is.forecast_esn <- function(object) {
  inherits(object, "forecast_esn")
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
  
  lower <- min(actual, fitted, na.rm = TRUE)
  upper <- max(actual, fitted, na.rm = TRUE)
  
  def_par = par(no.readonly = TRUE)
  
  par(mfrow = c(3, 2))
  
  # Plot 1: Actual and Fitted -------------------------------------------------
  plot(
    x = actual, 
    col = "black", 
    type = "l", 
    main = "Actual and Fitted",
    xlab = "Index",
    ylab = "Value",
    ylim = c(lower, upper)
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
  
  # Plot 2: Residuals ---------------------------------------------------------
  plot(
    x = resid, 
    col = "black", 
    type = "l", 
    main = "Residuals",
    xlab = "Index",
    ylab = "Value"
  )
  
  # Plot 3: Residuals vs. Fitted ----------------------------------------------
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
  
  # Plot 4: Histogram of Residuals --------------------------------------------
  
  # x-axis grid
  x <- seq(
    from = min(resid, na.rm = TRUE),
    to = max(resid, na.rm = TRUE),
    length = 40
  )
  
  # Normal curve
  resid_norm <- dnorm(
    x = x,
    mean = mean(resid, na.rm = TRUE),
    sd = sd(resid, na.rm = TRUE)
  )
  
  resid_density <- density(na.omit(resid))
  
  # Basic histogram
  hist(
    x = resid,
    prob = TRUE,
    col = "white",
    main = "Hisogram of Residuals",
    xlab = "Residuals",
    ylab = "Frequency",
    ylim = c(0, max(resid_density[["y"]]))
    )
  
  # Add normal distribution
  lines(
    x = x,
    y = resid_norm,
    col = "black",
    lty = 2,
    lwd = 1
  )
  
  # Add density estimate
  lines(
    resid_density,
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
  
  # Plot 5: Autocorrelation of Residuals --------------------------------------
  acf(
    x = na.omit(resid),
    plot = TRUE,
    main = "Sample Autocorrelation of Residuals",
    ci.col = "steelblue"
  )
  
  # Plot 6: Partial Autocorrelation of Residuals ------------------------------
  pacf(
    x = na.omit(resid),
    plot = TRUE,
    main = "Sample Partial Autocorrelation of Residuals",
    ci.col = "steelblue"
  )
  
  par(def_par)
}


#' @title Plot point forecasts and actuals of a trained ESN model.
#' 
#' @description Plot point forecasts and actuals of a trained ESN model as line
#'   chart. Optionally, fitted values and test data can be added to the plot.
#'
#' @param object An object of class \code{forecast_esn}.
#'
#' @return Line chart of point forecast and actual values.
#' @export

plot.forecast_esn <- function(object,
                              fitted = FALSE,
                              test = NULL) {
  
  # Extract actual, point forecast and model specification
  actual <- object[["actual"]]
  point <- object[["point"]]
  model_spec <- object[["model_spec"]]
  
  # Pad vectors with leading and trailing NAs to same length
  xactual <- c(actual, rep(NA_real_, length(point)))
  xpoint <- c(rep(NA_real_, length(actual)), point)
  

  if (fitted) {
    fitted <- object[["fitted"]]
    xfitted <- c(fitted, rep(NA_real_, length(point)))
  } else {
    xfitted <- NULL
  }
  
  if (!is.null(test)) {
    if (length(point) == length(test)) {
      xtest <- c(rep(NA_real_, length(actual)), test)
    } else {
      xtest <- NULL
    }
  }
  
  lower <- min(xactual, xpoint, xtest, na.rm = TRUE)
  upper <- max(xactual, xpoint, xtest, na.rm = TRUE)
  
  def_par = par(no.readonly = TRUE)
  
  plot(
    x = xactual,
    type = "l",
    main = paste0("Forecast from ", model_spec),
    ylim = c(lower, upper),
    xlab = "Index",
    ylab = "Value"
  )
  
  lines(
    x = xfitted,
    col = "steelblue",
    lwd = 1
    )
  
  lines(
    x = xtest, 
    col = "black",
    lwd = 1
  )
  
  lines(
    x = xpoint, 
    col = "steelblue",
    lwd = 2
    )
  
  par(def_par)
}

