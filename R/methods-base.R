
#' @title Checks if object is of class "esn"
#'
#' @description Returns \code{TRUE} if the object is of class "esn".
#'
#' @param object object to be tested.
#' @export

is.esn <- function(object) {
  inherits(object, "esn")
}


#' @title Checks if object is of class "forecast_esn"
#'
#' @description Returns \code{TRUE} if the object is of class "forecast_esn".
#'
#' @param object object to be tested.
#' @export

is.forecast_esn <- function(object) {
  inherits(object, "forecast_esn")
}


#' @title Provide a detailed summary of the trained ESN model
#' 
#' @description Provide a detailed summary of the trained ESN model.
#'
#' @param object An object of class \code{esn}.
#' @param ... Currently not in use.
#'
#' @return Print detailed model summary.
#' @export

summary.esn <- function(object, ...) {
  
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



#' @title Plot point forecasts and actuals of a trained ESN model.
#' 
#' @description Plot point forecasts and actuals of a trained ESN model as line
#'   chart. Optionally, test data (out-of-sample) can be added to the plot.
#'
#' @param x An object of class \code{forecast_esn}.
#' @param test Numeric vector. Test data, i.e., out-of-sample actual values.
#' @param ... Currently not in use.
#'
#' @return Line chart of point forecast and actual values.
#' @export

plot.forecast_esn <- function(x,
                              test = NULL,
                              ...) {
  
  # Extract actual, point forecast and model specification
  actual <- x[["actual"]]
  point <- x[["point"]]
  model_spec <- x[["model_spec"]]
  
  # Pad vectors with leading and trailing NAs to same length
  xactual <- c(actual, rep(NA_real_, length(point)))
  xpoint <- c(rep(NA_real_, length(actual)), point)
  
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

