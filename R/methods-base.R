
#' @title Checks if object is of class "esn"
#'
#' @description Returns \code{TRUE} if the object is of class "esn".
#'
#' @param object object to be tested.
#' 
#' @return Logical value. If \code{TRUE}, the object is of class "esn".
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' is.esn(xmodel)
#' 
#' @export

is.esn <- function(object) {
  inherits(object, "esn")
}



#' @title Checks if object is of class "forecast_esn"
#'
#' @description Returns \code{TRUE} if the object is of class "forecast_esn".
#'
#' @param object object to be tested.
#' 
#' @return Logical value. If \code{TRUE}, the object is of class "forecast_esn".
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' xfcst <- forecast_esn(xmodel, n_ahead = 12)
#' is.forecast_esn(xfcst)
#' 
#' @export

is.forecast_esn <- function(object) {
  inherits(object, "forecast_esn")
}



#' @title Print model specification of the trained ESN model
#' 
#' @description Print model specification of the trained ESN model.
#'
#' @param x An object of class \code{esn}.
#' @param ... Currently not in use.
#'
#' @return Print specification of the trained ESN model.
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' print(xmodel)
#' 
#' @export

print.esn <- function(x, ...) {
  # Print model specification
  cat(x$method$model_spec)
}


#' @title Provide a detailed summary of the trained ESN model
#' 
#' @description Provide a detailed summary of the trained ESN model.
#'
#' @param object An object of class \code{esn}.
#' @param ... Currently not in use.
#'
#' @return Print detailed model summary.
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' summary(xmodel)
#' 
#' @export

summary.esn <- function(object, ...) {
  
  # Extract method from object
  method <- object[["method"]]
  
  # Inputs (number of observations, number of differences, lags)
  n_obs <- length(method$model_data$yy)
  n_diff <- method$model_meta$n_diff
  lags <- method$model_meta$lags
  
  # Number of internal states
  n_states <- method$model_layer$n_states
  
  # Hyperparameters (leakage rate, spectral radius and density)
  alpha <- method$model_meta$alpha
  rho <- method$model_meta$rho
  density <- method$model_meta$density
  
  # Scaling
  scale_win <- method$scale_win
  scale_wres <- method$scale_wres
  scale_inputs <- method$scale_inputs
  
  # Model selection
  n_models <- method$model_meta$n_models
  df <- round(method$model_meta$df, 2)
  lambda <- round(method$model_meta$lambda, 4)
  
  # Print output
  
  cat(
    "\n--- Inputs -----------------------------------------------------", "\n",
    "n_obs        = ", n_obs, "\n",
    "n_diff       = ", n_diff, "\n",
    "lags         = ", lags, "\n",
    sep = ""
  )
  
  cat(
    "\n--- Reservoir generation ---------------------------------------", "\n",
    "n_states     = ", n_states, "\n",
    "alpha        = ", alpha, "\n",
    "rho          = ", rho, "\n",
    "density      = ", density, "\n",
    "scale_inputs = ", "[", scale_inputs[1], ", ", scale_inputs[2], "]", "\n",
    "scale_win    = ", "[", -scale_win, ", ", scale_win, "]", "\n",
    "scale_wres   = ", "[", -scale_wres, ", ", scale_wres, "]", "\n",
    sep = ""
  )
  
  cat(
    "\n--- Model selection --------------------------------------------", "\n",
    "n_models     = ", n_models, "\n",
    "df           = ", df, "\n",
    "lambda       = ", lambda, "\n",
    sep = ""
  )
}


#' @title Plot internal states of a trained ESN model.
#' 
#' @description Plot internal states (i.e., the reservoir) of a trained ESN 
#'   model as line chart.
#'
#' @param x An object of class \code{esn}.
#' @param ... Currently not in use.
#'
#' @return Line chart of internal states.
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' plot(xmodel)
#' 
#' @export

plot.esn <- function(x,
                     ...) {
  
  # Extract reservoir and model specification from object
  states_train <- x[["states_train"]]
  model_spec <- x[["method"]][["model_spec"]]
  
  # # Save the current par settings, then immediately ensure they get restored
  # old_par <- par(no.readonly = TRUE)
  # on.exit(par(old_par))
  
  # Plot internal states as line chart
  matplot(
    states_train, 
    type = "l",
    col = "lightgrey",
    lty = 1,
    main = model_spec,
    xlab = "Index",
    ylab = "Value",
    ...
  )
}


#' @title Plot point forecasts and actual values of a trained ESN model.
#' 
#' @description Plot point forecasts, actual and fitted values of a trained ESN 
#'   model as line chart. Optionally, test data (out-of-sample) can be added to 
#'   the plot.
#'
#' @param x An object of class \code{forecast_esn}.
#' @param test Numeric vector. Test data, i.e., out-of-sample actual values.
#' @param fitted Logical value. If \code{TRUE}, fitted values are added.
#' @param ... Currently not in use.
#'
#' @return Line chart of point forecast and actual values.
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' xfcst <- forecast_esn(xmodel, n_ahead = 12)
#' plot(xfcst)
#' 
#' @export

plot.forecast_esn <- function(x,
                              test = NULL,
                              fitted = TRUE,
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
    }
  } else {
    xtest <- NULL
  }
  
  # Extract fitted and pad vectors with leading and trailing NAs to same length
  if (fitted == TRUE) {
    xfitted <- x[["fitted"]]
    xfitted <- c(xfitted, rep(NA_real_, length(point)))
  } else {
    xfitted <- NULL
  }
  
  lower <- min(xactual, xpoint, xtest, na.rm = TRUE)
  upper <- max(xactual, xpoint, xtest, na.rm = TRUE)
  
  # # Save the current par settings, then immediately ensure they get restored
  # old_par <- par(no.readonly = TRUE)
  # on.exit(par(old_par))
  
  plot(
    x = xactual,
    type = "l",
    main = model_spec,
    ylim = c(lower, upper),
    xlab = "Index",
    ylab = "Value"
  )
  
  # Add vertical dashed line for split into training and testing
  if (!is.null(test)) {
    abline(
      v = length(actual),
      lty = 2,
      lwd = 1
    )
  }
  
  # Add line for test data (if required)
  lines(
    x = xtest, 
    col = "black",
    lwd = 1
  )
  
  # Add line for fitted values (if required)
  lines(
    x = xfitted, 
    col = "steelblue",
    lwd = 1
  )
  
  # Add line for point forecasts
  lines(
    x = xpoint, 
    col = "steelblue",
    lwd = 2
    )
}
