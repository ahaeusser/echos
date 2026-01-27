
#' @title Checks if object is of class "esn"
#'
#' @description Returns \code{TRUE} if the object is of class \code{esn}.
#'
#' @param object object to be tested.
#' 
#' @return Logical value. If \code{TRUE}, the object is of class \code{esn}.
#' 
#' @family base functions
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
#' @description Returns \code{TRUE} if the object is of class \code{forecast_esn}.
#'
#' @param object object to be tested.
#' 
#' @return Logical value. If \code{TRUE}, the object is of class \code{forecast_esn}.
#' 
#' @family base functions
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


#' @title Checks if object is of class "tune_esn"
#'
#' @description Returns \code{TRUE} if the object is of class \code{tune_esn}.
#'
#' @param object object to be tested.
#' 
#' @return Logical value. If \code{TRUE}, the object is of class \code{tune_esn}.
#' 
#' @family base functions
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' fit <- tune_esn(
#'   y = xdata,
#'   n_ahead = 12,
#'   n_split = 5,
#'   alpha = c(0.5, 1),
#'   rho   = c(1.0),
#'   tau   = c(0.4),
#'   inf_crit = "bic"
#' )
#' is.tune_esn(fit)
#' 
#' @export

is.tune_esn <- function(object) {
  inherits(object, "tune_esn")
}


#' @title Print model specification of the trained ESN model
#' 
#' @description Provides a compact overview of the model specification in the 
#'    format \code{ESN({n_states, alpha, rho}, {n_models, df})}.
#'
#' @param x An object of class \code{esn}. The result of a call to \code{train_esn()}.
#' @param ... Currently not in use.
#'
#' @return Print specification of the trained ESN model.
#' 
#' @family base functions
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
#' @param object An object of class \code{esn}. The result of a call to \code{train_esn()}.
#' @param ... Currently not in use.
#'
#' @return Print detailed model summary.
#' 
#' @family base functions
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



#' @title Provide a summary of the hyperparameter tuning
#' 
#' @description Provide a summary of the tuned hyperparameters \code{alpha}, 
#'   \code{rho} and \code{tau}.
#'
#' @param object An object of class \code{tune_esn}. The result of a call to \code{tune_esn()}.
#' @param metric Character value. The metric used to select the best hyperparameter combination (\code{metric = c("mse", "mae")}.
#' @param ... Currently not in use.
#'
#' @return Print detailed model summary.
#' 
#' @family base functions
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' fit <- tune_esn(
#'   y = xdata,
#'   n_ahead = 12,
#'   n_split = 5,
#'   alpha = c(0.5, 1),
#'   rho   = c(1.0),
#'   tau   = c(0.4),
#'   inf_crit = "bic"
#' )
#' 
#' summary(fit)
#' 
#' @export

summary.tune_esn <- function(object, 
                             metric = "mse", 
                             ...) {
  
  if (!is.tune_esn(object))
    stop("x must be an object of class tune_esn")
  
  pars <- object[["pars"]]
  
  # Calculate mean
  xpars <- aggregate(
    x   = pars[[metric]],
    by  = list(id = pars$id),
    FUN = mean,
    na.rm = TRUE
  )
  
  names(xpars)[2] <- "mean"
  best_id <- which.min(xpars$mean)
  pars <- pars[pars$id == best_id, ]
  return(pars)
}



#' @title Plot internal states of a trained ESN model
#' 
#' @description Plot internal states (i.e., the reservoir) of a trained ESN 
#'   model as line chart.
#'
#' @param x An object of class \code{esn}. The result of a call to \code{train_esn()}.
#' @param ... Further arguments passed to \code{matplot()}.
#'
#' @return Line chart of internal states.
#' 
#' @family base functions
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' xmodel <- train_esn(y = xdata)
#' plot(xmodel)
#' 
#' @export

plot.esn <- function(x,
                     ...) {
  
  if (!is.esn(x))
    stop("x must be an object of class esn")
  
  # Extract reservoir and model specification from object
  states_train <- x[["states_train"]]
  model_spec <- x[["method"]][["model_spec"]]
  
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


#' @title Plot forecasts of a trained ESN model
#' 
#' @description Plot point forecasts and forecast intervals, actual values of a 
#'   trained ESN model. Optionally, test data (out-of-sample) and fitted values
#'   can be added to the plot.
#'
#' @param x An object of class \code{forecast_esn}. The result of a call to \code{forecast_esn()}.
#' @param test Numeric vector. Test data, i.e., out-of-sample actual values.
#' @param fitted Logical value. If \code{TRUE}, fitted values are added.
#' @param interval Logical value. If \code{TRUE}, forecast intervals are added.
#' @param n_obs Integer value. If \code{NULL}, all in-sample values are shown, otherwise only the last \code{n_obs}.
#' @param ... Further arguments passed to \code{plot()}.
#'
#' @return Line chart of point forecast and actual values.
#' 
#' @family base functions
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
                              interval = TRUE,
                              n_obs = NULL,
                              ...) {
  
  # Extract data --------------------------------------------------------------
  
  if (!is.forecast_esn(x))
    stop("x must be an object of class forecast_esn")
  
  model_spec <- x[["model_spec"]]   # Model specification
  n_ahead <- x[["n_ahead"]]         # Forecast horizon
  n_train  <- length(x[["actual"]]) # Number of in-sample data
  
  # Full index for in-sample (n_train) and out-of-sample (n_ahead)
  index <- seq(
    from = 1, 
    to = (n_train + n_ahead),
    by = 1
  )
  
  # Filter index and overwrite n_train
  if (!is.null(n_obs)) {
    index <- tail(index, n = (n_obs + n_ahead))
    n_train <- n_obs 
  }
  
  # Prepare actual values -----------------------------------------------------
  # Extract data and filter
  xactual <- tail(x[["actual"]], n = n_train)
  # Pad vector with trailing NAs to same length
  xactual <- c(xactual, rep(NA_real_, n_ahead))
  
  # Prepare fitted values -----------------------------------------------------
  if (fitted == TRUE) {
    # Extract data and filter
    xfitted <- tail(x[["fitted"]], n = n_train)
    # Pad vector with trailing NAs to same length
    xfitted <- c(xfitted, rep(NA_real_, n_ahead))
  } else {
    xfitted <- NULL
  }
  
  # Prepare point forecasts ---------------------------------------------------
  # Extract data
  xpoint <- x[["point"]]
  # Pad vector with leading NAs to same length
  xpoint  <- c(rep(NA_real_, n_train), xpoint)
  
  # Prepare forecast intervals ------------------------------------------------
  if (interval == TRUE) {
    if (!is.null(x[["interval"]])) {
      # Extract data
      xinterval <- x[["interval"]]
      # Pad matrix with leading NAs to same length
      xinterval <- rbind(
        matrix(
          data = NA_real_,
          nrow = n_train,
          ncol = ncol(xinterval)),
        xinterval)
    } else {
      xinterval <- NULL
    }
  }
  
  # Prepare test data ---------------------------------------------------------
  if (!is.null(test)) {
    if (n_ahead == length(test)) {
      xtest <- c(rep(NA_real_, n_train), test)
    }
  } else {
    xtest <- NULL
  }
  
  # Establish limits of y-axis
  ymin <- min(xactual, xpoint, xtest, xfitted, xinterval, na.rm = TRUE)
  ymax <- max(xactual, xpoint, xtest, xfitted, xinterval, na.rm = TRUE)
  
  # Create base plot ----------------------------------------------------------
  plot(
    x = index,
    y = xactual,
    type = "l",
    main = model_spec,
    ylim = c(ymin, ymax),
    xlab = "Index",
    ylab = "Value",
    ...
  )
  
  # Add test data
  if (!is.null(test)) {
    # Add vertical dashed line for split into training and testing
    abline(
      v = index[n_train],
      lty = 2,
      lwd = 1
    )
    
    # Add line for test data
    lines(
      x = index,
      y = xtest, 
      col = "black",
      lwd = 1
    )
  }
  
  # Add fitted values
  if (fitted == TRUE) {
    # Add line for fitted values
    lines(
      x = index,
      y = xfitted, 
      col = "steelblue",
      lwd = 1
    )
  }
  
  # Add polygon for interval forecasts (if required/available)
  if (!is.null(xinterval)) {
    
    # Extract levels and sort from large to small
    levels <- x[["levels"]]
    levels <- sort(levels, decreasing = TRUE)
    # Number of levels
    n_levels <- length(levels)
    
    base_cols <- colorRampPalette(c("#d4e3ff", "#7ea8ff"))(n_levels)
    alphas <- seq(0.20, 0.45, length.out = n_levels)
    
    for (i in 1:n_levels) {
      lower <- xinterval[, paste0("lower(", sprintf("%02d", levels[i]), ")")]
      upper <- xinterval[, paste0("upper(", sprintf("%02d", levels[i]), ")")]
      
      shade_col <- adjustcolor(
        base_cols[i], 
        alpha.f = alphas[i]
      )
      
      polygon(
        x = c(index, rev(index)),
        y = c(lower, rev(upper)),
        col = shade_col,
        border = NA
      )
    }
  }
  
  # Add line for point forecasts
  lines(
    x = index,
    y = xpoint, 
    col = "steelblue",
    lwd = 2
  )
}




#' @title Plot forecasts from a tuned ESN object
#' 
#' @description Plot actual values and the point forecasts from the best 
#'   hyperparameter combination selected via \code{tune.esn()} using the 
#'   selected accuracy metric. Forecasts are shown as separate line segments 
#'   for each test split, with vertical dashed lines marking the starts of test 
#'   windows.
#'
#' @param x An object of class \code{tune_esn}. The result of a call to \code{tune_esn()}.
#' @param metric Character value. The metric used to select the best hyperparameter combination (\code{metric = c("mse", "mae")}.
#' @param ... Further arguments passed to \code{plot()}.
#'
#' @return Line chart of point forecast and actual values.
#' 
#' @family base functions
#' 
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' fit <- tune_esn(
#'   y = xdata,
#'   n_ahead = 12,
#'   n_split = 5,
#'   alpha = c(0.5, 1),
#'   rho   = c(1.0),
#'   tau   = c(0.4),
#'   inf_crit = "bic"
#' )
#' 
#' plot(fit)
#' 
#' @export

plot.tune_esn <- function(x,
                          metric = "mse",
                          ...) {
  
  if (!is.tune_esn(x))
    stop("x must be an object of class tune_esn")
  
  # Pre-processing ============================================================
  
  # Extract objects
  pars <- x[["pars"]]
  fcst <- x[["fcst"]]
  actual <- x[["actual"]]
  
  # Select optimal hyperparameter combination
  best_pars <- summary(x, metric = metric)
  best_id <- unique(best_pars[["id"]])
  idx <- which(pars$id == best_id)
  
  # Number of observations
  nobs <- length(actual)
  
  # Establish limits of y-axis
  ymin <- min(actual, as.numeric(fcst[idx, , drop = FALSE]), na.rm = TRUE)
  ymax <- max(actual, as.numeric(fcst[idx, , drop = FALSE]), na.rm = TRUE)
  
  # Create base plot ============================================================
  
  plot(
    x = seq_len(nobs),
    y = actual,
    type = "l",
    main = "Time Series Cross-Validation",
    ylim = c(ymin, ymax),
    xlab = "Index",
    ylab = "Value",
    ...
  )
  
  # Add line for point forecasts (each split as its own segment)
  for (j in seq_along(idx)) {
    i <- idx[j]
    ts <- best_pars[["test_start"]][j]:best_pars[["test_end"]][j]
    h <- length(ts)
    lines(
      x = ts, 
      y = fcst[i, seq_len(h)], 
      col = "steelblue", 
      lwd = 2
    )
  }
  
  # Vertical lines at test window starts
  abline(
    v = unique(best_pars[["test_start"]]),
    col = "black", 
    lty = "dashed"
  )
}
