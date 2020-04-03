
#' @title Create constant (intercept term).
#' 
#' @description This functions creates the constant (intercept term) for the design matrix as numeric matrix.
#'
#' @param n_obs Integer value. The number of observations.
#'
#' @return y_const Numeric matrix with dimension (n_obs x 1).

create_const <- function(n_obs) {
  y_const <- matrix(
    data = 1,
    nrow = n_obs,
    ncol = 1,
    dimnames = list(c(), "const"))
  
  return(y_const)
}



#' @title Create lagged variables of a matrix.
#' 
#' @description Create lagged variables of a matrix, shifting each column back by a given number of observations.
#' 
#' @param data Numeric vector or matrix. Each column is a variable and each row an observation.
#' @param lags List containing vectors with the number of lags (in units of observations) per variable.
#' 
#' @return y_lag Numeric matrix with lagged variables of the input data.

create_lags <- function(data, lags) {
  # Number of input variables
  n_inputs <- ncol(data)
  # Number of observations
  n_obs <- nrow(data)
  # Number of lags in total
  n_lags <- sum(lengths(lags))
  # Number of lags by input variable
  n_lags_inputs <- lengths(lags)
  # Names of output variables
  names_outputs <- colnames(data)
  
  # Preallocate empty matrix for lagged variables
  y_lag <- matrix(
    data = 0,
    nrow = n_obs,
    ncol = n_lags)
  
  # Create matrix with combinations of input variables and lags
  index <- matrix(
    data = c(rep(seq(1, n_inputs),
                 times = n_lags_inputs),
             unlist(lags)),
    ncol = 2)
  
  # Lag variables
  for (i in seq_len(nrow(index))) {
    x <- data[, index[i, 1]]
    k <- index[i, 2]
    y_lag[, i] <- c(rep(NA_real_, k), x)[1:length(x)]
  }
  
  # Names of lagged variables (combination of names_output and lags)
  names_lags <- paste0(
    rep(names_outputs,
        times = n_lags_inputs),
    "(",
    unlist(lags),
    ")")
  
  colnames(y_lag) <- names_lags
  return(y_lag)
}



#' @title Create lagged variables of a matrix for iterative forecasting.
#' 
#' @description Create lagged variables of a matrix for iterative forecasting, shifting each column back by a given number of observations and fill with NAs for the updates.
#' 
#' @param data Numeric vector or matrix. Each column is a variable and each row an observation.
#' @param lags List containing vectors with the number of lags (in units of observations) per variable.
#' @param n_ahead Integer value. The forecast horizon (n-step ahead).
#' 
#' @return y_lag Numeric matrix with the lagged variables of the input data for iterative forecasting.

create_revolved <- function(data, lags, n_ahead) {
  # Number of input variables
  n_inputs <- ncol(data)
  # Maximum number of lags (overall)
  max_lag <- max(unlist(lags))
  # Number of rows
  n_rows <- (max_lag + n_ahead + 1)
  # Number of lags in total
  n_lags <- sum(lengths(lags))
  # Number of lags by input variable
  n_lags_inputs <- lengths(lags)
  # Names of output variables
  names_outputs <- colnames(data)
  
  # Preallocate empty output matrix for lagged variables
  y_lag <- matrix(
    data = NA_real_,
    nrow = n_rows,
    ncol = n_lags)
  
  # Create matrix with combinations of input variables and lags
  index <- matrix(
    data = c(rep(seq(1, n_inputs),
                 times = n_lags_inputs),
             unlist(lags)),
    ncol = 2)
  
  for (i in seq_len(nrow(index))) {
    x <- data[, index[i, 1]]
    k <- index[i, 2]
    lag <- c(0, x[c((length(x) - k + 1):length(x))])
    length(lag) <- n_rows
    y_lag[, i] <- lag
  }
  
  # Names of lagged variables (combination of names_output and lags)
  names_lags <- paste0(
    rep(names_outputs,
        times = n_lags_inputs),
    "(",
    unlist(lags),
    ")")
  
  colnames(y_lag) <- names_lags
  return(y_lag)
}




#' @title Create trigonometric terms.
#' 
#' @description This function creates the trigonometric terms for the design matrix as numeric matrix.
#'
#' @param times Integer value. The number of observations.
#' @param k Integer vector. The number of seasonal cycles per period.
#' @param period Integer vector. The periodicity of the time series.
#' 
#' @return

create_season <- function(times, k, period) {
  
  # Patch for older versions of R that do not have sinpi and cospi functions.
  if (!exists("sinpi")) {
    sinpi <- function(x) {
      sin(pi * x)
    }
    cospi <- function(x) {
      cos(pi * x)
    }
  }
  
  if (length(period) != length(k)) {
    stop("Number of periods does not match number of orders")
  }
  if (any(2 * k > period)) {
    stop("k must be not be greater than period/2")
  }
  
  # Compute periods of all Fourier terms
  p <- numeric(0)
  labels <- character(0)
  for (j in seq_along(period))
  {
    if (k[j] > 0) {
      p <- c(p, (1:k[j]) / period[j])
      labels <- c(labels, paste0(
        paste0(c("sin(", "cos("), rep(1:k[j], rep(2, k[j]))),
        "-", round(period[j]), ")"))
    }
  }
  # Remove equivalent seasonal periods due to multiple seasonality
  k <- duplicated(p)
  p <- p[!k]
  labels <- labels[!rep(k, rep(2, length(k)))]
  
  # Remove columns where sinpi = 0
  k <- abs(2 * p - round(2 * p)) > .Machine$double.eps
  
  # Compute matrix of Fourier terms
  X <- matrix(NA_real_, nrow = length(times), ncol = 2L * length(p))
  for (j in seq_along(p))
  {
    if (k[j]) {
      X[, 2L * j - 1L] <- sinpi(2 * p[j] * times)
    }
    X[, 2L * j] <- cospi(2 * p[j] * times)
  }
  colnames(X) <- labels
  
  # Remove missing columns
  X <- X[, !is.na(colSums(X)), drop = FALSE]
  
  return(X)
}


#' @title Create model specification.
#' 
#' @description This function creates the model specification (short summary) as a string.
#'
#' @param n_layers List containing the number of inputs (n_inputs), reservoir size (n_res) and the number of outputs (n_outputs).
#' @param pars List containing the hyperparameters alpha, rho, lambda and density.
#' @param season Integer vector. The number of seasonal cycles per period.
#' @param period Integer vector. The periodicity of the time series.
#'
#' @return model_spec Character value. The model specification as string.

create_spec <- function(n_layers,
                        pars,
                        season,
                        period) {
  
  # Number of inputs and outputs and reservoir size
  str_layer <- paste0(
    "{",
    n_layers$n_inputs, ",",
    n_layers$n_res, ",",
    n_layers$n_outputs,
    "}")
  
  # Hyperparameters
  str_pars <- paste0(
    "{",
    round(pars$alpha, 2), ",",
    round(pars$rho, 2), ",",
    round(pars$lambda, 2),
    "}")
  
  # Seasonality and periodicity
  if (is.null(season)) {
    season <- 0}
  
  str_season <- paste(
    "{",
    paste("(", period, "-", season, ")",
          collapse = ",",
          sep = ""), "}",
    sep = "")
  
  # Model specification
  model_spec <- paste0(
    "ESN", "(", str_layer, ", ", str_pars, ", ", str_season, ")")
  
  return(model_spec)
}



#' @title Create the input weight matrix.
#' 
#' @description This function creates the random input weight matrices.
#' 
#' @param n_inputs Integer value. The number of input features.
#' @param n_res Integer value. The number of internal states within the reservoir (reservoir size).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' 
#' @return win List containing the input weight matrices.

create_win <- function(n_inputs,
                       n_res,
                       scale_runif) {
  win <- matrix(
    data = runif(n = n_res * n_inputs,
                 min = scale_runif[1],
                 max = scale_runif[2]),
    nrow = n_res,
    ncol = n_inputs)
  return(win)
}




#' @title Create the reservoir weight matrix.
#' 
#' @description This function creates the random reservoir weight matrix (scaled to spectral radius rho).
#' 
#' @param n_res Integer value. The number of internal states within the reservoir (reservoir size).
#' @param rho Numeric value. The spectral radius for scaling the weight matrix.
#' @param density Numeric value. The parameter defines the connectivity of the reservoir weight matrix (dense or sparse).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param symmetric Logical value. If \code{TRUE}, the matrix is symmetric.
#' 
#' @return wres Numeric matrix. The final reservoir weight matrix.

create_wres <- function(n_res,
                        rho,
                        density,
                        scale_runif,
                        symmetric = FALSE) {
  
  # Create initial random weight matrix for the reservoir
  wres <- matrix(
    data = runif(n = n_res * n_res,
                 min = scale_runif[1],
                 max = scale_runif[2]),
    nrow = n_res,
    ncol = n_res)
  
  # Create a random sparse pattern matrix with defined density
  wsparse <- rsparsematrix(
    nrow = n_res,
    ncol = n_res,
    density = density,
    rand.x = NULL,
    symmetric = symmetric)
  
  wres <- as.matrix(wres * wsparse)
  
  # Calculate the absolute, maximum eigenvalue of the reservoir weight matrix
  eig <- eigen(wres, symmetric = symmetric, only.values = TRUE)$values
  max_abs_eig <- max(abs(eig))
  
  # Rescale the reservoir weight matrix to spectral radius rho
  wres <- 1 / max_abs_eig * wres * rho
  return(wres)
}



#' @title Calculate n-th differences.
#' 
#' @description This function takes a numeric matrix and calculates n-th differences for each column. Leading NAs are padded.
#'
#' @param data Numeric matrix.
#' @param n_diff Integer value. The number of differences.
#' @param na_rm Logical value. If \code{TRUE}, NAs are ignored.
#'
#' @return y_diff Numeric matrix with differenced data.

diff_data <- function(data, n_diff, na_rm = TRUE) {
  
  names_outputs <- colnames(data)
  n_outputs <- ncol(data)
  
  y_diff <- colDiffs(
    data,
    lag = 1L,
    differences = n_diff)
  
  colnames(y_diff) <- names_outputs
  
  if (na_rm == FALSE) {
    fill_na <- matrix(
      data = NA_real_,
      nrow = n_diff,
      ncol = n_outputs)
    y_diff <- rbind(fill_na, y_diff)
  }
  return(y_diff)
}



#' @title Forecast a fitted ESN.
#' 
#' @description Calculate point forecasts for a fitted ESN (internally).
#' 
#' @param win Numeric matrix. Weights for the input variables.
#' @param wres Numeric matrix. Weights for the reservoir.
#' @param wout Numeric matrix. Weights for output variables (estimated coefficients from ridge regression).
#' @param n_ahead Integer value. The forecast horizon (n-step ahead).
#' @param alpha Numeric value. The Leakage rate (smoothing parameter).
#' @param lags List containing integer vectors with the lags associated with each output variable.
#' @param inputs Numeric matrix. Initialized input features (gets updated during forecasting process).
#' @param states_train Numeric matrix. Internal states from training (necessary due to last values).
#' @param innov Numeric matrix. The innovations for simulation.
#' 
#' @return fcst Numeric matrix with point forecasts.
#' @return states_fcst Numeric matrix with internal states used for forecasting.

predict_esn <- function(win,
                        wres,
                        wout,
                        n_ahead,
                        alpha,
                        lags,
                        inputs,
                        states_train,
                        innov = NULL) {
  
  # Number of output variables and internal states (reservoir size)
  n_outputs <- ncol(wout)
  n_res <- nrow(wres)
  
  # Preallocate empty matrices to store point forecasts and internal states
  fcst <- matrix(
    data = NA_real_,
    nrow = n_ahead,
    ncol = n_outputs,
    dimnames = list(c(), colnames(wout)))
  
  states_fcst_upd <- matrix(
    data = NA_real_,
    nrow = (n_ahead + 1),
    ncol = (n_res),
    dimnames = list(c(), colnames(states_train)))
  
  # Create copy and fill first row with last values from states_train
  states_fcst <- states_fcst_upd
  states_fcst[1, ] <- states_train[nrow(states_train), ]
  
  # Number of lags by output variable
  n_lags <- lapply(lags, length)
  # Names of lagged variables as list
  names_lags_list <- lapply(seq_len(n_outputs), function(n) {
    paste(colnames(wout)[n], "(", lags[[n]], ")", sep = "")
  })
  
  # Dynamic forecasting (iterative mode)
  for (t in 2:(n_ahead + 1)) {
    # Calculate new internal states
    states_fcst_upd[t, ] <- t(tanh(win %*% t(inputs[t, , drop = FALSE]) + wres %*% t(states_fcst[(t - 1), , drop = FALSE])))
    states_fcst[t, ] <- alpha * states_fcst_upd[t, , drop = FALSE] + (1 - alpha) * states_fcst[(t - 1), , drop = FALSE]
    
    # Prepare design matrix
    X <- cbind(inputs[t, , drop = FALSE], states_fcst[t, , drop = FALSE])
    
    # Calculate point forecasts and save values
    if (is.null(innov)) {
      fcst[(t - 1), ] <- X %*% wout
    } else {
      fcst[(t - 1), ] <- X %*% wout + innov[(t - 1), , drop = FALSE]
    }
    
    # Update lagged variables in inputs
    for (i in seq_len(n_outputs)) {
      # Column index for block-wise looping and updating (by variable)
      index_col <- names_lags_list[[i]]
      # Row index for block-wise looping and updating (by variable)
      index_row <- inputs[, index_col, drop = FALSE]
      index_row <- as.numeric(apply(index_row, MARGIN = 2, FUN = function(x) min(which(is.na(x)))))
      for (ii in seq_len(n_lags[[i]])) {
        inputs[index_row[ii], index_col[ii]] <- fcst[(t-1), i]
      }
    }
  }
  
  # Store and return results
  result <- list(
    fcst = fcst,
    states_fcst = states_fcst)
  
  return(result)
}




#' @title Rescale (inverse scaling) the columns of a numeric matrix.
#' 
#' @description Recale (inverse scaling) the columns of a numeric matrix by applying the transformation backwards to original range.
#' 
#' @param data Numeric matrix containing the values to be rescaled. Each column is a variable and each row an observation.
#' @param old_range Numeric matrix with ranges (min and max) of original data.
#' @param new_range Numeric vector with new (scaled) interval.
#' 
#' @return data Numeric matrix with rescaled columns.

rescale_data <- function(data, old_range, new_range) {
  # Check y for missing values
  if (anyNA(data) == TRUE) {
    stop("data contains at least one missing value")
  }
  
  # Number of rows and columns in data
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # Extract maximum and minimum for inverse scaling
  min <- old_range["min", ]
  max <- old_range["max", ]
  
  # Extract lower and upper bound
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Vectorize calculation
  min <- matrix(
    data = rep(min, each = n_rows) ,
    nrow = n_rows,
    ncol = n_cols)
  
  max <- matrix(
    data = rep(max, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  lower <- matrix(
    data = rep(lower, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  upper <- matrix(
    data = rep(upper, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  # Inverse normalization to original interval
  data <- ((data - lower) * (max - min)) / (upper - lower) + min
  return(data)
}




#' @title Scale the columns of a numeric matrix.
#' 
#' @description Scale the columns of a numeric matrix to a specific interval.
#' 
#' @param data Numeric matrix containing the values to be scaled. Each column is a variable and each row an observation.
#' @param new_range Numeric vector. The range for scaling (first value represents the replacement for the min value, the second is the substitute for the max value).
#' 
#' @return data Numeric matrix with scaled columns.

scale_data <- function(data, new_range = c(-1, 1)) {
  
  # # Check y for missing values
  # if (anyNA(data) == TRUE) {
  #   stop("data contains at least one missing value")
  #   }
  
  # Number of rows (observations) and columns (variables) in data
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # Calculate minimum and maximum by column
  min <- colMins(data, na.rm = TRUE)
  max <- colMaxs(data, na.rm = TRUE)
  old_range <- rbind(min, max)
  rownames(old_range) <- c("min", "max")
  colnames(old_range) <- colnames(data)
  
  # Extract the lower and upper bound from interval
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Vectorize calculations (i.e. expand min, max, lower and upper to matrix)
  min <- matrix(
    data = rep(min, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  max <- matrix(
    data = rep(max, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  lower <- matrix(
    data = rep(lower,each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  upper <- matrix(
    data = rep(upper, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  # Scale matrix y column wise to new interval
  data <- lower + ((data - min) * (upper - lower) / (max - min))
  
  result <- list(
    data = data,
    old_range = old_range)
  
  return(result)
}




#' @title Simulate a fitted ESN.
#' 
#' @description Simulate future sample path from a fitted ESN.
#' 
#' @param win Numeric matrix. Weights for the input variables.
#' @param wres Numeric matrix. Weights for the reservoir.
#' @param wout Numeric matrix. Weights for the output variables (estimated coefficients from ridge regression).
#' @param n_ahead Integer value. The number of periods for forecasting (forecast horizon).
#' @param alpha Numeric value. The Leakage rate (smoothing parameter).
#' @param lags List containing integer vectors with the lags associated with each output variable.
#' @param inputs Numeric matrix. Initialized input features (gets updated during forecasting process).
#' @param states_train Numeric matrix. Internal states from training (necessary due to last values).
#' @param error Numeric matrix. The innovations for simulation (re-sampled residuals from fitted model).
#' @param n_sim Integer value. The number of simulations.
#' 
#' @return sim List with simulated future sample paths as numeric matrix.

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
