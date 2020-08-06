
#' @title Create constant (intercept term)
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



#' @title Create lagged variables of a matrix
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



#' @title Create lagged variables of a matrix for iterative forecasting
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




#' @title Create fourier terms
#' 
#' @description This function creates the fourier terms for the design matrix as numeric matrix.
#'
#' @param times Integer vector. Regular sequence with integers.
#' @param n_fourier Integer vector. The number of fourier terms per period (i.e. the number of sines and cosines for each period).
#' @param period Integer vector. The periodicity of the time series.
#' 
#' @return

create_fourier <- function(times,
                           n_fourier,
                           period) {
  
  # Patch for older versions of R that do not have sinpi and cospi functions.
  if (!exists("sinpi")) {
    sinpi <- function(x) {
      sin(pi * x)
    }
    cospi <- function(x) {
      cos(pi * x)
    }
  }
  
  if (length(period) != length(n_fourier)) {
    stop("Number of periods does not match number of orders")
  }
  if (any(2 * n_fourier > period)) {
    stop("n_fourier must be not be greater than period/2")
  }
  
  # Compute periods of all Fourier terms
  p <- numeric(0)
  labels <- character(0)
  for (j in seq_along(period))
  {
    if (n_fourier[j] > 0) {
      p <- c(p, (1:n_fourier[j]) / period[j])
      labels <- c(labels, paste0(
        paste0(c("sin(", "cos("), rep(1:n_fourier[j], rep(2, n_fourier[j]))),
        "-", round(period[j]), ")"))
    }
  }
  # Remove equivalent seasonal periods due to multiple seasonality
  n_fourier <- duplicated(p)
  p <- p[!n_fourier]
  labels <- labels[!rep(n_fourier, rep(2, length(n_fourier)))]
  
  # Remove columns where sinpi = 0
  n_fourier <- abs(2 * p - round(2 * p)) > .Machine$double.eps
  
  # Compute matrix of Fourier terms
  X <- matrix(NA_real_, nrow = length(times), ncol = 2L * length(p))
  for (j in seq_along(p))
  {
    if (n_fourier[j]) {
      X[, 2L * j - 1L] <- sinpi(2 * p[j] * times)
    }
    X[, 2L * j] <- cospi(2 * p[j] * times)
  }
  colnames(X) <- labels
  
  # Remove missing columns
  X <- X[, !is.na(colSums(X)), drop = FALSE]
  
  return(X)
}



#' @title Rotate a matrix
#' 
#' @description Little helper function to rotate a matrix. If you apply rotate
#'    two times, the matrix is flipped (\code{rotate(rotate(x))}).
#'
#' @param x A matrix.
#'
#' @return A matrix.

rotate <- function(x) {
  t(apply(x, 2, rev))
}



#' @title Create a grid of fourier terms for best subset regression
#' 
#' @description \code{create_grid_fourier} creates a grid of all combinations
#'    of fourier terms. One fourier term is always a pair of sine and cosine
#'    terms. If higher order fourier terms are used, it is assumed that the
#'    previous terms are added too. 
#'
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles) per period.
#' @param period Integer vector. The periodicity of the time series (e.g. for monthly data \code{period = c(12)}, for hourly data \code{period = c(24, 168)}).
#'
#' @return blocks A tibble containing zeros and ones for all combinations of fourier terms.

create_grid_fourier <- function(n_fourier,
                                period) {
  
  # Initialize empty list
  blocks <- vector(
    mode = "list",
    length = length(period)
  )
  
  # Create matrices with zeros and ones as blocks
  for (j in 1:length(period)) {
    
    # Initialize matrix with zeros
    mat <- matrix(
      data = 0,
      nrow = n_fourier[j],
      ncol = n_fourier[j]
    )
    
    # Fill lower triangular with ones
    mat[lower.tri(mat, diag = TRUE)] <- 1
    
    # Repeat each column two times (one for sine and one for cosine)
    mat <- matrix(
      data = rep(mat, each = 2),
      ncol = 2 * ncol(mat), 
      byrow = TRUE)
    
    # Add row with zeros and flip matrix
    mat <- rbind(mat, 0)
    mat <- rotate(rotate(mat))
    
    colnames(mat) <- paste0(
      paste0(c("sin(", "cos("), rep(1:n_fourier[j], rep(2, n_fourier[j]))),
      "-", round(period[j]), ")")
    
    # Store matrices in list
    blocks[[j]] <- mat
  }
  
  # Merge matrices
  blocks <- as_tibble(do.call(merge, blocks))
  return(blocks)
}



#' @title Create model specification
#' 
#' @description This function creates the model specification (short summary) as a string.
#'
#' @param model_layers List containing the number of inputs (n_inputs), reservoir size (n_res) and the number of outputs (n_outputs).
#' @param model_pars List containing the hyperparameters alpha, rho, lambda and density.
#' @param model_inputs List containing the model inputs (constant, lags, n_fourier, period).
#'
#' @return model_spec Character value. The model specification as string.

create_spec <- function(model_layers,
                        model_pars,
                        model_inputs) {
  
  # Number of inputs and outputs and reservoir size
  str_layer <- paste0(
    "{",
    model_layers$n_inputs, ",",
    model_layers$n_res, ",",
    model_layers$n_outputs,
    "}")
  
  # Hyperparameters
  str_pars <- paste0(
    "{",
    round(model_pars$alpha, 2), ",",
    round(model_pars$rho, 2), ",",
    round(model_pars$lambda, 2),
    "}")
  
  # Seasonality and periodicity
  if (is.null(model_inputs$n_fourier)) {
    str_season <- NULL
  } else {
    str_season <- paste(
      ", {",
      paste("(", model_inputs$period, "-", model_inputs$n_fourier, ")",
            collapse = ",",
            sep = ""), "}",
      sep = "")
  }
  
  # Model specification
  model_spec <- paste0(
    "ESN", "(", str_layer, ", ", str_pars, str_season, ")")
  
  return(model_spec)
}



#' @title Create the input weight matrix
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




#' @title Create the reservoir weight matrix
#' 
#' @description This function creates the random reservoir weight matrix
#'    (scaled to spectral radius rho).
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



#' @title Test for seasonal and non-seasonal differences required to achieve stationarity
#' 
#' @description This function tests for the number of seasonal and
#'    non-seasonal differences required to achieve stationarity.
#'
#' @param .data A \code{tsibble} in wide format.
#' @param period Integer value. The seasonal period of the time series.
#' @param alpha Numeric value. The significance level for the statistical test.
#'
#' @return A list with the number of seasonal differences (\code{n_sdiff})
#'    and non-seasonal differences (\code{n_diff}).
#'    
#' @export

check_unitroots <- function(.data,
                            period,
                            alpha = 0.05) {
  
  # Number of observations
  n_obs <- nrow(.data)
  
  # Maximum seasonal period
  period <- max(period)

  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(.data)[measured_vars(.data)])
  
  # Test for seasonal differences
  n_sdiff <- apply(y, 2, function(y) {
    unitroot_nsdiffs(
      x = y,
      alpha = alpha,
      unitroot_fn = ~feat_stl(., .period)[2] < 0.64,
      differences = 0:1,
      .period = period)
  })
  
  yd <- diff_data(
    data = y,
    period = period,
    n_sdiff = n_sdiff,
    n_diff = rep(0, times = ncol(y))
  )
  
  # Test for non-seasonal differences
  n_diff <- apply(y, 2, function(y) {
    unitroot_ndiffs(
      x = yd,
      alpha = alpha,
      unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],
      differences = 0:1)
  })
  
  structure(
    list(
      n_sdiff = n_sdiff,
      n_diff = n_diff
    )
  )
}



#' @title Calculate seasonal and non-seasonal differences of a numeric matrix.
#'
#' @description This function takes a numeric matrix and calculates seasonal
#'    and non-seasonal differences for each column.
#'
#' @param data Numeric matrix.
#' @param period Integer vector. The periodicity of the time series.
#' @param n_sdiff Integer vector. The number of seasonal differences.
#' @param n_diff Integer vector. The number of non-seasonal differences.
#'
#' @return y_diff Numeric matrix with the differenced data.

diff_data <- function(data,
                      period,
                      n_sdiff,
                      n_diff) {
  
  names_outputs <- colnames(data)
  n_outputs <- ncol(data)
  
  y_diff <- lapply(seq_len(n_outputs), function(n) {
    diff_vec(
      y = data[, n],
      period = max(period),
      n_sdiff = n_sdiff[n],
      n_diff = n_diff[n])
  })
  
  y_diff <- do.call(cbind, y_diff)
  colnames(y_diff) <- names_outputs
  return(y_diff)
}




#' @title Calculate seasonal and non-seasonal differences of a numeric vector
#'
#' @description This function takes a numeric vector and calculates seasonal
#'    and non-seasonal differences.
#'
#' @param y Numeric vector.
#' @param period Integer vector. The periodicity of the time series.
#' @param n_sdiff Integer value. The number of seasonal differences.
#' @param n_diff Integer value. The number of non-seasonal differences.
#'
#' @return y_diff Numeric vector with the differenced data.

diff_vec <- function(y,
                     period,
                     n_sdiff,
                     n_diff) {
  
  # Calculate seasonal difference
  if (n_sdiff > 0) {
    y_diff <- diff(
      x = y,
      differences = n_sdiff,
      lag = period)
  } else {
    y_diff <- y
  }
  
  # Calculate non-seasonal difference
  if (n_diff > 0) {
    y_diff <- diff(
      x = y_diff,
      differences = n_diff,
      lag = 1L)
  } else {
    y_diff <- y_diff
  }
  
  # Pad vector with leading NAs
  fill_na <- rep(NA_real_, (n_sdiff * period + n_diff))
  y_diff <- c(fill_na, y_diff)
  return(y_diff)
}




#' @title Integrate seasonal and non-seasonal differences of a numeric matrix
#'    ("inverse difference")
#'
#' @description This function takes a numeric matrix and integrates seasonal
#'    and non-seasonal differences for each column ("inverse difference").
#'
#' @param data Numeric matrix containing the original data.
#' @param data_diff Numeric matrix containing the differenced data.
#' @param period Integer vector. The periodicity of the time series.
#' @param n_sdiff Integer vector. The number of seasonal differences.
#' @param n_diff Integer vector. The number of non-seasonal differences.
#' 
#' @return y_int Numeric matrix with the inverse differenced data.

inv_diff_data <- function(data,
                          data_diff,
                          period,
                          n_sdiff,
                          n_diff) {
  
  names_outputs <- colnames(data)
  n_outputs <- ncol(data)
  
  y_int <- lapply(seq_len(n_outputs), function(n) {
    inv_diff_vec(
      y = data[, n],
      y_diff = data_diff[, n],
      period = max(period),
      n_sdiff = n_sdiff[n],
      n_diff = n_diff[n])
  })
  
  y_int <- do.call(cbind, y_int)
  colnames(y_int) <- names_outputs
  return(y_int)
}




#' @title Integrate seasonal and non-seasonal differences of a numeric vector
#'    ("inverse difference")
#'
#' @description This function takes a numeric vector and integrates seasonal
#'    and non-seasonal differences ("inverse difference").
#'
#' @param y Numeric vector containing the original data.
#' @param y_diff Numeric vector containing the differenced data.
#' @param period Integer vector. The periodicity of the time series.
#' @param n_sdiff Integer value. The number of seasonal differences.
#' @param n_diff Integer value. The number of non-seasonal differences.
#'
#' @return y_int Numeric vector with the inverse differenced data.

inv_diff_vec <- function(y,
                         y_diff,
                         period,
                         n_sdiff,
                         n_diff) {
  
  y <- as.numeric(y)
  y_diff <- as.numeric(na.omit(y_diff))
  
  # Forecast horizon
  n_ahead <- length(y_diff)
  
  yi <- tail(y, n_diff)             # starting value for non-seasonal differences
  yii <- tail(y, n_sdiff * period)  # starting value for seasonal differences
  idx <- length(y) - period         # index of starting value
  
  #1: Doubled differenced
  if (n_sdiff > 0 & n_diff > 0) {
    
    # Integrate first difference
    yi <- diff(
      x = y,
      differences = n_sdiff,
      lag = period)[idx]
    
    y_int <- diffinv(
      x = y_diff,
      lag = 1L,
      differences = n_diff,
      xi = yi)
    
    # Integrate seasonal difference
    y_int <- diffinv(
      x = y_int,
      lag = period,
      differences = n_sdiff,
      xi = yii)
  }
  
  # Case 2: Seasonal differenced only
  if (n_sdiff > 0 & n_diff == 0) {
    
    # Integrate seasonal difference
    y_int <- diffinv(
      x = y_diff,
      lag = period,
      differences = n_sdiff,
      xi = yii)
  }
  
  # Case 3: Non-seasonal differenced only
  if (n_sdiff == 0 & n_diff > 0) {
    
    # Integrate difference
    y_int <- diffinv(
      x = y_diff,
      lag = 1L,
      differences = n_diff,
      xi = yi)
  }
  
  # Case 4: No differences at all
  if (n_sdiff == 0 & n_diff == 0) {
    y_int <- y_diff
  }
  
  # Cut y_int to previous length
  y_int <- y_int[c((length(y_int) - n_ahead + 1):length(y_int))]
  
  return(y_int)
}




#' @title Estimate best subset autoregressive models and select lags
#' 
#' @description Estimate best subset autoregressive models and select lags.
#'    The underlying model is an ARp model (subset autoregressive model) and
#'    by default the BICq information criterion is used. This is a wrapper
#'    function for \code{select_lags_vec} that works on the columns of a
#'    numeric matrix.
#'
#' @param data Numeric matrix containing the original data.
#' @param period Integer vector. The periodicity of the time series.
#' @param n_sdiff Integer vector. The number of seasonal differences.
#' @param n_diff Integer vector. The number of non-seasonal differences.
#' @param max_lag Integer value. Maximum number of non-seasonal lags.
#'
#' @return

select_lags <- function(.data,
                        period,
                        n_sdiff,
                        n_diff,
                        max_lag) {
  
  y <- invoke(cbind, unclass(.data)[measured_vars(.data)])
  n_outputs <- ncol(y)
  
  lags <- lapply(seq_len(n_outputs), function(n) {
    select_lags_vec(
      y = y[, n],
      period = period,
      n_sdiff = n_sdiff[n],
      n_diff = n_diff[n],
      max_lag = max_lag)
  })
  
  return(lags)
}



#' @title Estimate best subset autoregressive models and select lags
#' 
#' @description Estimate best subset autoregressive models and select lags.
#'    The underlying model is an ARp model (subset autoregressive model) and
#'    by default the BICq information criterion is used.
#'
#' @param y Numeric vector containing the original data.
#' @param period Integer vector. The periodicity of the time series.
#' @param n_sdiff Integer value. The number of seasonal differences.
#' @param n_diff Integer value. The number of non-seasonal differences.
#' @param max_lag Integer value. Maximum number of non-seasonal lags.
#'
#' @return

select_lags_vec <- function(y,
                            period,
                            n_sdiff,
                            n_diff,
                            max_lag) {
  # Difference data
  yd <- diff_vec(
    y = y,
    period = max(period),
    n_sdiff = n_sdiff,
    n_diff = n_diff
  )
  
  # Drop NAs
  yd <- na.omit(yd)
  
  # Convert to msts object
  # yd <- msts(yd, seasonal.periods = period)
  
  # Estimate subset autoregressive models (ARp)
  lags <- SelectModel(
    z = yd,
    lag.max = max_lag,
    ARModel = "ARp",
    Criterion = "BICq",
    Best = 1)
  
  lags <- union(lags, period)
  return(lags)
}






#' @title Model selection
#' 
#' @description Model selection procedure
#'
#' @param data A \code{tsibble} containing the time series data.
#' @param lags A list containing integer vectors with the lags associated with each output variable.
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles per period).
#' @param period Integer vector. The periodicity of the time series (e.g. for monthly data \code{period = c(12)}, for hourly data \code{period = c(24, 168)}).
#' @param const Logical value. If \code{TRUE}, a constant term (intercept) is used.
#' @param n_sdiff Integer vector. The number of seasonal differences. 
#' @param n_diff Integer vector. The number of non-seasonal differences.
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("AIC", "BIC", "HQ")}.
#'
#' @return A list with vectors const and lags

select_inputs <- function(data,
                          lags,
                          n_fourier,
                          period,
                          const,
                          n_sdiff,
                          n_diff,
                          n_initial,
                          scale_inputs,
                          inf_crit) {
  
  # Pre-processing ============================================================
  
  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(data)[measured_vars(data)])
  
  # Calculate seasonal and non-seasonal differences
  y <- diff_data(
    data = y,
    period = period,
    n_sdiff = n_sdiff,
    n_diff = n_diff)
  
  # Scale data to the specified interval
  scaled <- scale_data(
    data = y,
    new_range = scale_inputs)
  
  y <- scaled$data
  old_range <- scaled$old_range
  
  
  # Create input layer ========================================================
  
  # Create lagged variables as matrix
  if (is.null(lags)) {
    y_lag <- NULL
  } else {
    y_lag <- create_lags(
      data = y,
      lags = lags)
  }
  
  # Create fourier terms (trigonometric terms) as matrix
  if (all(n_fourier == 0)) {
    y_seas <- NULL
  } else {
    y_seas <- create_fourier(
      times = 1:nrow(y),
      n_fourier = n_fourier,
      period = period)
  }
  
  # Create constant term (intercept term) as matrix
  if (const == FALSE) {
    y_const <- NULL
  } else {
    y_const <- create_const(
      n_obs = nrow(y))
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_const,
    y_lag,
    y_seas)
  
  # Drop NAs for training
  inputs <- inputs[complete.cases(inputs), ]
  
  # Number of observations (total)
  n_total <- nrow(y)
  # Number of observations (training)
  n_train <- nrow(inputs)
  
  
  # Create output layer (train model) =========================================
  
  # Concatenate inputs and reservoir
  X <- inputs
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- X[((n_initial + 1):nrow(X)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):nrow(y)), , drop = FALSE]
  
  # Linear observation weights within the interval [1, 2]
  # obs_weights <- (0:(nrow(Xt) - 1)) * (1 / (nrow(Xt) - 1)) + 1
  # Equal observation weights
  obs_weights <- rep(1, nrow(Xt))
  
  
  # names_lags <- colnames(y_lag)
  # names_seas <- colnames(y_seas)
  #
  # # Split a vector x into n chunks
  # split_vec <- function(x, n) {
  #   split(x, cut(seq_along(x), n, labels = FALSE)) 
  # }
  # 
  # fourier_terms <- split_vec(
  #   x = names_seas,
  #   n = sum(n_fourier, na.rm = TRUE))
  # 
  # fourier_terms <- transpose(
  #   .l = fourier_terms,
  #   .names = c("sine", "cosine")) %>% 
  #   simplify_all() %>%
  #   as_tibble() %>%
  #   unite(
  #     col = "pair",
  #     sep = "_",
  #     remove = FALSE)
  
  
  # Prepare model grid
  # names_inputs <- c(colnames(y_const), colnames(y_lag), fourier_terms$pair)
  names_inputs <- c(colnames(y_const), colnames(y_lag))
  n_inputs <- length(names_inputs)
  
  # Create named list with input names and vector with 0 and 1
  model_grid <- setNames(rep(list(c(0, 1)), n_inputs), names_inputs)
  # Create all possible combinations of the inputs as tibble
  model_grid <- cross_df(model_grid)
  # Drop first row (white noise model)
  model_grid <- model_grid[-c(1), ]
  # Number of combinations
  n_models <- nrow(model_grid)
  
  
  # model_seas <- model_grid %>%
  #   select(fourier_terms$pair)
  # 
  # sine <- model_seas
  # names(sine) <- fourier_terms$sine
  # 
  # cosine <- model_seas
  # names(cosine) <- fourier_terms$cosine
  # 
  # model_grid <- bind_cols(
  #   model_grid,
  #   sine,
  #   cosine)
  # 
  # model_grid <- model_grid %>%
  #   select(-fourier_terms$pair)
  
  
  # Train models via least squares
  model_metrics <- 1:n_models %>% map_dfr(
    .f = function(n) {
      # Train individual models
      model <- train_ridge(
        X = Xt[, which(model_grid[n, ] == 1), drop = FALSE],
        y = yt,
        lambda = 0,
        weights = obs_weights)
      
      # Store model metrics
      model_metrics <- tibble(
        df = model$df,
        AIC = model$aic,
        BIC = model$bic,
        HQ = model$hq)
    }
  )
  
  # Filter row with minimum information criterion
  model_metrics <- model_metrics %>%
    mutate(id = row_number(), .before = df) %>%
    slice(which.min(!!sym(inf_crit)))
  
  # Filter for optimal model and transpose to long format
  model_inputs <- model_grid %>%
    slice(model_metrics$id) %>%
    pivot_longer(
      cols = everything(),
      names_to = "input",
      values_to = "usage")
  
  # Check for constant (intercept) term
  const <- model_inputs %>%
    filter(input == "const") %>%
    pull(usage)
  
  const <- ifelse(const == 1, TRUE, FALSE)
  
  # Check for relevant lags
  lags <- model_inputs %>%
    filter(!input == "const") %>%
    filter(usage == 1) %>%
    pull(input) %>%
    parse_number() %>%
    list()
  
  list(
    const = const,
    lags = lags
  )
}







#' @title Forecast a fitted ESN
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




#' @title Rescale (inverse scaling) the columns of a numeric matrix
#' 
#' @description Rescale (inverse scaling) the columns of a numeric matrix
#'    by applying the transformation backwards to original range.
#' 
#' @param data Numeric matrix containing the values to be rescaled. Each column is a variable and each row an observation.
#' @param old_range Numeric matrix with ranges (min and max) of original data.
#' @param new_range Numeric vector with new (scaled) interval.
#' 
#' @return data Numeric matrix with rescaled columns.

rescale_data <- function(data,
                         old_range,
                         new_range) {
  
  # # Check y for missing values
  # if (anyNA(data) == TRUE) {
  #   stop("data contains at least one missing value")
  # }
  
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




#' @title Scale the columns of a numeric matrix
#' 
#' @description Scale the columns of a numeric matrix to a specific interval.
#' 
#' @param data Numeric matrix containing the values to be scaled. Each column is a variable and each row an observation.
#' @param new_range Numeric vector. The range for scaling (first value represents the replacement for the min value, the second is the substitute for the max value).
#' 
#' @return data Numeric matrix with scaled columns.

scale_data <- function(data,
                       new_range = c(-1, 1)) {
  
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




#' @title Simulate a fitted ESN
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
