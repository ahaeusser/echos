
#' @title Create lagged variables of a matrix
#' 
#' @description Create lagged variables of a matrix, shifting each column
#'   back by a given number of observations.
#' 
#' @param data Numeric vector or matrix. Each column is a variable and each row an observation.
#' @param lags List containing vectors with the number of lags (in units of observations) per variable.
#' 
#' @return y_lag Numeric matrix with lagged variables of the input data.
#' @noRd

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
    ncol = n_lags
    )
  
  # Create matrix with combinations of input variables and lags
  index <- matrix(
    data = c(rep(seq(1, n_inputs),
                 times = n_lags_inputs),
             unlist(lags)),
    ncol = 2
    )
  
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
#' @description Create lagged variables of a matrix for iterative forecasting,
#'   shifting each column back by a given number of observations and fill with NAs for the updates.
#' 
#' @param data Numeric vector or matrix. Each column is a variable and each row an observation.
#' @param lags List containing vectors with the number of lags (in units of observations) per variable.
#' @param n_ahead Integer value. The forecast horizon (n-step ahead).
#' 
#' @return y_lag Numeric matrix with the lagged variables of the input data for iterative forecasting.
#' @noRd

create_revolved <- function(data,
                            lags,
                            n_ahead) {
  
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
    ncol = n_lags
    )
  
  # Create matrix with combinations of input variables and lags
  index <- matrix(
    data = c(rep(seq(1, n_inputs),
                 times = n_lags_inputs),
             unlist(lags)),
    ncol = 2
    )
  
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
#' @param x Integer vector. Regular sequence with integers.
#' @param period Integer vector. The periodicity of the time series.
#' @param k Integer vector. The number of fourier terms per period (i.e. the number of sines and cosines for each period).
#'  
#' @return out Numeric matrix containing the specified fourier terms.
#' @noRd

create_fourier <- function(x,
                           period,
                           k) {
  
  if (length(period) != length(k)) {
    stop("Number of periods does not match number of orders")
  }
  
  if (any(2 * k > period)) {
    stop("k must be not be greater than period/2")
  }
  
  # Compute periods of all fourier terms
  p <- numeric(0)
  labels <- character(0)
  for (j in seq_along(period)) {
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
  
  # Compute matrix of fourier terms
  out <- matrix(
    data = NA_real_,
    nrow = length(x),
    ncol = 2L * length(p))
  
  for (j in seq_along(p)) {
    if (k[j]) {
      out[, 2L * j - 1L] <- sinpi(2 * p[j] * x)
    }
    out[, 2L * j] <- cospi(2 * p[j] * x)
  }
  
  colnames(out) <- labels
  
  # Remove missing columns
  out <- out[, !is.na(colSums(out)), drop = FALSE]
  
  return(out)
}



#' @title Create model specification
#' 
#' @description This function creates the model specification (short summary) as a string.
#'
#' @param model_layers List containing the number of inputs (n_inputs), 
#'   reservoir size (n_res), internal states (n_states) and the number of 
#'   outputs (n_outputs).
#'
#' @return model_spec Character value. The model specification as string.
#' @noRd

create_spec <- function(model_layers) {
  
  # Number of input variables
  n_inputs <- model_layers$n_inputs
  # Number of reservoirs
  n_res <- model_layers$n_res
  # Number of internal states per reservoir
  n_states <- model_layers$n_states
  # Number of output variables
  n_outputs <- model_layers$n_outputs
  
  # Model specification
  model_spec <- paste0(
    "ESN", "(", 
    n_inputs, ",", 
    "{", n_res, ",", n_states, "}", ",", 
    n_outputs, 
    ")")
  
  return(model_spec)
}


#' @title Create the input weight matrix
#' 
#' @description This function creates the random input weight matrices.
#' 
#' @param n_inputs Integer value. The number of input features.
#' @param n_states Integer value. The number of internal states within the reservoir (reservoir size).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' 
#' @return win List containing the input weight matrices.
#' @noRd

create_win <- function(n_inputs,
                       n_states,
                       scale_runif) {
  
  win <- matrix(
    data = runif(
      n = n_states * n_inputs,
      min = scale_runif[1],
      max = scale_runif[2]),
    nrow = n_states,
    ncol = n_inputs
    )
  
  return(win)
}



#' @title Create the reservoir weight matrix
#' 
#' @description This function creates the random reservoir weight matrix
#'    (scaled to spectral radius rho).
#' 
#' @param n_states Integer value. The number of internal states within the reservoir (reservoir size).
#' @param rho Numeric value. The spectral radius for scaling the weight matrix.
#' @param density Numeric value. The parameter defines the connectivity of the reservoir weight matrix (dense or sparse).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' @param symmetric Logical value. If \code{TRUE}, the matrix is symmetric.
#' 
#' @return wres Numeric matrix. The final reservoir weight matrix.
#' @noRd

create_wres <- function(n_states,
                        rho,
                        density,
                        scale_runif,
                        symmetric = FALSE) {
  
  # Create initial random weight matrix for the reservoir
  wres <- matrix(
    data = runif(
      n = n_states * n_states,
      min = scale_runif[1],
      max = scale_runif[2]),
    nrow = n_states,
    ncol = n_states
    )
  
  # Create a random sparse pattern matrix with defined density
  wsparse <- rsparsematrix(
    nrow = n_states,
    ncol = n_states,
    density = density,
    rand.x = NULL,
    symmetric = symmetric
    )
  
  wres <- as.matrix(wres * wsparse)
  
  # Calculate the absolute, maximum eigenvalue of the reservoir weight matrix
  eig <- eigen(wres, symmetric = symmetric, only.values = TRUE)$values
  max_abs_eig <- max(abs(eig))
  
  # Rescale the reservoir weight matrix to spectral radius rho
  wres <- 1 / max_abs_eig * wres * rho
  return(wres)
}



#' @title Helper function to concatenate a string and one number.
#' 
#' @description Helper function to concatenate a string and one number (e.g.
#'   test(1), test(2), ..., test(n)).
#'
#' @param x Character value.
#' @param n Integer value.
#'
#' @return x Character vector.
#' @noRd

paste_names <- function(x, n) {
  x <- paste0(
    x,"(",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"),
    ")")
  
  return(x)
}



#' @title Calculate nth-differences of a numeric matrix
#'
#' @description This function takes a numeric matrix and calculates 
#'    nth-differences for each column.
#'
#' @param data Numeric matrix.
#' @param n_diff Integer vector. The number of differences.
#'
#' @return yd Numeric matrix with the differenced data.
#' @noRd

diff_data <- function(data, n_diff) {
  
  name_output <- colnames(data)
  n_output <- ncol(data)
  
  yd <- lapply(
    seq_len(n_output),
    function(i) {
      diff_vec(
        y = data[, i],
        n = n_diff[i])
      }
    )
  
  yd <- do.call(cbind, yd)
  colnames(yd) <- name_output
  return(yd)
}


#' @title Calculate the nth-difference of a numeric vector
#'
#' @description This function takes a numeric vector and calculates
#'    the nth-difference.
#'
#' @param y Numeric vector.
#' @param n Integer value. The number of differences.
#'
#' @return yd Numeric vector with the differenced data.
#' @noRd

diff_vec <- function(y, n) {
  
  # Calculate n-th difference
  if (n > 0) {
    yd <- diff(
      x = y,
      differences = n,
      lag = 1L
      )
  } else {
    yd <- y
  }
  
  # Pad vector with leading NAs
  yd <- c(rep(NA_real_, n), yd)
  return(yd)
}


#' @title Integrate differences of a numeric matrix ("inverse difference")
#'
#' @description This function takes a numeric matrix and integrates
#'   the differences for each column ("inverse difference").
#'
#' @param data Numeric matrix containing the original data.
#' @param data_diff Numeric matrix containing the differenced data.
#' @param n_diff Integer vector. The number of non-seasonal differences.
#' 
#' @return y_int Numeric matrix with the inverse differenced data.
#' @noRd

inv_diff_data <- function(data,
                          data_diff,
                          n_diff) {
  
  names_outputs <- colnames(data_diff)
  n_outputs <- ncol(data_diff)
  
  y_int <- lapply(
    seq_len(n_outputs),
    function(n) {
      inv_diff_vec(
        y = data,
        y_diff = data_diff[, n],
        n_diff = n_diff)
    }
  )
  
  y_int <- do.call(cbind, y_int)
  colnames(y_int) <- names_outputs
  
  return(y_int)
}


#' @title Integrate differences of a numeric vector ("inverse difference")
#'
#' @description This function takes a numeric vector and integrates
#'    (non-seasonal) differences ("inverse difference").
#'
#' @param y Numeric vector containing the original data.
#' @param y_diff Numeric vector containing the differenced data.
#' @param n_diff Integer value. The number of non-seasonal differences.
#'
#' @return y_int Numeric vector with the inverse differenced data.
#' @noRd

inv_diff_vec <- function(y,
                         y_diff,
                         n_diff) {
  
  y <- as.numeric(y)
  y_diff <- as.numeric(na.omit(y_diff))
  
  # Forecast horizon
  n_ahead <- length(y_diff)
  
  # Starting value for integration
  yi <- tail(y, n_diff)
  
  if (n_diff > 0) {
    # Integrate differenced data
    y_int <- diffinv(
      x = y_diff,
      lag = 1L,
      differences = n_diff,
      xi = yi
      )
  } else {
    # No differences
    y_int <- y_diff
  }
  
  # Cut y_int to previous length
  y_int <- y_int[c((length(y_int) - n_ahead + 1):length(y_int))]
  
  return(y_int)
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
#' @noRd

rescale_data <- function(data,
                         old_range,
                         new_range) {
  
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
    ncol = n_cols
  )
  
  max <- matrix(
    data = rep(max, each = n_rows),
    nrow = n_rows,
    ncol = n_cols
  )
  
  lower <- matrix(
    data = rep(lower, each = n_rows),
    nrow = n_rows,
    ncol = n_cols
  )
  
  upper <- matrix(
    data = rep(upper, each = n_rows),
    nrow = n_rows,
    ncol = n_cols
  )
  
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
#' @noRd

scale_data <- function(data,
                       new_range = c(-1, 1)) {
  
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
    ncol = n_cols
  )
  
  max <- matrix(
    data = rep(max, each = n_rows),
    nrow = n_rows,
    ncol = n_cols
  )
  
  lower <- matrix(
    data = rep(lower,each = n_rows),
    nrow = n_rows,
    ncol = n_cols
  )
  
  upper <- matrix(
    data = rep(upper, each = n_rows),
    nrow = n_rows,
    ncol = n_cols
  )
  
  # Scale matrix y column wise to new interval
  data <- lower + ((data - min) * (upper - lower) / (max - min))
  
  result <- list(
    data = data,
    old_range = old_range
  )
  
  return(result)
}
