
#' @title Create lagged variables of a numeric vector
#' 
#' @description Create lagged variables of a numeric vector, shifting each 
#'   column back by a given number of observations.
#' 
#' @param y Numeric vector with input data.
#' @param lags Integer vector with the number of lags (in units of observations).
#' 
#' @return ylag Numeric matrix with lagged variables of the input vector.
#' @noRd

create_lags <- function(y, lags) {
  
  # Number of observations
  n_obs <- length(y)
  # Number of lags
  n_lags <- length(lags)
  
  # Preallocate empty matrix for lagged variables
  ylag <- matrix(
    data = 0,
    nrow = n_obs,
    ncol = n_lags
  )
  
  # Lag variables
  for (i in seq_along(lags)) {
    ylag[, i] <- c(rep(NA_real_, lags[i]), y)[1:length(y)]
  }
  
  colnames(ylag) <- paste0(
    rep("lag", times = length(lags)),
    "(",
    unlist(lags),
    ")")
  
  return(ylag)
}



#' @title Create lagged variables of a numeric vector for iterative forecasting
#' 
#' @description Create lagged variables of a numeric vector for iterative 
#'   forecasting, shifting each column back by a given number of observations 
#'   and fill with NAs for the updates.
#' 
#' @param y Numeric vector with input data.
#' @param lags Integer vector with the number of lags (in units of observations).
#' @param n_ahead Integer value. The forecast horizon (n-step ahead).
#' 
#' @return ylag Numeric matrix with the lagged variables of the input data for iterative forecasting.
#' @noRd

create_revolved <- function(y,
                            lags,
                            n_ahead) {
  
  # Number of rows
  n_rows <- (max(lags) + n_ahead + 1)
  # Number of columns
  n_lags <- length(lags)
  
  # Preallocate empty output matrix for lagged variables
  ylag <- matrix(
    data = NA_real_,
    nrow = n_rows,
    ncol = n_lags
  )
  
  # Lag variables
  for (i in seq_along(lags)) {
    lag <- c(0, y[c((length(y) - lags[i] + 1):length(y))])
    length(lag) <- n_rows
    ylag[, i] <- lag
  }
  
  colnames(ylag) <- paste0(
    rep("lag", times = length(lags)),
    "(",
    unlist(lags),
    ")")
  
  return(ylag)
}



#' @title Create the input weight matrix
#' 
#' @description Create the random input weight matrix.
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



#' @title Create a random binary matrix with a given density
#' 
#' @description Create a random binary matrix with a given density.
#'
#' @param n_row Positive integer. The number of rows.
#' @param n_col Positive integer. The number of columns.
#' @param density Numeric value. The density is the proportion of non-zero elements. A number between zero and one. 
#'
#' @return m Integer matrix.
#' @noRd

random_matrix <- function(n_row,
                          n_col,
                          density = 0.5) {
  
  # Argument handling ---------------------------------------------------------
  if (!is.numeric(n_row) || n_row <= 0 || n_row %% 1 != 0)
    stop("n_row must be a positive integer")
  if (!is.numeric(n_col) || n_col <= 0 || n_col %% 1 != 0)
    stop("n_col must be a positive integer")
  if (!is.numeric(density) || density < 0 || density > 1)
    stop("density must be a number between 0 and 1")
  
  n_row <- as.integer(n_row)
  n_col <- as.integer(n_col)
  
  # Determine the number of non-zero elements (nnz) ---------------------------
  n_total <- n_row * n_col
  nnz     <- round(n_total * density)
  nnz     <- max(0L, min(nnz, n_total))
  
  # Create matrix -------------------------------------------------------------
  m <- matrix(
    data = 0L,
    nrow = n_row,
    ncol = n_col
  )
  
  if (nnz > 0) {
    idx <- sample.int(n_total, nnz, replace = FALSE)
    m[idx] <- 1L
  }
  return(m)
}



#' @title Create the reservoir weight matrix
#' 
#' @description Create the random reservoir weight matrix (scaled to spectral 
#'   radius rho).
#' 
#' @param n_states Integer value. The number of internal states within the reservoir (reservoir size).
#' @param rho Numeric value. The spectral radius for scaling the weight matrix.
#' @param density Numeric value. The parameter defines the connectivity of the reservoir weight matrix (dense or sparse).
#' @param scale_runif Numeric vector. The lower and upper bound of the uniform distribution.
#' 
#' @return wres Numeric matrix. The final reservoir weight matrix.
#' @noRd

create_wres <- function(n_states,
                        rho,
                        density,
                        scale_runif) {
  
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
  wsparse <- random_matrix(
    n_row = n_states,
    n_col = n_states,
    density = density
  )
  
  wres <- as.matrix(wres * wsparse)
  
  # Calculate the absolute, maximum eigenvalue of the reservoir weight matrix
  eig <- eigen(wres, symmetric = FALSE, only.values = TRUE)$values
  max_abs_eig <- max(abs(eig))
  
  # Rescale the reservoir weight matrix to spectral radius rho
  wres <- 1 / max_abs_eig * wres * rho
  return(wres)
}



#' @title Helper function to concatenate a string and sequential integers.
#' 
#' @description Concatenate a string \code{x} and sequential integers up 
#'   to \code{n} (e.g. test(1), test(2), ..., test(n)).
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



#' @title Calculate the nth-difference of a numeric vector
#'
#' @description Takes a numeric vector as input and calculates
#'    the nth-difference.
#'
#' @param y Numeric vector.
#' @param n_diff Integer value. The number of differences.
#'
#' @return yd Numeric vector with the differenced data.
#' @noRd

diff_vec <- function(y, n_diff) {
  
  # Calculate n-th difference
  if (n_diff > 0) {
    yd <- diff(
      x = y,
      differences = n_diff,
      lag = 1L
      )
  } else {
    yd <- y
  }
  
  # Pad vector with leading NAs
  yd <- c(rep(NA_real_, n_diff), yd)
  return(yd)
}



#' @title Integrate differences of a numeric vector ("inverse difference")
#'
#' @description Takes a numeric vector as input and integrates
#'    (non-seasonal) differences ("inverse difference").
#'
#' @param y Numeric vector containing the original data.
#' @param yd Numeric vector containing the differenced data.
#' @param n_diff Integer value. The number of (non-seasonal) differences.
#'
#' @return y_int Numeric vector with the inverse differenced data.
#' @noRd

inv_diff_vec <- function(y,
                         yd,
                         n_diff) {
  
  y <- as.numeric(y)
  yd <- as.numeric(na.omit(yd))
  
  # Forecast horizon
  n_ahead <- length(yd)
  
  # Starting value for integration
  yi <- tail(y, n_diff)
  
  if (n_diff > 0) {
    # Integrate differenced data
    y_int <- diffinv(
      x = yd,
      lag = 1L,
      differences = n_diff,
      xi = yi
      )
  } else {
    # No differences
    y_int <- yd
  }
  
  # Cut y_int to previous length
  y_int <- y_int[c((length(y_int) - n_ahead + 1):length(y_int))]
  
  return(y_int)
}



#' @title Scale a numeric vector
#' 
#' @description Scale a numeric vector to a specific interval.
#' 
#' @param y Numeric vector containing the data to be scaled.
#' @param new_range Numeric vector. The range for scaling (first value represents the replacement for the min value, the second is the substitute for the max value).
#' 
#' @return result List with numeric vector (\code{ys}) scaled to the interval and the old range as reference.
#' @noRd

scale_vec <- function(y,
                      new_range = c(-1, 1)) {
  
  # Number of observations
  n_obs <- length(y)
  
  # Minimum and maximum
  min <- min(y, na.rm = TRUE)
  max <- max(y, na.rm = TRUE)
  old_range <- c(min, max)
  
  # Extract the lower and upper bound from interval
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Scale matrix y column wise to new interval
  ys <- lower + ((y - min) * (upper - lower) / (max - min))
  
  result <- list(
    ys = ys,
    old_range = old_range
  )
  
  return(result)
}



#' @title Rescale (inverse scaling) a numeric vector
#' 
#' @description Rescale (inverse scaling) a numeric vector by applying the 
#'   transformation backwards to original range.
#' 
#' @param ys Numeric vector containing the values to be rescaled.
#' @param old_range Numeric vector with ranges (min and max) of original data.
#' @param new_range Numeric vector with new (scaled) interval.
#' 
#' @return y Numeric vector with rescaled values.
#' @noRd

rescale_vec <- function(ys,
                        old_range,
                        new_range) {
  
  # Number of observations
  n_obs <- length(ys)
  
  # Extract maximum and minimum for inverse scaling
  min <- old_range[1]
  max <- old_range[2]
  
  # Extract lower and upper bound
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Inverse normalization to original interval
  y <- ((ys - lower) * (max - min)) / (upper - lower) + min
  
  return(y)
}


#' @title Moving block bootstrap
#' 
#' @description Creates a moving block bootstrap of a numeric vector.
#' 
#' @param yr Numeric vector containing the values to be bootstrapped.
#' @param n_ahead Integer value. The number of periods for forecasting (i.e. forecast horizon).
#' @param n_sim Integer value. The number of future sample path generated during simulation.
#' @param n_size Integer value. The size of each block.
#' 
#' @return boot_matrix Numeric matrix with n_sim rows and n_ahead columns.
#' @noRd

moving_block <- function(x, n_ahead, n_sim, n_size) {
  
  if (!is.numeric(x)) stop("Input 'x' must be a numeric vector.")
  if (n_size > length(x)) stop("Block size cannot be larger than the input vector.")
  
  # Number of overlapping blocks available
  n_blocks <- length(x) - n_size + 1
  
  # Create a list of overlapping blocks
  blocks <- lapply(1:n_blocks, function(i) x[i:(i + n_size - 1)])
  
  # Preallocate empty matrix to store blocks
  boot_matrix <- matrix(
    data = NA_real_,
    nrow = n_sim,
    ncol = n_ahead
  )
  
  for (i in 1:n_sim) {
    # Select enough blocks to cover n_ahead
    n_blocks_needed <- ceiling(n_ahead / n_size)
    
    sampled_blocks <- sample(blocks, size = n_blocks_needed, replace = TRUE)
    boot_series <- unlist(sampled_blocks)[1:n_ahead]
    
    boot_matrix[i, ] <- boot_series
  }
  
  return(boot_matrix)
}




#' @title KPSS unit root test
#' 
#' @description Performs the KPSS unit root test, where the null hypothesis is 
#'    stationarity. The test type specifies the deterministic component either 
#'    as constant \code{"mu"} or a constant plus linear trend \code{"tau"}.
#'
#' @param y Numeric vector containing the response variable.
#' @param type Test type, where \code{"mu"} is a constant and \code{"tau"} is constant plus linear trend.
#' @param alpha Significance level of the test. \code{alpha = c(0.10, 0.05, 0.025, 0.01)}.
#'
#' @return A \code{list} containing:
#'    \itemize{
#'       \item{\code{stat}: Numeric value. The KPSS test statistic.}
#'       \item{\code{crit}: Numeric value. The critical value for a specific significance level \code{alpha}.}
#'       \item{\code{reject}: Logical value. If \code{TRUE}, the null hypothesis (= stationarity) is rejected, i.e., \code{y} is non-stationary.}
#'       \item{\code{alpha}: Numeric value. The significance level of the test.}
#'       \item{\code{type}: Character vector. The type of the test, either \code{"mu"} with a constant or \code{"tau"} with constant plus linear trend.}
#'       }
#' @noRd

test_kpss <- function(y,
                      type  = c("mu", "tau"),
                      alpha = 0.05) {
  
  # Argument checks -----------------------------------------------------------
  stopifnot(is.numeric(y), length(y) > 2)
  type  <- match.arg(type)
  alpha <- match.arg(
    as.character(alpha),
    c("0.1", "0.05", "0.025", "0.01"))
  alpha_num <- as.numeric(alpha)
  
  n <- length(y)
  t <- seq_len(n)
  
  # Demean ("mu") or detrend ("tau") ------------------------------------------
  
  if (type == "mu") {
    e <- y - mean(y)
  } else {
    e <- residuals(lm(y ~ t))
  }
  
  # Truncation lag (Bartlett) -------------------------------------------------
  q <- max(1L, floor(3 * sqrt(n) / 13))
  
  # Long-run variance  (Newey–West) -------------------------------------------
  gamma0 <- sum(e^2) / n
  
  if (q > 0) {
    
    # Pre-allocate a numeric vector of length q
    gammas <- numeric(q)
    # Sample autocovariance
    for (h in 1:q) {
      gammas[h] <- sum(e[(h + 1):n] * e[1:(n - h)]) / n
    }
    
    weights <- 1 - (1:q) / (q + 1)
    sigma2  <- gamma0 + 2 * sum(weights * gammas)
  } else {
    sigma2 <- gamma0
  }
  
  # KPSS test statistic -------------------------------------------------------
  S      <- cumsum(e)
  eta_sq <- sum(S^2) / n^2
  stat   <- eta_sq / sigma2
  
  # Critical values -----------------------------------------------------------
  crit_vals <- list(
    mu  = c("0.1" = 0.347, "0.05" = 0.463, "0.025" = 0.574, "0.01" = 0.739),
    tau = c("0.1" = 0.119, "0.05" = 0.146, "0.025" = 0.176, "0.01" = 0.216)
  )
  
  # Extract critical value
  crit <- crit_vals[[type]][[alpha]]
  
  # Decision: compare test statistic and critical value
  reject <- stat > crit
  
  # Store results -------------------------------------------------------------
  test <- list(
    stat = stat,
    crit = crit,
    reject = reject,
    alpha = alpha_num,
    type = type
  )
  
  return(test)
}



#' @title Estimate the number of differences required to achieve stationarity
#' 
#' @description Iteratively differences a univariate time series and applies
#'   \code{test_kpss} at each step until the null hypothesis of stationarity 
#'   can no longer be rejected. 
#'
#' @param y Numeric vector containing the response variable.
#' @param max_diff Integer value. The maximum number of differences to check.
#' @param type Test type, where \code{"mu"} is a constant and \code{"tau"} is constant plus linear trend.
#' @param alpha Significance level of the test. \code{alpha = c(0.10, 0.05, 0.025, 0.01)}.
#'
#' @return n_diff Integer value. The number of differences required to achieve stationarity.
#' @noRd

estimate_ndiff <- function(y,
                           max_diff = 1,
                           alpha = 0.05,
                           type  = c("mu", "tau")) {
  # Argument handling
  type <- match.arg(type)
  
  for (n_diff in 0:max_diff) {
    
    if (n_diff == 0) {
      yd <- y
    } else {
      yd <- diff(y, differences = n_diff)
    }
    
    # Time series too short for reliable test
    if (length(yd) < 3L)
      break
    
    # Conduct KPSS test
    test <- test_kpss(
      y = yd,
      type = type,
      alpha = alpha
    )
    
    # Stationarity not rejected
    if (!test$reject) {
      return(n_diff)
    }
  }
}


