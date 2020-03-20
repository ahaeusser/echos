# Function to do the work.


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