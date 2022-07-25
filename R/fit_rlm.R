
#' @title Estimate a linear model via robust regression
#' 
#' @description Estimate a linear model via robust regression. 
#'   \code{fit_rlm()} is a wrapper function for \code{MASS::rlm()} with some
#'   additional output like goodness-of-fit metrics (e.g. information criteria). 
#'   The resulting object size is much smaller.
#'
#' @param X Numeric matrix. The design matrix containing the predictor variables.
#' @param y Numeric matrix. The response variable(s).
#'
#' @return A list containing the estimated coefficients, fitted values and some
#'   goodness-of-fit metrics.
#' @export

fit_rlm <- function(x, y) {
  
  # Fit linear model
  model <- rlm(
    x = x,
    y = y
  )
  
  # Extract coefficients and fitted values
  wout <- as.matrix(coef(model))
  colnames(wout) <- colnames(y)
  fitted <- fitted(model)
  
  # Calculate and store model metrics
  nobs <- nrow(x)
  df <- ncol(x) + 1
  w <- rep.int(1, nobs)
  resid <- resid(model)
  loglik <- 0.5 * (sum(log(w)) - nobs * (log(2 * pi) + 1 - log(nobs) + log(sum(w * resid^2))))
  aic <- -2 * loglik + 2 * df
  aicc <- aic + (2*df^2 + 2*df) / (nobs - df - 1)
  bic <- -2 * loglik + log(nobs) * df
  hqc <- -2 * loglik + 2 * df * log(log(nobs))
  mse <- mean(resid^2)
  mae <- mean(abs(resid))
  
  metrics <- tibble(
    loglik = loglik,
    nobs = nobs,
    df = df,
    aic = aic,
    aicc = aicc,
    bic = bic,
    hqc = hqc,
    mse = mse,
    mae = mae
  )
  
  # Store results
  list(
    wout = wout,
    fitted = fitted,
    metrics = metrics
  )
}
