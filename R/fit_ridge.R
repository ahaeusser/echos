
#' @title Estimate a linear model via ridge regression
#' 
#' @description Estimate a linear model via Ordinary Least Squares (OLS). 
#'   \code{fit_lm()} is a wrapper function for \code{stats::lm.fit()} with some
#'   additional output like goodness-of-fit metrics (e.g. information criteria). 
#'   The function is slightly faster than \code{stats::lm()}, bit most 
#'   importantly, the resulting object size is much smaller.
#'
#' @param X Numeric matrix. The design matrix containing the predictor variables.
#' @param y Numeric matrix. The response variable(s).
#'
#' @return A list containing the estimated coefficients, fitted values and some
#'   goodness-of-fit metrics.
#' @export

fit_ridge <- function(x, y, lambda) {
  
  lpp <- diag(lambda, ncol(x))
  lpp[1, 1] <- 0
  
  # Estimate coefficients of linear model
  wout <- solve(crossprod(x) + lpp, crossprod(x, y))
  colnames(wout) <- colnames(y)
  
  # Calculate fitted values and residuals
  fitted <- x %*% wout
  resid <- y - fitted
  
  # Calculate and store model metrics
  nobs <- nrow(x)
  df <- estimate_dof(x = x, lambda = lambda)
  w <- rep.int(1, nobs)
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
    lambda = lambda,
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


#' @title Estimate effective degrees of freedom
#' 
#' @description The function estimates the effective degrees of freedom.
#'
#' @param x Numeric matrix. The design matrix containing the predictor variables.
#' @param lambda Numeric value. The regularization parameter.
#'
#' @return Numeric value.
#' @noRd

estimate_dof <- function(x, lambda) {
  
  # Diagonal matrix with lambda values
  Ipp_lambda <- diag(
    x = lambda,
    nrow = ncol(x),
    ncol = ncol(x)
  )
  
  # Calculate effective degrees of freedom
  sum(diag((x %*% solve(crossprod(x) + Ipp_lambda)) %*% t(x)))
}

