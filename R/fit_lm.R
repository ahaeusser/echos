
#' @title Estimate a linear model via Ordinary Least Squares
#' 
#' @description Estimate a linear model via Ordinary Least Squares.
#'
#' @param X Numeric matrix. The design matrix containing the predictor variables.
#' @param y Numeric matrix. The response variable(s).
#'
#' @return A list containing the estimated coefficients, fitted values etc.
#' @noRd

fit_lm <- function(x, y) {
  
  # Fit linear model
  model <- lm.fit(
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
  mse <- mean(resid^2)
  mae <- mean(abs(resid))
  
  metrics <- tibble(
    loglik = loglik,
    nobs = nobs,
    df = df,
    aic = aic,
    aicc = aicc,
    bic = bic,
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
