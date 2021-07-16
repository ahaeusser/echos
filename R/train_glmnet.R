
#' @title Estimate a GLM via elastic net regularization
#' 
#' @description Estimate a Generalized Linear Model (GLM) via elastic net
#'   regularization.
#'
#' @param X Numeric matrix. The design matrix containing the predictor variables.
#' @param y Numeric matrix. The response variable(s).
#' @param lambda Numeric value. The regularization parameter.
#' @param type Numeric value. The elastic net mixing parameter.
#' @param weights Numeric vector. Observation weights for weighted least squares estimation.
#' @param penalty Numeric vector. Penalty factors applied to the coefficients. 
#' @param ... Further arguments passed to \code{glmnet::glmnet()}.
#'
#' @return A list containing the estimated coefficients, fitted values etc.
#' @noRd

train_glmnet <- function(X,
                         y,
                         lambda,
                         type,
                         weights,
                         penalty,
                         ...) {
  
  model_object <- glmnet(
    x = X,
    y = y,
    lambda = lambda,
    family = "gaussian",
    alpha = type,
    standardize = FALSE,
    intercept = TRUE,
    ...
  )
  
  # Extract estimated coefficients
  wout <- as.matrix(coef(model_object))

  # Calculate fitted values
  yf <- predict(
    object = model_object,
    newx = X,
    s = lambda
    )
  
  # Calculate residuals
  yr <- y - yf
  
  # Adjust column names
  colnames(wout) <- colnames(y)
  colnames(yf) <- colnames(y)
  colnames(yr) <- colnames(y)
  
  # Effective degrees of freedom
  if (type == 0) {
    # For ridge regression, the effective degrees of freedom has to be
    # estimated by the trace of the hat matrix
    dof <- estimate_dof(
      X = X,
      lambda = lambda)
  } else {
    # For LASSO, the degrees of freedom equals the number of non-zero
    # coefficients and can directly be extracted from the trained model
    dof <- model_object$df
  }

  # Number of observations
  n_obs <- nrow(y)
  
  # Determinant of the residual variance-covariance matrix
  det_sigma = det((t(yr) %*% yr)) / n_obs
  # Akaike information criterion (AIC)
  aic <- log(det_sigma) + (2 / n_obs) * dof
  # Bayesian information criterion (BIC)
  bic <- log(det_sigma) + (log(n_obs) / n_obs) * dof
  # Hannan-Quinn information criterion (HQ)
  hq <- log(det_sigma) + (2 * log(log(n_obs)) / n_obs) * dof
  
  list(
    model_object = model_object,
    wout = wout,
    yf = yf,
    yr = yr,
    dof = dof,
    det_sigma = det_sigma,
    aic = aic,
    bic = bic,
    hq = hq
  )
}


#' @title Estimate effective degrees of freedom
#' 
#' @description The function estimates the effective degrees of freedom.
#'
#' @param X Numeric matrix. The design matrix containing the predictor variables.
#' @param lambda Numeric value. The regularization parameter.
#'
#' @return Numeric value.
#' @noRd

estimate_dof <- function(X, lambda) {
  
  # Diagonal matrix with lambda values
  Ipp_lambda <- diag(
    x = lambda,
    nrow = ncol(X),
    ncol = ncol(X)
    )
  
  # Calculate effective degrees of freedom
  sum(diag((X %*% solve(crossprod(X) + Ipp_lambda)) %*% t(X)))
}
