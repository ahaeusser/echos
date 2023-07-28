
# Define the test cases
test_that("estimate_dof returns correct degrees of freedom", {
  
  
  # Create (random) test data
  set.seed(42)
  n <- 100
  sigma2 <- 0.01
  beta0 <- 0
  beta1 <- 2
  x <- runif(n)
  eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
  y <- beta0 + beta1 * x + eps
  # Design matrix
  X <- cbind(1, x)
  
  # Test case 1: Basic test with lambda = 0 -----------------------------------
  expect_equal(estimate_dof(x = X, lambda = 0), ncol(X))
  
  # Test case 2: Basic test with lambda = 1 -----------------------------------
  expect_lt(estimate_dof(x = X, lambda = 1), ncol(X))
  
})


# Define the test cases
test_that("fit_ridge returns correct estimates for linear regression", {
  
  
  # Create (random) test data
  set.seed(42)
  n <- 100
  sigma2 <- 0.01
  beta0 <- 0
  beta1 <- 2
  x <- runif(n)
  eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
  y <- beta0 + beta1 * x + eps
  # Design matrix
  X <- cbind(1, x)
  
  # lm() function as benchmark
  fit_lm <- lm(y ~ x)
  coef_lm <- as.numeric(coef(fit_lm))
  fitted_lm <- as.numeric(fitted(fit_lm))
  
  # Test case 1: Basic test with lambda = 0 -----------------------------------
  model_ridge1 <- fit_ridge(x = X, y = y, lambda = 0)
  coef_ridge1 <- as.numeric(model_ridge1[["wout"]])
  fitted_ridge1 <- as.numeric(model_ridge1[["fitted"]])
  
  # Check coefficients
  expect_equal(coef_ridge1, coef_lm)
  # Check fitted values
  expect_equal(fitted_ridge1, fitted_lm)
  
  # Test case 2: Basic test with lambda = 1 -----------------------------------
  model_ridge2 <- fit_ridge(x = X, y = y, lambda = 1)
  coef_ridge2 <- as.numeric(model_ridge2[["wout"]])
  
  # Check (slope) coefficients
  expect_lt(coef_ridge2[2], coef_lm[2])
  
})