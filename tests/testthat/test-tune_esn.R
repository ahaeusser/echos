
test_that("tune_esn function works correctly", {
  
  # Test data
  y <- as.numeric(AirPassengers)
  n_ahead <- 12
  n_split <- 5
  alpha <- seq(0.1, 1.0, 0.1)
  rho <- c(1.0)
  tau <- c(0.4)
  
  # Test the function call
  esn_pars <- tune_esn(
    y = y,
    n_ahead = n_ahead,
    n_split = n_split,
    alpha = alpha,
    rho = rho,
    tau = tau
  )
  
  # Test object lengths and dimensions
  expect_equal(ncol(esn_pars), 10)
  expect_equal(nrow(esn_pars), n_split * length(alpha) * length(rho) * length(tau))
})
