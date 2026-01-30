
test_that("train_esn returns an esn object", {
  xdata <- as.numeric(AirPassengers)
  xmodel <- train_esn(y = xdata)
  expect_true(is.esn(xmodel))
  expect_s3_class(xmodel, "esn")
})

test_that("forecast_esn returns a forecast_esn object", {
  xdata <- as.numeric(AirPassengers)
  xmodel <- train_esn(y = xdata)
  xfcst <- forecast_esn(xmodel, n_ahead = 12)
  expect_true(is.forecast_esn(xfcst))
  expect_s3_class(xfcst, "forecast_esn")
})

test_that("tune_esn returns a tune_esn object", {

  # Test data
  y <- as.numeric(AirPassengers)
  n_ahead <- 12
  n_split <- 5
  alpha <- seq(1.0, 0.1)
  rho <- c(1.0)
  tau <- c(0.4)
  
  # Test the function call
  xfit <- tune_esn(
    y = y,
    n_ahead = n_ahead,
    n_split = n_split,
    alpha = alpha,
    rho = rho,
    tau = tau
  )

  expect_true(is.tune_esn(xfit))
  expect_s3_class(xfit, "tune_esn")
})
