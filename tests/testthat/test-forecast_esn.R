
test_that("forecast_esn function works correctly", {
  
  # Define arguments
  n_ahead <- 18
  n_sim <- 200
  levels <- c(80, 95)
  
  # Prepare data
  n_obs <- length(AirPassengers)
  n_train <- n_obs - n_ahead
  ytrain <- AirPassengers[(1:n_train)]
  ytest <- AirPassengers[((n_train+1):n_obs)]
  
  # Train and forecast ESN model
  esn_model <- train_esn(ytrain)
  
  esn_fcst <- forecast_esn(
    object = esn_model,
    n_ahead = n_ahead,
    n_sim = n_sim,
    levels = levels
  )
  
  # Test if the function output is of class "forecast_esn"
  expect_true(class(esn_fcst) == "forecast_esn")
  expect_true(is.forecast_esn(esn_fcst))
  
  # Test object lengths and dimensions
  expect_equal(length(esn_fcst$point), n_ahead)
  expect_equal(length(esn_fcst$std), n_ahead)
  expect_equal(dim(esn_fcst$sim), c(n_ahead, n_sim))
  expect_equal(dim(esn_fcst$interval), c(n_ahead, 2*length(esn_fcst$levels)))
})
