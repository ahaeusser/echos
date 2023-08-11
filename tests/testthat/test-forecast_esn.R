
test_that("forecast_esn function works correctly", {
  
  # Create test data
  n_ahead <- 18
  n_obs <- length(AirPassengers)
  n_train <- n_obs - n_ahead
  ytrain <- AirPassengers[(1:n_train)]
  ytest <- AirPassengers[((n_train+1):n_obs)]
  
  # Test the function call
  esn_model <- train_esn(ytrain)
  esn_fcst <- forecast_esn(esn_model)
  
  # Test if the function output is of class "esn"
  expect_true(class(esn_fcst) == "forecast_esn")
  
  # Test if the residuals vector has the same length as the input vector
  expect_equal(length(esn_fcst$point), n_ahead)
  
  # Add more tests as needed...
})
