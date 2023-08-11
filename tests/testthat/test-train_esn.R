
test_that("train_esn function works correctly", {
  
  # Create test data
  n_ahead <- 18
  n_obs <- length(AirPassengers)
  n_train <- n_obs - n_ahead
  ytrain <- AirPassengers[(1:n_train)]
  ytest <- AirPassengers[((n_train+1):n_obs)]
  
  # Test the function call
  esn_model <- train_esn(ytrain)
  
  # Test if the function output is of class "esn"
  expect_true(class(esn_model) == "esn")
  
  # Test if the residuals vector has the same length as the input vector
  expect_equal(length(esn_model$actual), length(ytrain))
  
  # Test if the residuals vector has the same length as the input vector
  expect_equal(length(esn_model$fitted), length(ytrain))
  
  # Test if the residuals vector has the same length as the input vector
  expect_equal(length(esn_model$resid), length(ytrain))
  
  # Test if the actual and fitted vectors have the same length
  expect_equal(length(esn_model$actual), length(esn_model$fitted))
  
  # Test if the actual and fitted vectors have the same length
  expect_equal(length(esn_model$resid), length(esn_model$fitted))
  
  # Test if the model_weights list contains the expected elements
  expect_equal(names(esn_model$method$model_weights), c("win", "wres", "wout"))
  
  # Test if n_states is as expected
  expect_equal(esn_model$method$model_layers$n_states, 50)
  
  # Test of n_models is as expected
  expect_equal(esn_model$method$model_meta$n_models, 100)
  
  # Add more tests as needed...
})
