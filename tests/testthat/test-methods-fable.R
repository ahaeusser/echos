
test_that("test methods-fable with one key variable", {
  
  # Arguments
  n_ahead <- 18
  
  # Prepare train data
  train_frame <- m4_data %>%
    filter(series == "M21655")
  
  # Expected number of internal states
  n_states <- min(floor(nrow(train_frame) * 0.4), 200)
  # Expected number of coefficients (n_states plus intercept term)
  n_coeff <- n_states + 1
  # Expected number of models
  n_models <- n_states * 2
  
  # Train ESN model
  mable_frame <- train_frame %>%
    model("ESN" = ESN(value))
  
  # Test object mable ---------------------------------------------------------
  expect_equal(is_mable(mable_frame), TRUE)
  expect_equal(nrow(mable_frame), 1)
  
  # Test residuals() ----------------------------------------------------------
  resid <- mable_frame %>%
    residuals()
  
  expect_equal(is_tsibble(resid), TRUE)
  expect_equal(nrow(resid), nrow(train_frame))
  
  # Test fitted() -------------------------------------------------------------
  fitted <- mable_frame %>%
    fitted()
  
  expect_equal(is_tsibble(fitted), TRUE)
  expect_equal(nrow(fitted), nrow(train_frame))
  
  # Test tidy() ---------------------------------------------------------------
  coeffs <- mable_frame %>% 
    tidy()
  
  expect_equal(nrow(coeffs), n_coeff)
  
  # Test glance() -------------------------------------------------------------
  metrics <- mable_frame %>% 
    glance()
  
  expect_equal(nrow(metrics), n_models)
  
  # Test reservoir() ----------------------------------------------------------
  states <- mable_frame %>% 
    reservoir()
  
  expect_equal(length(unique(states[["state"]])), n_states)
  
  # Forecast ESN model
  fable_frame <- mable_frame %>%
    forecast(h = n_ahead)
  
  # Test object fable ---------------------------------------------------------
  expect_equal(is_fable(fable_frame), TRUE)
  expect_equal(nrow(fable_frame), n_ahead)
})



test_that("test methods-fable with two key variables", {
  
  # Arguments
  n_ahead <- 18
  
  # Prepare train data
  train_frame <- m4_data %>%
    filter(series %in% c("M21655", "M2717"))
  
  # Train ESN model
  mable_frame <- train_frame %>%
    model("ESN" = ESN(value))
  
  # Test object mable ---------------------------------------------------------
  expect_equal(is_mable(mable_frame), TRUE)
  expect_equal(nrow(mable_frame), 2)
  
  # Test residuals() ----------------------------------------------------------
  resid <- mable_frame %>%
    residuals()
  
  expect_equal(is_tsibble(resid), TRUE)
  expect_equal(nrow(resid), nrow(train_frame))
  
  # Test fitted() -------------------------------------------------------------
  fitted <- mable_frame %>%
    fitted()
  
  expect_equal(is_tsibble(fitted), TRUE)
  expect_equal(nrow(fitted), nrow(train_frame))
  
  # Forecast ESN model
  fable_frame <- mable_frame %>%
    forecast(h = n_ahead)
  
  # Test object fable ---------------------------------------------------------
  expect_equal(is_fable(fable_frame), TRUE)
  expect_equal(nrow(fable_frame), 2*n_ahead)
})
