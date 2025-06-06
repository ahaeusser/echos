
test_that("test methods-fable", {
  
  # Arguments
  n_ahead <- 18
  
  # Prepare train data
  train_frame <- m4_data %>%
    filter(series %in% c("M21655", "M2717"))
  
  # Train ESN model
  mable_frame <- train_frame %>%
    model("ESN" = ESN(value))
  
  # Test mable ----------------------------------------------------------------
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
  
  # Test fable ----------------------------------------------------------------
  expect_equal(is_fable(fable_frame), TRUE)
  expect_equal(nrow(fable_frame), 2*n_ahead)
})
