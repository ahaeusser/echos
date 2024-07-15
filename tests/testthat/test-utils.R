
# Define the test cases
test_that("create_lags returns correct lagged variables", {
  
  # Test case 1: Basic test with one lag --------------------------------------
  y1 <- 1:5
  lags1 <- 1
  
  expected_output1 <- matrix(
    data = c(NA, 1, 2, 3, 4),
    nrow = 5,
    ncol = 1,
    dimnames = list(NULL, c("lag(1)"))
  )
  
  expect_equal(create_lags(y1, lags1), expected_output1)
  
  # Test case 2: Multiple lags ------------------------------------------------
  y2 <- 5:1
  lags2 <- c(1, 2)
  
  expected_output2 <- matrix(
    data = c(NA, 5, 4, 3, 2, NA, NA, 5, 4, 3),
    nrow = 5,
    ncol = 2,
    dimnames = list(NULL, c("lag(1)", "lag(2)"))
  )
  
  expect_equal(create_lags(y2, lags2), expected_output2)
  
  # Test case 3: Zero lag -----------------------------------------------------
  y3 <- c(2.5, 4.2, 6.7)
  lags3 <- 0
  
  expected_output3 <- matrix(
    data = c(2.5, 4.2, 6.7),
    nrow = 3,
    ncol = 1,
    dimnames = list(NULL, c("lag(0)"))
  )
  
  expect_equal(create_lags(y3, lags3), expected_output3)
  
  # Test case 4: Empty input --------------------------------------------------
  y4 <- numeric(0)
  lags4 <- c(1, 2)
  
  expected_output4 <- matrix(
    data = numeric(0),
    nrow = 0,
    ncol = 2,
    dimnames = list(NULL, c("lag(1)", "lag(2)"))
  )
  
  expect_equal(create_lags(y4, lags4), expected_output4)
})



# Define the test cases
test_that("create_revolved returns correct revolved variables", {
  
  # Test case 1: Basic test with one lag and one step ahead -------------------
  y1 <- 1:5
  lags1 <- 1
  n_ahead1 <- 1
  
  expected_output1 <- matrix(
    data = c(0, 5, NA_real_),
    nrow = 3,
    ncol = 1,
    dimnames = list(NULL, c("lag(1)"))
  )
  expect_equal(create_revolved(y1, lags1, n_ahead1), expected_output1)
  
  # Test case 2: Multiple lags and multiple steps ahead -----------------------
  y2 <- 5:1
  lags2 <- c(1, 2)
  n_ahead2 <- 2
  
  expected_output2 <- matrix(
    data = c(0, 1, NA_real_, NA_real_, NA_real_, 0, 2, 1, NA_real_, NA_real_),
    nrow = 5,
    ncol = 2,
    dimnames = list(NULL, c("lag(1)", "lag(2)"))
  )
  
  expect_equal(create_revolved(y2, lags2, n_ahead2), expected_output2)
  
  # Test case 3: Zero lag and zero steps ahead --------------------------------
  y3 <- c(2.5, 4.2, 6.7)
  lags3 <- 0
  n_ahead3 <- 0
  
  expected_output3 <- matrix(
    data = 0,
    nrow = 1,
    ncol = 1,
    dimnames = list(NULL, c("lag(0)"))
  )
  
  expect_equal(create_revolved(y3, lags3, n_ahead3), expected_output3)
  
  # Test case 4: Empty input --------------------------------------------------
  y4 <- numeric(0)
  lags4 <- c(1, 2)
  n_ahead4 <- 2
  
  expected_output4 <- matrix(
    data = c(0, NA_real_, NA_real_, NA_real_, NA_real_, 0, NA_real_, NA_real_, NA_real_, NA_real_),
    nrow = 5,
    ncol = 2,
    dimnames = list(NULL, c("lag(1)", "lag(2)"))
  )
  
  expect_equal(create_revolved(y4, lags4, n_ahead4), expected_output4)
})



# Define the test cases
test_that("create_win returns matrix with correct dimensions", {
  # Set a random seed for reproducibility
  set.seed(123)
  
  # Test case 1: Basic test with small inputs ---------------------------------
  n_inputs1 <- 2
  n_states1 <- 3
  scale_runif1 <- c(0, 1)
  output1 <- create_win(n_inputs1, n_states1, scale_runif1)
  expect_equal(dim(output1), c(n_states1, n_inputs1))
  
  # Test case 2: Large inputs -------------------------------------------------
  n_inputs2 <- 5
  n_states2 <- 10
  scale_runif2 <- c(-1, 1)
  output2 <- create_win(n_inputs2, n_states2, scale_runif2)
  expect_equal(dim(output2), c(n_states2, n_inputs2))
  
  # Test case 3: Zero inputs and states ---------------------------------------
  n_inputs3 <- 0
  n_states3 <- 0
  scale_runif3 <- c(0, 0)
  output3 <- create_win(n_inputs3, n_states3, scale_runif3)
  expect_equal(dim(output3), c(n_states3, n_inputs3))
})



# Define the test cases
test_that("create_wres returns correct reservoir weight matrix", {
  
  # Test case 1: Basic test with small states and density ---------------------
  n_states1 <- 5
  rho1 <- 0.9
  density1 <- 0.3
  scale_runif1 <- c(-1, 1)
  symmetric1 <- FALSE
  output1 <- create_wres(n_states1, rho1, density1, scale_runif1, symmetric1)
  
  # Check the dimensions of the output matrix
  expect_equal(dim(output1), c(n_states1, n_states1))
  
  # Test case 2: Large states and density with symmetric matrix ---------------
  n_states2 <- 10
  rho2 <- 0.95
  density2 <- 0.6
  scale_runif2 <- c(0, 0.5)
  symmetric2 <- TRUE
  output2 <- create_wres(n_states2, rho2, density2, scale_runif2, symmetric2)
  
  # Check the dimensions of the output matrix
  expect_equal(dim(output2), c(n_states2, n_states2))
})



# Define the test cases
test_that("paste_names returns correct names", {
  
  # Test case 1: Basic test with single character x and multiple n ------------
  x1 <- "A"
  n1 <- 5
  expected_output1 <- c("A(1)", "A(2)", "A(3)", "A(4)", "A(5)")
  expect_equal(paste_names(x1, n1), expected_output1)
  
  # Test case 2: Multi-character x and multiple n -----------------------------
  x2 <- "name"
  n2 <- 10
  expected_output2 <- c(
    "name(01)", "name(02)", "name(03)", "name(04)", "name(05)",
    "name(06)", "name(07)", "name(08)", "name(09)", "name(10)"
  )
  expect_equal(paste_names(x2, n2), expected_output2)
  
  # Test case 3: Single character x and single n ------------------------------
  x3 <- "X"
  n3 <- 1
  expected_output3 <- "X(1)"
  expect_equal(paste_names(x3, n3), expected_output3)
  
  # Test case 4: Empty string x and multiple n --------------------------------
  x4 <- ""
  n4 <- 2
  expected_output4 <- c("(1)", "(2)")
  expect_equal(paste_names(x4, n4), expected_output4)
})



# Define the test cases
test_that("diff_vec returns correct vector with differences", {
  
  # Test case 1: Basic test with small vector and n_diff = 1 ------------------
  y1 <- c(3, 8, 2, 10, 5)
  n_diff1 <- 1
  expected_output1 <- c(NA_real_, 5, -6, 8, -5)
  
  expect_equal(diff_vec(y1, n_diff1), expected_output1)
  
  # Test case 2: Large vector and n_diff = 2 ----------------------------------
  y2 <- 1:10
  n_diff2 <- 2
  expected_output2 <- c(NA_real_, NA_real_, 0, 0, 0, 0, 0, 0, 0, 0)
  
  expect_equal(diff_vec(y2, n_diff2), expected_output2)
  
  # Test case 3: Empty vector and n_diff = 3 ----------------------------------
  y3 <- numeric(0)
  n_diff3 <- 3
  expected_output3 <- c(NA_real_, NA_real_, NA_real_)
  
  expect_equal(diff_vec(y3, n_diff3), expected_output3)
})



# Define the test cases
test_that("inv_diff_vec returns correct inverse differenced vector", {
  
  # Test case 1: Basic test with n_diff = 1
  y1 <- c(3, 8, 2, 10, 5, 8, 4, 9, 10, 12)
  yd1 <- diff(y1, differences = 1, lag = 1L)
  
  ytrain1 <- head(y1, 6)
  yfc1 <- tail(yd1, 4)
  n_diff1 <- 1
  
  expected_output1 <- c(4, 9, 10, 12)
  
  expect_equal(inv_diff_vec(ytrain1, yfc1, n_diff1), expected_output1)
  
  # Test case 2: Basic test with n_diff = 2 -----------------------------------
  y2 <- c(5, 8, 10, 3, 12, 9, 2, 8, 6, 4, 3, 8)
  yd2 <- diff(y2, differences = 2, lag = 1L)
  
  ytrain2 <- head(y2, 6)
  yfc2 <- tail(yd2, 6)
  n_diff2 <- 2
  
  expected_output2 <- c(2, 8, 6, 4, 3, 8)
  
  expect_equal(inv_diff_vec(ytrain2, yfc2, n_diff2), expected_output2)
})



# Define the test cases
test_that("scale_vec returns correct scaled vector and old range", {
  
  # Test case 1: Basic test with default new_range ----------------------------
  y1 <- c(3, 8, 2, 10, 5)
  
  expected_output1 <- list(
    ys = c(-0.75, 0.50, -1.00, 1.00, -0.25),
    old_range = c(2, 10)
  )
  expect_equal(scale_vec(y1), expected_output1)
  
  # Test case 2: Basic test with custom new_range -----------------------------
  y2 <- c(2, 8, 2, 10, 12)
  new_range2 <- c(0, 1)
  expected_output2 <- list(
    ys = c(0.0, 0.6, 0.0, 0.8, 1.0),
    old_range = c(2, 12)
  )
  expect_equal(scale_vec(y2, new_range2), expected_output2)
})



# Define the test cases
test_that("scale_vec returns correct scaled vector and old range", {
  
  # Test case 1: Basic test with default new_range ----------------------------
  ys1 <- c(-0.75, 0.50, -1.00, 1.00, -0.25)
  old_range1 <- c(2, 10)
  new_range1 <- c(-1, 1)
  expected_output1 <- c(3, 8, 2, 10, 5)
  
  expect_equal(rescale_vec(ys1, old_range1, new_range1), expected_output1)
  
  # Test case 2: Basic test with custom new_range -----------------------------
  ys2 <- c(0.0, 0.6, 0.0, 0.8, 1.0)
  old_range2 <- c(2, 12)
  new_range2 <- c(0, 1)
  expected_output2 <- c(2, 8, 2, 10, 12)
  
  expect_equal(rescale_vec(ys2, old_range2, new_range2), expected_output2)
  
})
