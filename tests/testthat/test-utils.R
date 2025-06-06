
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


test_that("matrix dimensions, type and values are correct", {
  m <- random_matrix(10, 10, density = 0.5)
  
  # correct dimensions & integer mode
  expect_equal(dim(m), c(10, 10))
  expect_true(is.integer(m))
  
  # only 0 / 1 values
  expect_true(all(m %in% c(0L, 1L)))
  
  # correct number of ones (rounded)
  expected_nnz <- 50
  expect_equal(sum(m), expected_nnz)
})


test_that("density edge cases (0 and 1) work", {
  m0 <- random_matrix(4, 4, density = 0)
  expect_equal(sum(m0), 0)
  
  m1 <- random_matrix(4, 4, density = 1)
  expect_equal(sum(m1), 16)          # 4 × 4
  expect_true(all(m1 == 1L))
})


test_that("large matrices respect density within tolerance", {
  # Deterministic but we only check the count, not positions
  set.seed(42)
  n_row <- 100; n_col <- 80; dens <- 0.27
  m <- random_matrix(n_row, n_col, dens)
  
  expect_equal(sum(m), round(n_row * n_col * dens))
})


test_that("invalid inputs trigger errors", {
  expect_error(random_matrix(0, 5))
  expect_error(random_matrix(-3, 5))
  expect_error(random_matrix(5.7, 5))
  
  expect_error(random_matrix(5, 0))
  expect_error(random_matrix(5, -2))
  expect_error(random_matrix(5, 4.2))
  
  expect_error(random_matrix(5, 5, density = -0.1))
  expect_error(random_matrix(5, 5, density = 1.1))
  expect_error(random_matrix(5, 5, density = "high"))
})


test_that("create_wres returns correct reservoir weight matrix", {
  
  # Basic test
  n_states <- 5
  rho <- 0.9
  density <- 0.3
  scale_runif <- c(-1, 1)
  output <- create_wres(n_states, rho, density, scale_runif)
  
  # Check the dimensions of the output matrix
  expect_equal(dim(output), c(n_states, n_states))
})


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


test_that("test_kpss() returns list with expected fields", {
  set.seed(1)
  res <- test_kpss(rnorm(40), type = "mu", alpha = 0.05)
  
  # class / type
  expect_type(res, "list")
  
  # required names present (order not important)
  expect_named(
    res,
    expected = c("stat", "crit", "reject", "alpha", "type"),
    ignore.order = TRUE
  )
  
  # individual element checks
  expect_type(res$stat,  "double")
  expect_type(res$crit,  "double")
  expect_type(res$reject,"logical")
  expect_equal(res$alpha, 0.05)
  expect_match(res$type, "mu|tau")
})


test_that("stationary white-noise series is (usually) NOT rejected", {
  # Create data (stationary white noise process, i.i.d. N(0, 1))
  set.seed(123)
  wn  <- rnorm(200)
  
  # Run KPSS test
  res <- test_kpss(wn, type = "mu", alpha = 0.05)
  
  expect_lt(res$stat, res$crit)  # statistic below critical value
  expect_false(res$reject)       # do not reject stationarity
})


test_that("random walk is rejected as non-stationary", {
  # Create data (non-stationary random walk)
  set.seed(456)
  rw  <- cumsum(rnorm(200))
  
  # Run KPSS test
  res <- test_kpss(rw, type = "mu", alpha = 0.05)
  
  expect_gt(res$stat, res$crit)  # statistic above critical value
  expect_true(res$reject)        # reject stationarity
})


test_that("random walk with trend is rejected by the tau test", {
  # Create data (random walk with trend)
  set.seed(789)
  rw_trend <- 0.5 * (1:300) + cumsum(rnorm(300))
  
  # Run KPSS test
  res <- test_kpss(rw_trend, type = "tau", alpha = 0.025)
  
  expect_true(res$reject)
})


test_that("critical values change with type and alpha", {
  # Create data (random normal)
  dummy <- rnorm(50)
  
  # Run KPSS tests
  c_mu_10   <- test_kpss(dummy, type = "mu",  alpha = 0.10)$crit
  c_mu_005  <- test_kpss(dummy, type = "mu",  alpha = 0.05)$crit
  c_tau_10  <- test_kpss(dummy, type = "tau", alpha = 0.10)$crit
  
  expect_lt(c_mu_10,  c_mu_005)   # tighter alpha, larger critical value
  expect_gt(c_mu_10,  c_tau_10)   # mu critical values larger tau at same alpha
})


test_that("invalid inputs trigger informative errors", {
  bad_y   <- list(1, 2, 3)
  short_y <- c(1, 2)
  
  expect_error(test_kpss(bad_y), "is.numeric")             # non-numeric y
  expect_error(test_kpss(short_y), "length")               # too few obs
  expect_error(test_kpss(rnorm(10), type = "foo"), "arg")  # bad type
  expect_error(test_kpss(rnorm(10), alpha = 0.2), "arg")   # bad alpha
})


test_that("stationary AR(1) series returns 0 differences", {
  # Create data (stationary AR(1) process)
  set.seed(42)
  y <- stats::arima.sim(model = list(ar = 0.5), n = 100)
  
  # Test case
  n_diff <- estimate_ndiff(y, max_diff = 2, alpha = 0.05, type = "mu")
  
  expect_type(n_diff, "integer")
  expect_equal(n_diff, 0L)
})


test_that("random walk requires exactly one difference", {
  # Create data (random walk)
  set.seed(42)
  y <- cumsum(rnorm(100))
  
  # Test case
  n_diff <- estimate_ndiff(y, max_diff = 2, alpha = 0.05, type = "mu")
  expect_equal(n_diff, 1L)
})


test_that("I(2) series is handled correctly and max_diff is honoured", {
  # Create data (I(2) process)
  set.seed(42)
  y_I2 <- cumsum(cumsum(rnorm(100)))
  
  # Test case 1: If max_diff = 2, it should find 2
  expect_equal(estimate_ndiff(y_I2, max_diff = 2, alpha = 0.05, type = "mu"), 2L)
  
  # Test case 2: If max_diff = 1, we expect NULL (did not reach stationarity)
  expect_null(estimate_ndiff(y_I2, max_diff = 1, alpha = 0.05, type = "mu"))
})


test_that("trend-stationary series behaves differently under mu vs tau", {
  # Create data (strong deterministic trend)
  set.seed(42)
  n  <- 200
  t  <- seq_len(n)
  y  <- 0.8 * t + rnorm(n, sd = 0.1)
  
  # Detrended test
  nd_tau <- estimate_ndiff(y, type = "tau", alpha = 0.05)
  # Demean-only test
  nd_mu  <- estimate_ndiff(y, type = "mu",  alpha = 0.05)
  
  # Test case 1: tau treats deterministic trend as OK
  expect_equal(nd_tau, 0L)
  
  # Test case 2: mu needs a difference to remove trend
  expect_equal(nd_mu,  1L)
})


test_that("very short series yields NULL", {
  expect_null(estimate_ndiff(c(1, 2), max_diff = 2))
})

