
#' @title Best subset regression to select model inputs
#' 
#' @description The function \code{tune_inputs()} performs best subset
#'   regression to identify the optimal model inputs. The models are estimated
#'   via Ordinary Least Squares (OLS). If the number of all possible models
#'   exceeds \code{n_sample}, a random search is performed.
#'
#' @param data A \code{tsibble} containing the time series data.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param fourier A \code{list} containing the periods and the number of fourier terms as integer vector.
#' @param xreg A \code{tsibble} containing exogenous variables.
#' @param dy Integer vector. The nth-differences of the response variable.
#' @param dx Integer vector. The nth-differences of the exogenous variables.
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param weights Numeric vector. Observation weights for weighted least squares estimation.
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("aic", "bic", "hq")}.
#' @param n_sample Integer value. The number of random samples for random search.
#' @param n_seed Integer value. The seed for the random number generator (for reproducibility).
#'
#' @return A \code{list} containing:
#'   \itemize{
#'     \item{\code{const}: Logical value. If \code{TRUE}, an intercept term is used.}
#'     \item{\code{lags}: A \code{list} containing integer vectors with the lags associated with each input variable.}
#'     \item{\code{n_fourier}: Integer vector. The number of fourier terms (seasonal cycles per period).}
#'     } 
#' @export

tune_inputs <- function(data,
                        lags,
                        fourier,
                        xreg,
                        dy,
                        dx,
                        n_initial,
                        weights,
                        scale_inputs,
                        inf_crit,
                        n_sample,
                        n_seed) {
  
  # Pre-processing ============================================================
  
  # Prepare exogenous variables
  if (is.null(xreg)) {
    xx <- NULL
  } else {
    # Convert tsibble to numeric matrix
    xreg <- invoke(cbind, unclass(xreg)[measured_vars(xreg)])
    # Copy of original data for later usage
    xx <- xreg
    
    # Calculate nth-differences
    if (is.null(dx)) {dx <- 0}
    xreg <- diff_data(
      data = xreg,
      n_diff = dx)
    
    # Scale data to specified interval
    xreg <- scale_data(
      data = xreg,
      new_range = scale_inputs)$data
  }
  
  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(data)[measured_vars(data)])
  
  # Number of total observations
  n_total <- nrow(y)
  
  # Calculate seasonal and non-seasonal differences
  y <- diff_data(
    data = y,
    n_diff = dy)
  
  # Scale data to the specified interval
  scaled <- scale_data(
    data = y,
    new_range = scale_inputs)
  
  y <- scaled$data
  old_range <- scaled$old_range
  
  
  # Create input layer ========================================================
  
  # Create constant term (intercept term) as matrix
  y_const <- create_const(
    n_obs = nrow(y))
  
  # Create lagged variables as matrix
  if (is.null(lags)) {
    y_lag <- NULL
  } else {
    y_lag <- create_lags(
      data = y,
      lags = lags)
  }
  
  # Create fourier terms (trigonometric terms) as matrix
  if (is.null(fourier)) {
    y_fourier <- NULL
  } else {
    # Create numeric matrix of fourier terms
    y_fourier <- create_fourier(
      x = 1:n_total,
      period = fourier[[1]],
      k = fourier[[2]])
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_const,
    y_lag,
    y_fourier,
    xreg)
  
  # Drop NAs for training
  inputs <- inputs[complete.cases(inputs), , drop = FALSE]
  
  # Number of observations (training)
  n_train <- nrow(inputs)
  
  # Create output layer (train model) =========================================
  
  # Concatenate inputs and reservoir
  X <- inputs
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- X[((n_initial + 1):nrow(X)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):nrow(y)), , drop = FALSE]
  
  # Observation weights for ridge regression
  if (is.null(weights)) {
    weights <- rep(1, nrow(Xt))
  }
  
  
  # Create random grid ========================================================
  
  # Set seed for reproducibility
  set.seed(n_seed)
  
  grid_const <- random_const(
    y_const = y_const,
    n_sample = n_sample)
  
  if (is.null(y_lag)) {
    grid_lags <- NULL
  } else {
    grid_lags <- random_lags(
      y_lag = y_lag,
      n_sample = n_sample)
  }
  
  if (is.null(y_fourier)) {
    grid_fourier <- NULL
  } else {
    grid_fourier <- random_fourier(
      fourier = fourier,
      n_sample = n_sample)
  }
  
  
  if (!is.null(xreg)) {
    grid_xreg <- matrix(
      data = 1,
      nrow = n_sample,
      ncol = ncol(xreg),
      dimnames = list(c(), colnames(xreg)))
    
    grid_xreg <- as_tibble(grid_xreg)
  } else {
    grid_xreg <- NULL
  }
  
  random_grid <- bind_cols(
    grid_const,
    grid_lags,
    grid_fourier,
    grid_xreg) %>%
    distinct()
  
  # Ensure feasibility of fourier terms
  random_grid <- random_grid[, colnames(Xt)]
  
  # Train models via least squares
  model_metrics <- map_dfr(
    .x = seq_len(nrow(random_grid)),
    .f = function(n) {
      # Train individual models
      model <- train_ridge(
        X = Xt[, which(random_grid[n, ] == 1), drop = FALSE],
        y = yt,
        lambda = 0,
        weights = weights)
      
      # Store model metrics
      tibble(
        dof = model$dof,
        aic = model$aic,
        bic = model$bic,
        hq = model$hq
      )
    }
  )
  
  # Filter row with minimum information criterion
  model_metrics <- model_metrics %>%
    mutate(id = row_number(), .before = .data$dof) %>%
    slice(which.min(!!sym(inf_crit)))
  
  model_grid <- random_grid
  
  # Filter for optimal model and transpose to long format
  model_inputs <- model_grid %>%
    slice(model_metrics$id) %>%
    pivot_longer(
      cols = everything(),
      names_to = "input",
      values_to = "usage")
  
  input_const <- tibble(
    input = colnames(y_const),
    type = "const")
  
  if (!is.null(lags)) {
    input_lag <- tibble(
      input = colnames(y_lag),
      type = "lag")
  } else {
    input_lag <- NULL
  }
  
  if (!is.null(fourier)) {
    input_fourier <- tibble(
      input = colnames(y_fourier),
      type = "fourier")
  } else {
    input_fourier <- NULL
  }
  
  if (!is.null(xreg)) {
    input_xreg <- tibble(
      input = colnames(xreg),
      type = "xreg")
  } else {
    input_xreg <- NULL
  }
  
  input_types <- bind_rows(
    input_const,
    input_lag,
    input_fourier,
    input_xreg)
  
  model_inputs <- left_join(
    x = model_inputs,
    y = input_types,
    by = "input")
  
  
  # Extract final model inputs ==================================================
  
  # Check for constant term
  const <- model_inputs %>%
    filter(.data$type == "const") %>%
    mutate(value = ifelse(.data$usage == 1, TRUE, FALSE)) %>%
    pull(.data$value)
  
  # Check for relevant lags
  if (any(filter(model_inputs, .data$type == "lag")$usage != 0)) {
    lags <- model_inputs %>%
      filter(.data$type == "lag") %>%
      filter(.data$usage == 1) %>%
      mutate(value = str_nth_number(.data$input, n = 1)) %>%
      pull(.data$value) %>%
      list()
  } else {
    # If usage is zero for all lags, at least lag one is used (fallback option)
    lags <- list(c(1))
  }
  
  # Check for fourier terms
  if (any(filter(model_inputs, .data$type == "fourier")$usage != 0)) {
    fourier <- model_inputs %>%
      filter(.data$type == "fourier") %>%
      mutate(k = str_nth_number(.data$input, n = 1)) %>%
      mutate(period = str_nth_number(.data$input, n = 2)) %>%
      mutate(flag = .data$usage * .data$k) %>%
      group_by(.data$period) %>%
      summarise(k = max(.data$flag, na.rm = TRUE), .groups = "drop")
    
    fourier <- list(
      fourier$period,
      fourier$k)
  } else {
    fourier <- NULL
  }
  
  # Store and return
  model_inputs <- list(
    const = const,
    lags = lags,
    fourier = fourier)
  
  return(model_inputs)
}
