
#' @title Best subset regression to select model inputs
#' 
#' @description The function \code{tune_inputs()} performs best subset
#'   regression to identify the optimal model inputs. The models are estimated
#'   via Ordinary Least Squares (OLS). If the number of all possible models
#'   exceeds \code{n_sample}, a random search is performed.
#'
#' @param data A \code{tsibble} containing the time series data.
#' @param lags A \code{list} containing integer vectors with the lags associated with each input variable.
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles per period).
#' @param period Integer vector. The periodicity of the time series (e.g. \code{period = c(12)} for monthly data or \code{period = c(24, 168)} for hourly data).
#' @param n_diff Integer vector. The number of non-seasonal differences.
#' @param n_initial Integer value. The number of observations of internal states for initial drop out (throw-off).
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("aic", "bic", "hq")}.
#' @param n_sample Integer value. The number of random samples for random search.
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
                        n_fourier,
                        period,
                        n_diff,
                        n_initial,
                        scale_inputs,
                        inf_crit,
                        n_sample,
                        n_seed) {
  
  # Pre-processing ============================================================
  
  # Get response variables (convert tsibble to numeric matrix)
  y <- invoke(cbind, unclass(data)[measured_vars(data)])
  
  # Calculate seasonal and non-seasonal differences
  y <- diff_data(
    data = y,
    n_diff = n_diff)
  
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
  if (is.null(n_fourier)) {
    y_seas <- NULL
  } else {
    y_seas <- create_fourier(
      times = 1:nrow(y),
      n_fourier = n_fourier,
      period = period)
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_const,
    y_lag,
    y_seas)
  
  # Drop NAs for training
  inputs <- inputs[complete.cases(inputs), , drop = FALSE]
  
  # Number of observations (total)
  n_total <- nrow(y)
  # Number of observations (training)
  n_train <- nrow(inputs)
  
  # Create output layer (train model) =========================================
  
  # Concatenate inputs and reservoir
  X <- inputs
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- X[((n_initial + 1):nrow(X)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):nrow(y)), , drop = FALSE]
  
  # Linear observation weights within the interval [1, 2]
  # obs_weights <- (0:(nrow(Xt) - 1)) * (1 / (nrow(Xt) - 1)) + 1
  # Equal observation weights
  obs_weights <- rep(1, nrow(Xt))
  
  
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
  
  if (is.null(y_seas)) {
    grid_fourier <- NULL
  } else {
    grid_fourier <- random_fourier(
      n_fourier = n_fourier,
      period = period,
      n_sample = n_sample)
  }
  
  random_grid <- bind_cols(
    grid_const,
    grid_lags,
    grid_fourier) %>%
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
        weights = obs_weights)
      
      # Store model metrics
      model_metrics <- tibble(
        df = model$df,
        aic = model$aic,
        bic = model$bic,
        hq = model$hq)
    }
  )
  
  # Filter row with minimum information criterion
  model_metrics <- model_metrics %>%
    mutate(id = row_number(), .before = df) %>%
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
  
  if (!is.null(n_fourier)) {
    input_seas <- tibble(
      input = colnames(y_seas),
      type = "fourier")
  } else {
    input_seas <- NULL
  }
  
  input_types <- bind_rows(
    input_const,
    input_lag,
    input_seas)
  
  model_inputs <- left_join(
    x = model_inputs,
    y = input_types,
    by = "input")
  
  
  # Check for constant term
  const <- model_inputs %>%
    filter(type == "const") %>%
    mutate(value = ifelse(usage == 1, TRUE, FALSE)) %>%
    pull(value)
  
  # Check for relevant lags
  if (any(filter(model_inputs, type == "lag")$usage != 0)) {
    lags <- model_inputs %>%
      filter(type == "lag") %>%
      filter(usage == 1) %>%
      mutate(value = str_nth_number(input, n = 1)) %>%
      pull(value) %>%
      list()
  } else {
    # If usage is zero for all lags, at least lag one is used (fallback option)
    lags <- list(c(1))
  }
  
  # Check for fourier terms
  if (any(filter(model_inputs, type == "fourier")$usage != 0)) {
    n_fourier <- model_inputs %>%
      filter(type == "fourier") %>%
      mutate(value = str_nth_number(input, n = 1)) %>%
      mutate(period = str_nth_number(input, n = 2)) %>%
      mutate(flag = usage * value) %>%
      group_by(period) %>%
      summarise(
        value = max(flag, na.rm = TRUE),
        .groups = "drop") %>%
      pull(value)
  } else {
    n_fourier <- NULL
  }
  
  # Store and return
  model_inputs <- list(
    const = const,
    lags = lags,
    n_fourier = n_fourier)
  
  return(model_inputs)
}
