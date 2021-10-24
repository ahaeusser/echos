
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
#' @param scale_inputs Numeric vector. The lower and upper bound for scaling the time series data.
#' @param inf_crit Character value. The information criterion \code{inf_crit = c("aic", "bic", "hq")}.
#' @param n_models Integer value. The number of random samples for random search.
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
                        scale_inputs,
                        inf_crit,
                        n_models,
                        n_seed) {
  
  # Pre-processing ============================================================
  
  # Prepare constants as integers
  n_initial <- as.integer(n_initial)
  n_seed <- as.integer(n_seed)
  
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
      n_diff = dx
    )
    
    # Scale data to specified interval
    xreg <- scale_data(
      data = xreg,
      new_range = scale_inputs)$data
  }
  
  # Prepare output variable
  # Name of output variable
  name_output <- measured_vars(data)
  # Convert tsibble to numeric matrix
  y <- invoke(cbind, unclass(data)[name_output])
  # Number of outputs
  n_outputs <- ncol(y)
  # Number of total observations
  n_total <- nrow(y)
  # Create copy of original data for later usage
  yy <- y
  
  # Calculate nth-difference of output variable
  y <- diff_data(
    data = y,
    n_diff = dy
  )
  
  # Scale data to specified interval
  scaled <- scale_data(
    data = y,
    new_range = scale_inputs
  )
  
  y <- scaled$data
  old_range <- scaled$old_range
  
  
  # Create input layer ========================================================
  
  # Create lagged variables as matrix
  if (is.null(lags)) {
    y_lag <- NULL
  } else {
    y_lag <- create_lags(
      data = y,
      lags = lags
    )
  }
  
  # Create fourier terms as matrix
  if (is.null(fourier)) {
    y_fourier <- NULL
  } else {
    # Create numeric matrix of fourier terms
    y_fourier <- create_fourier(
      x = 1:n_total,
      period = fourier[[1]],
      k = fourier[[2]]
    )
  }
  
  # Concatenate input matrices
  inputs <- cbind(
    y_lag,
    y_fourier,
    xreg
  )
  
  # Drop NAs for training
  inputs <- inputs[complete.cases(inputs), , drop = FALSE]
  
  # Number of observations (training)
  n_train <- nrow(inputs)
  # Number of observations (accounted for initial throw-off)
  n_obs <- n_train - n_initial
  # Number of input features (constant, lagged variables, etc.)
  n_inputs <- ncol(inputs)
  # Train index (with initial throw-off)
  index_train <- c((1 + (n_total - n_train + n_initial)):n_total)
  # Train index (without initial throw-off)
  index_states <- c((1 + (n_total - n_train)):n_total)
  
  
  
  # Tune inputs ===============================================================
  
  # Concatenate inputs and reservoir
  Xt <- inputs
  # Adjust response and design matrix for initial throw-off and lag-length
  Xt <- Xt[((n_initial + 1):nrow(Xt)), , drop = FALSE]
  yt <- y[((n_initial + 1 + (n_total - n_train)):n_total), , drop = FALSE]
  
  
  # Create random grid ========================================================
  
  # Set seed for reproducibility
  set.seed(n_seed)
  
  grid_lags <- random_lags(
    y_lag = y_lag,
    n_sample = n_models
  )
  
  grid_fourier <- random_fourier(
    fourier = fourier,
    n_sample = n_models
  )
  
  random_grid <- bind_cols(
    grid_lags,
    grid_fourier) %>%
    distinct()
  
  # Ensure feasibility of fourier terms
  random_grid <- random_grid[, colnames(Xt)]
  
  const <- matrix(
    data = 1,
    nrow = nrow(Xt),
    ncol = 1,
    dimnames = list(c(), "(Intercept)")
  )
  
  # Train models via least squares
  model_object <- map(
    .x = seq_len(nrow(random_grid)),
    .f = ~{
      fit_lm(
        x = cbind(const, Xt[, which(random_grid[.x, ] == 1), drop = FALSE]),
        y = yt
      )
    }
  )
  
  n_models <- nrow(random_grid)
  
  model_names <- paste0("model(", 1:n_models, ")")
  names(model_object) <- model_names
  
  # Extract model metrics
  model_metrics <- map_dfr(
    .x = 1:n_models,
    .f = ~{model_object[[.x]]$metrics}
  )
  
  # Identify best models
  model_metrics <- model_metrics %>%
    mutate(id = model_names, .before = loglik) %>%
    arrange(!!sym(inf_crit)) %>%
    slice_head(n = 1)
  
  model_inputs <- rownames(model_object[[model_metrics$id]]$wout)[-1]
  model_inputs <- tibble(input = model_inputs)
  
  input_lag <- tibble(
    input = colnames(y_lag),
    type = "lag"
  )
  
  input_fourier <- tibble(
    input = colnames(y_fourier),
    type = "fourier"
  )
  
  input_types <- bind_rows(
    input_lag,
    input_fourier
  )
  
  model_inputs <- left_join(
    x = model_inputs,
    y = input_types,
    by = "input"
  )
  
  # Check for relevant lags
  if (any(model_inputs$type == "lag")) {
    lags <- model_inputs %>%
      filter(type == "lag") %>%
      mutate(value = str_nth_number(input, n = 1)) %>%
      pull(value) %>%
      list()
  } else {
    # If usage is zero for all lags, at least lag one is used (fallback option)
    lags <- list(c(1))
  }
  
  # Check for fourier terms
  if (any(model_inputs$type == "fourier")) {
    fourier <- model_inputs %>%
      filter(type == "fourier") %>%
      mutate(k = str_nth_number(input, n = 1)) %>%
      mutate(period = str_nth_number(input, n = 2)) %>%
      group_by(period) %>%
      summarise(k = max(k, na.rm = TRUE), .groups = "drop")
    
    fourier <- list(
      fourier$period,
      fourier$k
    )
  } else {
    fourier <- NULL
  }
  
  # Store and return
  model_inputs <- list(
    lags = lags,
    fourier = fourier
    )
  
  return(model_inputs)
}
