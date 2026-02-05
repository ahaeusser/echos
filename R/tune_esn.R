
#' @title Tune hyperparameters of an Echo State Network
#'
#' @description Tune hyperparameters of an Echo State Network (ESN) based on 
#'    time series cross-validation (i.e., rolling forecast). The input series is 
#'    split into \code{n_split} expanding-window train/test sets with test size 
#'    \code{n_ahead}. For each split and each hyperparameter combination 
#'    (\code{alpha, rho, tau}) an ESN is trained via \code{train_esn()} and 
#'    forecasts are generated via \code{forecast_esn()}.
#'
#' @param y Numeric vector containing the response variable.
#' @param n_ahead Integer value. The number of periods for forecasting (i.e. forecast horizon).
#' @param n_split Integer value. The number of rolling train/test splits.
#' @param alpha Numeric vector. The candidate leakage rates (smoothing parameters).
#' @param rho Numeric vector. The candidate spectral radii.
#' @param tau Numeric vector. The candidate reservoir scaling values.
#' @param min_train Integer value. Minimum training sample size for the first split.
#' @param ... Further arguments passed to \code{train_esn()}.
#' 
#' @return An object of class \code{"tune_esn"} (a list) with:
#'   \itemize{
#'      \item \code{pars}: A \code{tibble} with one row per hyperparameter combination and split. Columns include
#'            \code{alpha}, \code{rho}, \code{tau}, \code{split}, \code{train_start}, \code{train_end}, \code{test_start},
#'            \code{test_end}, \code{mse}, \code{mae}, and \code{id}.
#'      \item \code{fcst}: A numeric matrix of point forecasts with \code{nrow(fcst) == nrow(pars)} and
#'            \code{ncol(fcst) == n_ahead}.
#'      \item \code{actual}: The original input series \code{y} (numeric vector), returned for convenience.
#' }
#'
#' @family base functions
#' 
#' @references 
#'    \itemize{
#'       \item{Häußer, A. (2026). Echo State Networks for Time Series Forecasting: Hyperparameter Sweep and Benchmarking. arXiv preprint arXiv:2602.03912, 2026. \url{https://arxiv.org/abs/2602.03912}}
#'       \item{Jaeger, H. (2001). The “echo state” approach to analysing and training recurrent neural networks with an erratum note. Bonn, Germany: German National Research Center for Information Technology GMD Technical Report, 148(34):13.}
#'       \item{Jaeger, H. (2002). Tutorial on training recurrent neural networks, covering BPPT, RTRL, EKF and the "echo state network" approach.}
#'       \item{Lukosevicius, M. (2012). A practical guide to applying echo state networks. In Neural Networks: Tricks of the Trade: Second Edition, pages 659–686. Springer.}
#'       \item{Lukosevicius, M. and Jaeger, H. (2009). Reservoir computing approaches to recurrent neural network training. Computer Science Review, 3(3):127–149.}
#'    }
#'
#' @examples
#' xdata <- as.numeric(AirPassengers)
#' fit <- tune_esn(
#'   y = xdata,
#'   n_ahead = 12,
#'   n_split = 5,
#'   alpha = c(0.5, 1),
#'   rho   = c(1.0),
#'   tau   = c(0.4),
#'   inf_crit = "bic"
#' )
#' 
#' fit$pars
#'
#' @export

tune_esn <- function(y,
                     n_ahead = 12,
                     n_split = 5,
                     alpha = seq(0.1, 1, by = 0.1),
                     rho = seq(0.1, 1, by = 0.1),
                     tau = c(0.1, 0.2, 0.4),
                     min_train = NULL,
                     ...) {
  
  # Argument handling =========================================================
  
  # Check input data
  if (!(is.vector(y) & is.numeric(y))) {
    stop("tune_esn() requires a numeric vector as input.")
  }
  
  if (any(is.na(y))) {
    stop("tune_esn() does not support missing values.")
  }
  
  # Number of observations
  n_total <- length(y)
  
  # Check defaults
  if (is.null(min_train)) {
    min_train <- max(30L, 2L * n_ahead)
  }
  
  if (n_ahead < 1) {
    stop("n_ahead must be >= 1.")
  }
  
  if (n_split < 1) {
    stop("n_split must be >= 1.")
  }
  
  if (n_total < (min_train + n_ahead)) {
    stop("Time series too short for given min_train and n_ahead.")
  }
  
  # Pre-processing ============================================================
  
  # Create splits
  # Expanding-window splits ending at the end of the series
  # Each split uses a test window of length n_ahead
  train_end_last <- n_total - n_ahead
  
  train_ends <- train_end_last - (n_split - 1):0 * n_ahead
  train_ends <- sort(train_ends)
  
  # Enforce minimum training size
  train_ends <- train_ends[train_ends >= min_train]
  if (length(train_ends) == 0) {
    stop("No feasible splits. Reduce n_split/n_ahead or min_train.")
  }
  
  splits <- tibble(
    split = seq_along(train_ends),
    train_start = 1L,
    train_end = as.integer(train_ends),
    test_start = as.integer(train_ends + 1L),
    test_end = as.integer(train_ends + n_ahead)
  )
  
  # Hyperparameter grid
  pars <- expand_grid(
    alpha = alpha,
    rho = rho,
    tau = tau,
    split = splits[["split"]]
  )
  
  pars <- left_join(
    x = pars,
    y = splits,
    by = "split") %>%
    mutate(
      mse = NA_real_, 
      mae = NA_real_
      )
  
  n_steps <- nrow(pars)
  
  # Empty matrix to store points forecasts
  fcst <- matrix(
    data = NA_real_,
    nrow = n_steps,
    ncol = n_ahead
  )
  
  # Tune hyperparameters ======================================================
  
  for (i in seq_len(n_steps)) {
    
    alpha <- pars[["alpha"]][i]
    rho <- pars[["rho"]][i]
    tau <- pars[["tau"]][i]
    
    train_start <- pars[["train_start"]][i]
    train_end <- pars[["train_end"]][i]
    test_start <- pars[["test_start"]][i]
    test_end <- pars[["test_end"]][i]
    
    # Prepare train and test data
    ytrain <- y[(train_start:train_end)]
    ytest <- y[((test_start):test_end)]
    
    # Train ESN model
    model_fit <- train_esn(
      y = ytrain,
      alpha = alpha,
      rho = rho,
      tau = tau,
      ...
    )
    
    # Forecast ESN model
    model_fcst <- forecast_esn(
      object = model_fit,
      n_ahead = n_ahead,
      n_sim = NULL
    )
    
    point <- model_fcst[["point"]]
    error <- ytest - point
    mse <- mean(error^2)
    mae <- mean(abs(error))
    
    pars[["mae"]][i] <- mae
    pars[["mse"]][i] <- mse
    
    fcst[i, ] <- point
  }
  
  keys <- do.call(paste, c(pars[c("alpha","rho","tau")], sep = ","))
  pars[["id"]] <- match(keys, unique(keys))
  
  # Post-processing ===========================================================
  
  # Output model
  structure(
    list(
      pars = pars,
      fcst = fcst, 
      actual = y),
    class = "tune_esn"
  )
}
