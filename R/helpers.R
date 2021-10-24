
#' Create random grid of constant terms
#' 
#' @description The function creates a \code{tibble} with random combinations
#'    of constant terms (sampling with replacement). The number of combinations (rows)
#'    is given by \code{n_sample} and the number of columns is always one.
#'
#' @param y_const Numeric matrix. The result of a call to \code{create_const()}.
#' @param n_sample Integer value. The number of random samples.
#'
#' @return out Random grid of constant terms as \code{tibble}.
#' @noRd

random_const <- function(y_const,
                         n_sample = 1000) {
  
  out <- matrix(
    data = 1,
    nrow = n_sample,
    ncol = 1,
    dimnames = list(c(), colnames(y_const)))
  
  out <- as_tibble(out)
  
  return(out)
}



#' @title Create random grid of lags
#' 
#' @description The function creates a \code{tibble} with random combinations
#'    of lags (sampling with replacement). The number of combinations (rows)
#'    is given by \code{n_sample} and the number of columns is determined by
#'    \code{y_lag}.
#'
#' @param y_lag Numeric matrix. The result of a call to \code{create_lags()}.
#' @param n_sample Integer value. The number of random samples.
#'
#' @return out Random grid of lags as \code{tibble}.
#' @noRd 

random_lags <- function(y_lag,
                        n_sample = 1000) {
  
  out <- matrix(
    data = 0,
    nrow = n_sample,
    ncol = ncol(y_lag),
    dimnames = list(c(), colnames(y_lag)))
  
  for (i in seq_len(n_sample)) {
    index <- sort(sample(
      x = seq_len(ncol(y_lag)),
      size = sample(
        x = seq_len(ncol(y_lag)),
        size = 1),
      replace = FALSE))
    out[i, index] <- 1
  }
  
  out <- as_tibble(out)
  return(out)
}





#' @title Create a grid of fourier terms for best subset regression
#' 
#' @description \code{create_grid_fourier} creates a grid of all combinations
#'    of fourier terms. One fourier term is always a pair of sine and cosine
#'    terms. If higher order fourier terms are used, it is assumed that the
#'    previous terms are added too. 
#'
#' @param n_fourier Integer vector. The number of fourier terms (seasonal cycles) per period.
#' @param period Integer vector. The periodicity of the time series (e.g. for monthly data \code{period = c(12)}, for hourly data \code{period = c(24, 168)}).
#'
#' @return blocks A tibble containing zeros and ones for all combinations of fourier terms.
#' @noRd

create_grid_fourier <- function(n_fourier,
                                period) {
  
  # Initialize empty list
  blocks <- vector(
    mode = "list",
    length = length(period)
  )
  
  # Create matrices with zeros and ones as blocks
  for (j in 1:length(period)) {
    
    # Initialize matrix with zeros
    mat <- matrix(
      data = 0,
      nrow = n_fourier[j],
      ncol = n_fourier[j])
    
    # Fill lower triangular with ones
    mat[lower.tri(mat, diag = TRUE)] <- 1
    
    # Repeat each column two times (one for sine and one for cosine)
    mat <- matrix(
      data = rep(mat, each = 2),
      ncol = 2 * ncol(mat), 
      byrow = TRUE)
    
    # Add row with zeros and flip matrix
    mat <- rbind(mat, 0)
    mat <- rotate(rotate(mat))
    
    colnames(mat) <- paste0(
      paste0(c("sin(", "cos("), rep(1:n_fourier[j], rep(2, n_fourier[j]))),
      "-", round(period[j]), ")")
    
    # Store matrices in list
    blocks[[j]] <- mat
  }
  
  # Merge matrices for multiple periods or extract just the matrix
  if (length(period) > 1) {
    blocks <- as_tibble(do.call(merge, blocks))
  } else {
    blocks <- as_tibble(blocks[[1]])
  }
  return(blocks)
}



#' @title Create random grid of fourier terms
#' 
#' @description The function creates a \code{tibble} with random combinations
#'    of fourier terms (sampling with replacement). The number of combinations
#'    (rows) is given by \code{n_sample} and the number of columns is
#'    determined by \code{fourier} (period and k). One fourier term is 
#'    always a pair of sine and cosine terms. If higher order fourier terms
#'    are used, it is assumed that the previous terms are added too.
#'
#' @param fourier A \code{list} containing the periods and the number of fourier terms as integer vector.
#' @param n_sample Integer value. The number of random samples.
#'
#' @return out Random grid of fourier terms as \code{tibble}.
#' @noRd

random_fourier <- function(fourier,
                           n_sample = 1000) {
  
  # Prepare vectors with number of fourier terms k and periods
  fourier <- tibble(
    period = fourier[[1]],
    k = fourier[[2]]) %>%
    filter(k != 0)
  
  period <- fourier$period
  k <- fourier$k
  
  # Initialize empty list
  out <- vector(
    mode = "list",
    length = length(period)
  )
  
  # Create matrices with zeros and ones as blocks
  for (j in 1:length(period)) {
    
    # Initialize matrix with zeros
    mat <- matrix(
      data = 0,
      nrow = k[j],
      ncol = k[j])
    
    # Fill lower triangular with ones
    mat[lower.tri(mat, diag = TRUE)] <- 1
    
    # Repeat each column two times (one for sine and one for cosine)
    mat <- matrix(
      data = rep(mat, each = 2),
      ncol = 2 * ncol(mat), 
      byrow = TRUE)
    
    # Add row with zeros and flip matrix
    mat <- rbind(mat, 0)
    mat <- rotate(rotate(mat))
    
    colnames(mat) <- paste0(
      paste0(c("sin(", "cos("), rep(1:k[j], rep(2, k[j]))),
      "-", round(period[j]), ")")
    
    # Store matrices in list
    out[[j]] <- mat
  }
  
  # Merge matrices for multiple periods or extract just the matrix
  if (length(period) > 1) {
    out <- do.call(merge, out)
  } else {
    out <- out[[1]]
  }
  
  out <- as_tibble(out) %>%
    sample_n(
      size = n_sample,
      replace = TRUE)
  
  return(out)
}
