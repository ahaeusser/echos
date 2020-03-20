
#' Rescale (inverse scaling) the columns of a numeric matrix
#' 
#' Recale (inverse scaling) the columns of a numeric matrix by applying the transformation backwards to original range.
#' 
#' @param data is a vector or matrix containing the values to be rescaled. Each column is a variable and each row an observation.
#' @param old_range A numeric matrix with range (min and max) of original data.
#' @param new_range A numeric vector with new (scaled) interval.
#' 
#' @return data A numeric matrix with rescaled columns.

rescale_data <- function(data, old_range, new_range) {
  # Check y for missing values
  if (anyNA(data) == TRUE) {
    stop("data contains at least one missing value")
  }
  
  # Number of rows and columns in data
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # Extract maximum and minimum for inverse scaling
  min <- old_range["min", ]
  max <- old_range["max", ]
  
  # Extract lower and upper bound
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Vectorize calculation
  min <- matrix(
    data = rep(min, each = n_rows) ,
    nrow = n_rows,
    ncol = n_cols)
  
  max <- matrix(
    data = rep(max, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  lower <- matrix(
    data = rep(lower, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  upper <- matrix(
    data = rep(upper, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  # Inverse normalization to original interval
  data <- ((data - lower) * (max - min)) / (upper - lower) + min
  return(data)
}
