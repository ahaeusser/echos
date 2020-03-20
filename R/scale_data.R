
#' Scale the columns of a numeric matrix
#' 
#' Scale the columns of a numeric matrix to a specific interval
#' 
#' @param data A numeric matrix containing the values to be scaled. Each column is a variable and each row an observation.
#' @param new_range The range for scaling (first value represents the replacement for the min value, the second is the substitute for the max value, default c(-1, 1)).
#' 
#' @return data A numeric matrix with scaled columns.
#' 
#' @export

scale_data <- function(data, new_range = c(-1, 1)) {
  
  # # Check y for missing values
  # if (anyNA(data) == TRUE) {
  #   stop("data contains at least one missing value")
  #   }
  
  # Number of rows (observations) and columns (variables) in data
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # Calculate minimum and maximum by column
  min <- colMins(data, na.rm = TRUE)
  max <- colMaxs(data, na.rm = TRUE)
  old_range <- rbind(min, max)
  rownames(old_range) <- c("min", "max")
  colnames(old_range) <- colnames(data)
  
  # Extract the lower and upper bound from interval
  lower <- new_range[1]
  upper <- new_range[2]
  
  # Vectorize calculations (i.e. expand min, max, lower and upper to matrix)
  min <- matrix(
    data = rep(min, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  max <- matrix(
    data = rep(max, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  lower <- matrix(
    data = rep(lower,each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  upper <- matrix(
    data = rep(upper, each = n_rows),
    nrow = n_rows,
    ncol = n_cols)
  
  # Scale matrix y column wise to new interval
  data <- lower + ((data - min) * (upper - lower) / (max - min))
  
  result <- list(
    data = data,
    old_range = old_range)
  
  return(result)
}
