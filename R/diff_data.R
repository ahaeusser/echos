
#' Calculate n-th differences
#' 
#' This function takes a numeric matrix and calculates n-th differences for each column. Leading NAs are padded.
#'
#' @param data 
#' @param n_diff
#' @param na_rm
#'
#' @return
#' @export

diff_data <- function(data, n_diff, na_rm = TRUE) {
  
  names_outputs <- colnames(data)
  n_outputs <- ncol(data)
  
  y_diff <- matrixStats::colDiffs(
    data,
    lag = 1L,
    differences = n_diff)
  
  colnames(y_diff) <- names_outputs
  
  if (na_rm == FALSE) {
    fill_na <- matrix(
      data = NA_real_,
      nrow = n_diff,
      ncol = n_outputs)
    y_diff <- rbind(fill_na, y_diff)
  }
  return(y_diff)
}