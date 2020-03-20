
#' Inverse transforms the columns of a numeric matrix
#' 
#' Inverse transforms the columns of a numeric matrix using inverse ordered quantile normalizing transformation (OQR)
#'
#' @param object A list with objects of class orderNorm.
#' @param new_data New data for inverse transforming (e.g. forecast)
#'
#' @return data Inverse transformed data as numeric matrix.
#' 
#' @export

inv_trans_data <- function(object, new_data) {
  
  # Check y for missing values
  if (anyNA(new_data) == TRUE) {
    stop("data contains at least one missing value")
  }
  
  names_outputs <- colnames(new_data)
  n_outputs <- ncol(new_data)
  
  data <- sapply(seq_len(n_outputs), function(n) {
    cbind(predict(
      object = object[[n]],
      newdata = new_data[, n],
      inverse = TRUE,
      warn = FALSE))
  })
  colnames(data) <- names_outputs
  return(data)
}