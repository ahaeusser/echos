
#' Transform the columns of a numeric matrix
#' 
#' Transform the columns of a numeric matrix using ordered quantile normalizing transformation (OQR)
#' 
#' @param data A numeric matrix containing the values to be transformed. Each column is a variable and each row an observation.
#' 
#' @return result A list with the transformed data as numeric matrix and objects of class orderNorm (for inverse transform).
#' 
#' @export

trans_data <- function(data) {
  
  # # Check y for missing values
  # if (anyNA(data) == TRUE) {
  #   stop("data contains at least one missing value")
  # }
  
  names_outputs <- colnames(data)
  n_outputs <- ncol(data)
  
  # Perform OQR transformation column wise
  trans_obj <- lapply(seq_len(n_outputs), function(n) {
    bestNormalize::orderNorm(data[, n], warn = FALSE)
  })
  
  # Extract transformed data and concatenate column wise
  data <- sapply(seq_len(n_outputs), function(n) {
    cbind(predict(trans_obj[[n]]))
  })
  
  colnames(data) <- names_outputs
  
  result <- list(
    data = data,
    trans_obj = trans_obj)
  
  return(result)
}
