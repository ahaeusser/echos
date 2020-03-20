#' Input weight matrix
#' 
#' This function creates the random input weight matrices
#' 
#' @param n_inputs Number of input features
#' @param n_res Number of internal states within the reservoir
#' @param scale_runif Lower and upper bound of the uniform distribution
#' 
#' @return win A list containing the input weight matrices.

create_win <- function(n_inputs,
                       n_res,
                       scale_runif) {
  win <- matrix(
    data = runif(n = n_res * n_inputs,
                 min = scale_runif[1],
                 max = scale_runif[2]),
    nrow = n_res,
    ncol = n_inputs)
  return(win)
}