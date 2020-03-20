#' Reservoir weights
#' 
#' This function creates the random reservoir weight matrix (scaled to spectral radius rho)
#' 
#' @param n_res Number of internal states within the reservoir
#' @param density The parameter defines the connectivity of the reservoir weight matrix (dense or sparse)
#' @param unif_range Lower and upper bound of the uniform distribution
#' 
#' @return wres The final reservoir weight matrix

create_wres <- function(n_res,
                        rho,
                        density,
                        scale_runif,
                        symmetric = FALSE) {
  
  # Create initial random weight matrix for the reservoir
  wres <- matrix(
    data = runif(n = n_res * n_res,
                 min = scale_runif[1],
                 max = scale_runif[2]),
    nrow = n_res,
    ncol = n_res)
  
  # Create a random sparse pattern matrix with defined density
  wsparse <- rsparsematrix(
    nrow = n_res,
    ncol = n_res,
    density = density,
    rand.x = NULL,
    symmetric = symmetric)
  
  wres <- as.matrix(wres * wsparse)
  
  # Calculate the absolute, maximum eigenvalue of the reservoir weight matrix
  eig <- eigen(wres, symmetric = symmetric, only.values = TRUE)$values
  max_abs_eig <- max(abs(eig))
  
  # Rescale the reservoir weight matrix to spectral radius rho
  wres <- 1 / max_abs_eig * wres * rho
  return(wres)
}
