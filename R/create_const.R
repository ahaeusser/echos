

create_const <- function(n_obs) {
  y_const <- matrix(
    data = 1,
    nrow = n_obs,
    ncol = 1,
    dimnames = list(c(), "const"))
  
  return(y_const)
}