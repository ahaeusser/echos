
#' @importFrom purrr map_dfr invoke
#' @importFrom tidyr gather spread
#' @importFrom matrixStats colCumsums colDiffs colMins colMaxs rowSds
#' @importFrom Matrix rsparsematrix
#' @import Rcpp
#' @import RcppArmadillo
#' @import tsibble
#' @rawNamespace import(dplyr, except = id)
#' @import fabletools
#' @useDynLib echos, .registration = TRUE

.onUnload <- function (libpath) {
  library.dynam.unload("echos", libpath)
}

NULL