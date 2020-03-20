
#' @import Rcpp
#' @import RcppArmadillo
#' @import dplyr
#' @import tsibble
#' @import ggplot2
#' @import fabletools
#' @importFrom purrr map_dfr invoke
#' @importFrom tidyr gather spread
#' @importFrom matrixStats colCumsums colDiffs colMins colMaxs
#' @importFrom Matrix rsparsematrix
#' @importFrom bestNormalize orderNorm
#' @useDynLib echos, .registration = TRUE
#' @name echos
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("echos", libpath)
}