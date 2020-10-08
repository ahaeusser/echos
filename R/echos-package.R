
#' @importFrom purrr map map_dfr invoke cross_df
#' @importFrom tidyr gather spread pivot_longer
#' @importFrom readr parse_number
#' @importFrom matrixStats colCumsums colDiffs colMins colMaxs rowSds
#' @importFrom Matrix rsparsematrix
#' @importFrom feasts unitroot_ndiffs unitroot_kpss
#' @importFrom rlang is_empty .data abort
#' @importFrom stats complete.cases diffinv na.omit optim pacf qnorm runif
#' @importFrom utils tail
#' @importFrom distributional dist_normal
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