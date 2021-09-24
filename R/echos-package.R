
#' @importFrom purrr map map_dfr invoke cross_df
#' @importFrom tidyr pivot_longer
#' @importFrom strex str_nth_number
#' @importFrom matrixStats colCumsums colDiffs colMins colMaxs rowSds
#' @importFrom Matrix rsparsematrix
#' @importFrom feasts unitroot_ndiffs unitroot_kpss
#' @importFrom rlang is_empty .data abort
#' @importFrom stats complete.cases diffinv na.omit optim pacf qnorm runif lm
#' @importFrom utils tail
#' @importFrom distributional dist_normal
#' @importFrom glmnet glmnet cv.glmnet
#' @importFrom broom glance
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