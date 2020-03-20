
#' @import Rcpp
#' @import RcppArmadillo
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @import matrixStats
#' @import tsibble
#' @import rlang
#' @useDynLib echos, .registration = TRUE
#' @name echos
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("echos", libpath)
}