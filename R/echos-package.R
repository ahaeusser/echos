
#' @importFrom purrr map map_dfr
#' @importFrom tidyr pivot_longer
#' @importFrom Matrix rsparsematrix
#' @importFrom matrixStats rowQuantiles
#' @importFrom forecast ndiffs
#' @importFrom rlang is_empty .data abort
#' @importFrom stats complete.cases diffinv na.omit runif
#' @importFrom graphics lines par abline matplot polygon
#' @importFrom grDevices adjustcolor colorRampPalette
#' @importFrom utils tail
#' @importFrom distributional dist_sample
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