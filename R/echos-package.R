
#' @importFrom tidyr pivot_longer expand_grid
#' @importFrom rlang .data enquo sym
#' @importFrom stats complete.cases diffinv na.omit runif quantile sd lm residuals
#' @importFrom graphics lines par abline matplot polygon
#' @importFrom grDevices adjustcolor colorRampPalette
#' @importFrom utils tail
#' @importFrom distributional dist_normal dist_sample
#' @importFrom dplyr select filter mutate arrange bind_rows bind_cols left_join
#' @import Rcpp
#' @import RcppArmadillo
#' @import tsibble
#' @import fabletools
#' @useDynLib echos, .registration = TRUE

.onUnload <- function (libpath) {
  library.dynam.unload("echos", libpath)
}
NULL