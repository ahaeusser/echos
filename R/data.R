
#' @title M4 dataset
#'
#' @description Monthly tsibble with six exemplary time series from the M4 
#'   Forecasting Competition.
#'
#' @docType data
#'
#' @usage data(m4_data)
#'
#' @format A time series object of class \code{tsibble} with 1.152 rows and 4 columns:
#'    \itemize{
#'       \item{\code{series}: Unique identifier as \code{character} (key variable).}
#'       \item{\code{category}: Category (e.g., Demographic, Macro) as \code{factor}.}
#'       \item{\code{index}: Date as \code{yearmonth} (index variable).}
#'       \item{\code{value}: Value as \code{numeric} (measurement variable).}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://github.com/Mcompetitions/M4-methods}{M4 Forecasting Competition}
#'
#' @examples
#' data(m4_data)
"m4_data"