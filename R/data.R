
#' @title Monthly time series "M10960" from the M4 Competition
#'
#' @description The dataset contains the monthly time series "M10960"
#'   from the M4 Competition.
#'
#' @docType data
#'
#' @usage data(m4_monthly)
#'
#' @format A time series object of class \code{tsibble} with 324 rows and 4 columns:
#'    \itemize{
#'       \item{\code{date_time}: Date and time (index variable)}
#'       \item{\code{series_id}: Time series ID from M4 forecasting competition (key variable)}
#'       \item{\code{category}: Category from M4 forecasting competition (key variable)}
#'       \item{\code{value}: Time series value (measurement variable)}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://github.com/Mcompetitions/M4-methods/tree/master/Dataset}{M4 Competition}
#'
#' @examples
#' data(m4_monthly)
"m4_monthly"