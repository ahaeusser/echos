
#' @title Hourly day-ahead electricity spot prices
#'
#' @description Hourly tsibble with day-ahead electricity spot prices from
#'   the ENTSO-E Transparency Platform. The data set contains time series data
#'   from 2019-01-01 00:00:00 to 2019-12-31 23:00:00 for the bidding zone
#'   Germany-Luxembourg.
#'
#' @docType data
#'
#' @usage data(spot_price)
#'
#' @format A time series object of class \code{tsibble} with 70.080 rows and 5 columns:
#'    \itemize{
#'       \item{\code{Time}: Date and time (index variable)}
#'       \item{\code{Series}: Time series name (key variable)}
#'       \item{\code{Unit}: Measured unit (key variable)}
#'       \item{\code{BZN}: Bidding zone (key variable)}
#'       \item{\code{Value}: Measurement variable}
#'       }
#'
#' @keywords datasets
#'
#' @source \href{https://transparency.entsoe.eu/transmission-domain/r2/dayAheadPrices/show}{ENTSO-E Transparency Platform}
#'
#' @examples
#' data(spot_price)
"spot_price"