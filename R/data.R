
#' @title M4 dataset
#'
#' @description \code{tsibble} with six monthly time series from the M4 
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

#' @title Synthetic data
#'
#' @description \code{tibble} with ten synthetic time series, i.e., Square Wave,
#'   Sawtooth Wave, Harmonic Wave, Harmonic Wave w/ Trend, Amplitude Modulated
#'   Wave, Frequency Modulated Wave, AR(1) Process, MA(2) Process, White Noise
#'   Process and Random Walk Process
#'
#' @docType data
#'
#' @usage data(synthetic_data)
#'
#' @format An object of class \code{tibble} with 2.000 rows and 3 columns:
#'    \itemize{
#'       \item{\code{variable}: Unique identifier as \code{character} (key variable).}
#'       \item{\code{index}: Index as \code{inteher} (index variable).}
#'       \item{\code{value}: Value as \code{numeric} (measurement variable).}
#'       }
#'
#' @keywords datasets
#'
#' @examples
#' data(synthetic_data)
"synthetic_data"
