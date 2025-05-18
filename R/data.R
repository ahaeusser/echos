
#' @title M4 dataset
#'
#' @description \code{tsibble} with six monthly time series from the M4 
#'   Forecasting Competition. The datasets contains the following time series:
#'   \itemize{
#'      \item{M21655 (Demographic), 1995 Jan - 2015 Mar}
#'      \item{M21683 (Demographic), 2000 Jan - 2023 Apr}
#'      \item{M2717 (Macro), 1996 Jan - 2016 Nov}
#'      \item{M28597 (Industry), 1996 Jan - 2016 Dec}
#'      \item{M42529 (Finance), 2001 Jan - 2009 Apr}
#'      \item{M4813 (Macro), 1994 Apr - 2006 May}
#'   }
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
#' @description \code{tibble} with ten synthetic time series. The dataset 
#'   contains the following time series:
#'   \itemize{
#'      \item{Square Wave}
#'      \item{Sawtooth Wave}
#'      \item{Harmonic Wave}
#'      \item{Harmonic Wave w/ Trend}
#'      \item{Amplitude Modulated Wave}
#'      \item{Frequency Modulated Wave}
#'      \item{AR(1) Process}
#'      \item{MA(2) Process}
#'      \item{White Noise Process}
#'      \item{Random Walk Process}
#'   }
#'
#' @docType data
#'
#' @usage data(synthetic_data)
#'
#' @format An object of class \code{tibble} with 2.000 rows and 3 columns:
#'    \itemize{
#'       \item{\code{variable}: Unique identifier as \code{character} (key variable).}
#'       \item{\code{index}: Index as \code{integer} (index variable).}
#'       \item{\code{value}: Value as \code{numeric} (measurement variable).}
#'       }
#'
#' @keywords datasets
#'
#' @examples
#' data(synthetic_data)
"synthetic_data"
