% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast_esn.R
\name{forecast_esn}
\alias{forecast_esn}
\title{Forecast an Echo State Network}
\usage{
forecast_esn(
  object,
  n_ahead = 18,
  levels = c(80, 95),
  n_sim = 100,
  n_seed = 42
)
}
\arguments{
\item{object}{An object of class \code{esn}. The result of a call to \code{train_esn()}.}

\item{n_ahead}{Integer value. The number of periods for forecasting (i.e. forecast horizon).}

\item{levels}{Integer vector. The levels of the forecast intervals, e.g., 80\% and 95\%.}

\item{n_sim}{Integer value. The number of future sample path generated during simulation.}

\item{n_seed}{Integer value. The seed for the random number generator (for reproducibility).}
}
\value{
A \code{list} containing:
   \itemize{
      \item{\code{point}: Numeric vector containing the point forecasts.}
      \item{\code{interval}: Numeric matrix containing the forecast intervals.}
      \item{\code{sim}: Numeric matrix containing the simulated future sample path.}
      \item{\code{levels}: Integer vector. The levels of the forecast intervals.}
      \item{\code{actual}: Numeric vector containing the actual values.}
      \item{\code{fitted}: Numeric vector containing the fitted values.}
      \item{\code{n_ahead}: Integer value. The number of periods for forecasting (forecast horizon).}
      \item{\code{model_spec}: Character value. The model specification as string.}
      }
}
\description{
Forecast an Echo State Network (ESN) from a trained model via
   recursive forecasting. Forecast intervals are generated by simulating
   future sample path based on a moving block bootstrap of the residuals and
   estimating the quantiles from the simulations.
}
\examples{
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
xfcst <- forecast_esn(xmodel, n_ahead = 12)
plot(xfcst)
}
