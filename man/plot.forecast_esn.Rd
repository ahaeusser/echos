% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-base.R
\name{plot.forecast_esn}
\alias{plot.forecast_esn}
\title{Plot point forecasts and actual values of a trained ESN model.}
\usage{
\method{plot}{forecast_esn}(x, test = NULL, fitted = TRUE, ...)
}
\arguments{
\item{x}{An object of class \code{forecast_esn}.}

\item{test}{Numeric vector. Test data, i.e., out-of-sample actual values.}

\item{fitted}{Logical value. If \code{TRUE}, fitted values are added.}

\item{...}{Currently not in use.}
}
\value{
Line chart of point forecast and actual values.
}
\description{
Plot point forecasts, actual and fitted values of a trained ESN 
  model as line chart. Optionally, test data (out-of-sample) can be added to 
  the plot.
}
\examples{
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
xfcst <- forecast_esn(xmodel, n_ahead = 12)
plot(xfcst)

}
