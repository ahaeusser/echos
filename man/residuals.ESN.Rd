% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-fable.R
\name{residuals.ESN}
\alias{residuals.ESN}
\title{Extract residuals from a trained ESN}
\usage{
\method{residuals}{ESN}(object, ...)
}
\arguments{
\item{object}{An object of class \code{mdl_df}, containing an ESN model.}

\item{...}{Currently not in use.}
}
\value{
Residuals extracted from the object.
}
\description{
Extract residuals from a trained ESN as \code{tsibble}.
}
\examples{
library(tsibble)
library(fable)
AirPassengers \%>\%
as_tsibble() \%>\%
model("ESN" = ESN(value)) \%>\%
residuals()

}
