% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-fable.R
\name{report.ESN}
\alias{report.ESN}
\title{Provide a detailed summary of the trained ESN model}
\usage{
\method{report}{ESN}(object, ...)
}
\arguments{
\item{object}{An object of class \code{ESN}.}

\item{...}{Currently not in use.}
}
\value{
Print detailed model summary.
}
\description{
Provide a detailed summary of the trained ESN model.
}
\examples{
library(tsibble)
library(fable)
AirPassengers \%>\%
as_tsibble() \%>\%
model("ESN" = ESN(value)) \%>\%
report()

}
