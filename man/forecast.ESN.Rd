% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ESN.R
\name{forecast.ESN}
\alias{forecast.ESN}
\title{Forecast a trained ESN}
\usage{
\method{forecast}{ESN}(
  object,
  new_data,
  specials = NULL,
  n_sim = 100,
  n_seed = 42,
  xreg = NULL,
  ...
)
}
\arguments{
\item{object}{An object of class \code{ESN}.}

\item{new_data}{Forecast horizon (n-step ahead forecast)}

\item{specials}{Currently not in use}

\item{n_sim}{Integer value. The number of future sample path to generate.}

\item{n_seed}{Integer value. The seed for the random number generator (for reproducibility).}

\item{xreg}{A \code{tsibble} containing exogenous variables.}

\item{...}{Currently not in use.}
}
\value{
An object of class \code{fable}.
}
\description{
Forecast a trained ESN.
}
