% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-base.R
\name{is.esn}
\alias{is.esn}
\title{Checks if object is of class "esn"}
\usage{
is.esn(object)
}
\arguments{
\item{object}{object to be tested.}
}
\value{
Logical value. If \code{TRUE}, the object is of class "esn".
}
\description{
Returns \code{TRUE} if the object is of class "esn".
}
\examples{
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
is.esn(xmodel)

}
