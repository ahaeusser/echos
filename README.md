
<!-- README.md is generated from README.Rmd. Please edit that file -->

# echos

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Codecov test
coverage](https://codecov.io/gh/ahaeusser/echos/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ahaeusser/echos?branch=master)
[![R-CMD-check](https://github.com/ahaeusser/echos/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahaeusser/echos/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The package provides a tidy interface for modeling and forecasting
univariate time series using Echo State Networks (ESNs). The model works
within the `fable` framework provided by the `fabletools` package, which
provides the tools to evaluate, visualize, and combine models in a
workflow consistent with the tidyverse.

***Disclaimer:*** The `echos` package is highly experimental and it is
very likely that there will be (substantial) changes in the near future.
These changes will probably affect the interface (e.g. arguments within
`ESN()`) and the underlying modeling procedure itself.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ahaeusser/echos")
```

## Getting started

``` r

library(echos)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo

# Forecast horizon
n_ahead <- 12 # forecast horizon
# Number of observations
n_obs <- length(AirPassengers)
# Number of observations for training
n_train <- n_obs - n_ahead

# Prepare train and test data
xtrain <- AirPassengers[(1:n_train)]
xtest <- AirPassengers[((n_train+1):n_obs)]

# Train and forecast ESN model
xmodel <- train_esn(y = xtrain)
xfcst <- forecast_esn(xmodel, n_ahead = n_ahead)

# Plot result
plot(xfcst, test = xtest)
```

<img src="man/figures/README-base-1.svg" width="100%" />
