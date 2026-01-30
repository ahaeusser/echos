
<!-- README.md is generated from README.Rmd. Please edit that file -->

# echos <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/echos)](https://cran.r-project.org/package=echos)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Codecov test
coverage](https://codecov.io/gh/ahaeusser/echos/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ahaeusser/echos?branch=master)
[![R-CMD-check](https://github.com/ahaeusser/echos/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahaeusser/echos/actions/workflows/R-CMD-check.yaml)
![](http://cranlogs.r-pkg.org/badges/echos?color=brightgreen)
![](https://cranlogs.r-pkg.org/badges/grand-total/echos?color=brightgreen)
<!-- badges: end -->

The `echos` package provides a comprehensive set of **functions and
methods** for modeling and forecasting univariate time series using
**Echo State Networks (ESNs)**. It offers two alternative approaches:

- **Base R interface:** Functions for modeling and forecasting time
  series using `numeric` vectors, allowing for straightforward
  integration with existing R workflows.
- **Tidy interface:** A seamless integration with the
  [`fable`](https://github.com/tidyverts/fable) framework based on
  [`tsibble`](https://github.com/tidyverts/tsibble), enabling tidy time
  series forecasting and model evaluation. This interface leverages the
  [`fabletools`](https://github.com/tidyverts/fabletools) package,
  providing a consistent and streamlined workflow for model development,
  evaluation, and visualization.

The package features a **lightweight implementation** that enables
**fast and fully automatic** model training and forecasting using ESNs.
You can quickly and easily build accurate ESN models without requiring
extensive hyperparameter tuning or manual configuration.

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=echos):

``` r
install.packages("echos")
```

You can install the **development** version from
[GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("ahaeusser/echos")
```

## Base R

``` r
library(echos)

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

<img src="man/figures/README-base-1.svg" alt="Plot forecast and test data" width="100%" />

## Tidy R

``` r
library(echos)
library(tidyverse)
library(tsibble)
library(fable)

# Prepare train data
train_frame <- m4_data %>%
  filter(series %in% c("M21655", "M2717"))

# Train and forecast ESN model
train_frame %>%
  model(
    "ESN" = ESN(value),
    "ARIMA" = ARIMA(value)
    ) %>%
  forecast(h = 18) %>%
  autoplot(train_frame, level = NULL)
```

<img src="man/figures/README-tidy-1.svg" alt="Plot forecast and train data" width="100%" />
