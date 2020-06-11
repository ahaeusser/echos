
<!-- README.md is generated from README.Rmd. Please edit that file -->

# echos

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This package provides a tidy R interface for modeling and forecasting
univariate time series using Echo State Networks (ESNs). The model works
within the `fable` framework provided by the `fabletools` package, which
provides the tools to evaluate, visualise, and combine models in a
workflow consistent with the tidyverse.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ahaeusser/echos")
```

## Usage

### Load packages

``` r
library(echos)
library(dplyr)
library(tsibble)
library(fabletools)
library(fable)
Sys.setlocale("LC_TIME", "C")
#> [1] "C"
```

### Prepare data

``` r
n_train <- 130  # training window
n_ahead <- 12   # testing window (forecast horizon)

# Prepare data as tsibble
data <- AirPassengers %>%
  as_tsibble() %>%
  rename(
    Date = index,
    Passengers = value)

# Training data
data_train <- data %>%
  slice(1:n_train)

data_train
#> # A tsibble: 130 x 2 [1M]
#>        Date Passengers
#>       <mth>      <dbl>
#>  1 1949 Jan        112
#>  2 1949 Feb        118
#>  3 1949 Mar        132
#>  4 1949 Apr        129
#>  5 1949 May        121
#>  6 1949 Jun        135
#>  7 1949 Jul        148
#>  8 1949 Aug        148
#>  9 1949 Sep        136
#> 10 1949 Oct        119
#> # ... with 120 more rows

# Testing data
data_test <- data %>%
  slice((n_train + 1):(n_train + n_ahead))

data_test
#> # A tsibble: 12 x 2 [1M]
#>        Date Passengers
#>       <mth>      <dbl>
#>  1 1959 Nov        362
#>  2 1959 Dec        405
#>  3 1960 Jan        417
#>  4 1960 Feb        391
#>  5 1960 Mar        419
#>  6 1960 Apr        461
#>  7 1960 May        472
#>  8 1960 Jun        535
#>  9 1960 Jul        622
#> 10 1960 Aug        606
#> 11 1960 Sep        508
#> 12 1960 Oct        461
```

### Model

``` r
# Train models
models <- data_train %>%
  model(
    esn = ESN(Passengers),
    arima = ARIMA(Passengers),
    ets = ETS(Passengers))

models
#> # A mable: 1 x 3
#>   esn                         arima                              ets          
#>   <model>                     <model>                            <model>      
#> 1 <ESN({4,200,1}, {1,1.5,0})> <ARIMA(3,0,0)(0,1,0)[12] w/ drift> <ETS(M,Ad,M)>

# Detailed report of ESN
models %>%
  select(esn) %>%
  report()
#> Series: Passengers 
#> Model: ESN({4,200,1}, {1,1.5,0}) 
#> 
#> Network size: 
#>  Inputs        = 4 
#>  Reservoir     = 200 
#>  Outputs       = 1 
#> 
#> Model inputs: 
#>  Constant = TRUE 
#>  Lags     = 1 3 12 
#> 
#> Differences: 
#>  Seasonal     =  1 
#>  Non-seasonal =  0 
#> 
#> Hyperparameters: 
#>  alpha   = 1 
#>  rho     = 1.5 
#>  lambda  = 0 
#>  density = 0.1 
#> 
#> Metrics: 
#>  df  = 95.95 
#>  AIC = -14.98 
#>  BIC = -12.42 
#>  HQ  = -13.94
```

### Forecast

``` r
# Forecast models
fcsts <- models %>%
  forecast(h = n_ahead)

fcsts
#> # A fable: 36 x 4 [1M]
#> # Key:     .model [3]
#>    .model     Date Passengers .distribution  
#>    <chr>     <mth>      <dbl> <dist>         
#>  1 esn    1959 Nov       344. N(344, 5.9e-05)
#>  2 esn    1959 Dec       372. N(372, 6.7e-05)
#>  3 esn    1960 Jan       413. N(413, 6.2e-05)
#>  4 esn    1960 Feb       391. N(391, 6.0e-05)
#>  5 esn    1960 Mar       462. N(462, 1.5e-04)
#>  6 esn    1960 Apr       439. N(439, 5.0e-05)
#>  7 esn    1960 May       476. N(476, 1.3e-04)
#>  8 esn    1960 Jun       519. N(519, 3.9e-04)
#>  9 esn    1960 Jul       592. N(592, 4.6e-04)
#> 10 esn    1960 Aug       598. N(598, 2.3e-04)
#> # ... with 26 more rows
```

### Visualize

``` r
fcsts %>%
  autoplot(
    rbind(data_train, data_test),
    level = NULL,
    size = 1)
```

<img src="man/figures/README-plot-1.svg" width="100%" />

## Work in Progress

  - Implement specials
      - `const()` - intercept term
      - `lags()` - autoregressive lags
      - `fourier()` - fourier terms (trigonometric terms) for
        seasonality
      - `xreg()` - exogenuous regressors
  - Implement further functions
      - `refit.ESN()`
      - `generate.ESN()`
      - `stream.ESN()`
      - `reservoir.ESN()`
      - `plot_reservoir()`
      - …
  - Enhance `ESN()` to mulitvariate time series
