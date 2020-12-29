
<!-- README.md is generated from README.Rmd. Please edit that file -->

# echos

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
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

## Usage

### Load packages

``` r
library(echos)
library(tscv)
library(dplyr)
library(tsibble)
library(fabletools)
library(fable)
```

### Prepare data

``` r

# Prepare dataset
data <- elec_price %>%
  tscv::clean_data()

# Setup for time series cross validation
n_init <- 2400   # size for training window
n_ahead <- 24    # size for testing window (forecast horizon)
mode <- "slide"  # fixed window approach
n_skip <- 23     # skip 23 observations
n_lag <- 0       # no lag

data <- data %>%
  tscv::split_data(
    n_init = n_init,
    n_ahead = n_ahead,
    mode = mode,
    n_skip = n_skip,
    n_lag = n_lag)

# Use only a small sample of data
data <- data %>%
  filter(BZN == "DE") %>%
  filter(split == 10)

data
#> # A tsibble: 2,424 x 9 [1h] <UTC>
#> # Key:       Series, Unit, BZN, split [1]
#>    Time                Series      Unit   BZN   Value split    id sample horizon
#>    <dttm>              <chr>       <chr>  <chr> <dbl> <int> <int> <chr>    <int>
#>  1 2019-01-10 00:00:00 Day-ahead ~ [EUR/~ DE     49.1    10   217 train       NA
#>  2 2019-01-10 01:00:00 Day-ahead ~ [EUR/~ DE     47.1    10   218 train       NA
#>  3 2019-01-10 02:00:00 Day-ahead ~ [EUR/~ DE     48.8    10   219 train       NA
#>  4 2019-01-10 03:00:00 Day-ahead ~ [EUR/~ DE     48.5    10   220 train       NA
#>  5 2019-01-10 04:00:00 Day-ahead ~ [EUR/~ DE     49.8    10   221 train       NA
#>  6 2019-01-10 05:00:00 Day-ahead ~ [EUR/~ DE     57.0    10   222 train       NA
#>  7 2019-01-10 06:00:00 Day-ahead ~ [EUR/~ DE     72.6    10   223 train       NA
#>  8 2019-01-10 07:00:00 Day-ahead ~ [EUR/~ DE     81.2    10   224 train       NA
#>  9 2019-01-10 08:00:00 Day-ahead ~ [EUR/~ DE     81.8    10   225 train       NA
#> 10 2019-01-10 09:00:00 Day-ahead ~ [EUR/~ DE     81.2    10   226 train       NA
#> # ... with 2,414 more rows
```

### Model

``` r
# Train models
models <- data %>%
  filter(sample == "train") %>%
  model(
    "ESN" = ESN(Value, lags = list(c(1, 2, 3, 24, 168))),
    "sNaive" = SNAIVE(Value ~ lag("week")))

models
#> # A mable: 1 x 6
#> # Key:     Series, Unit, BZN, split [1]
#>   Series         Unit     BZN   split                               ESN   sNaive
#>   <chr>          <chr>    <chr> <int>                           <model>  <model>
#> 1 Day-ahead Pri~ [EUR/MW~ DE       10 <ESN({6,200,1}, {0.54,0.78,0.1})> <SNAIVE>

# Detailed report of ESN for split 10
models %>%
  select(ESN) %>%
  report()
#> Series: Value 
#> Model: ESN({6,200,1}, {0.54,0.78,0.1}) 
#> 
#> Network size: 
#>  Inputs        =  6 
#>  Reservoir     =  200 
#>  Outputs       =  1 
#> 
#> Model inputs: 
#>  Constant =  TRUE 
#>  Lags     =  1 2 3 24 168 
#> 
#> Differences =  1 
#> 
#> Scaling: 
#>  Inputs         = (-1, 1)
#>  Random uniform = (-0.5, 0.5)
#> 
#> Hyperparameters: 
#>  alpha   = 0.54 
#>  rho     = 0.78 
#>  lambda  = 0.1 
#>  density = 0.1 
#> 
#> Metrics: 
#>  df  = 41.3 
#>  aic = -4.46 
#>  bic = -4.35 
#>  hq  = -4.42
```

### Forecast

``` r
# Forecast models
fcst <- models %>%
  forecast(h = n_ahead)

fcst
#> # A fable: 48 x 8 [1h] <UTC>
#> # Key:     Series, Unit, BZN, split, .model [2]
#>    Series        Unit    BZN   split .model Time                     Value .mean
#>    <chr>         <chr>   <chr> <int> <chr>  <dttm>                  <dist> <dbl>
#>  1 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 00:00:00  N(31, 13)  30.9
#>  2 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 01:00:00  N(31, 23)  30.9
#>  3 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 02:00:00  N(32, 35)  31.9
#>  4 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 03:00:00  N(34, 51)  34.1
#>  5 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 04:00:00  N(37, 60)  37.2
#>  6 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 05:00:00  N(39, 85)  39.3
#>  7 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 06:00:00 N(41, 101)  40.8
#>  8 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 07:00:00  N(40, 98)  39.9
#>  9 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 08:00:00  N(36, 99)  36.5
#> 10 Day-ahead Pr~ [EUR/M~ DE       10 ESN    2019-04-20 09:00:00  N(33, 91)  33.3
#> # ... with 38 more rows
```

### Visualize

``` r
tscv::plot_forecast(
  fcst = fcst,         # forecasts as fable
  data = data,         # training and test data
  split = 10,          # only split 10 is visualized
  include = 48)        # plot only the last 48 observations of training data
```

<img src="man/figures/README-plot-1.svg" width="100%" />

## Work in Progress

  - Implement further functions
      - `refit.ESN()`
      - `generate.ESN()`
      - `stream.ESN()`
      - `reservoir.ESN()`
      - `plot_reservoir()`
      - …
  - Enhance `ESN()` to multivariate time series
