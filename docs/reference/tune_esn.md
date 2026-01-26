# Tune Hyperparameters of an Echo State Network

Tune hyperparameters of an Echo State Network (ESN) based on time series
cross-validation (i.e., rolling forecast). The input series is split
into `n_split` expanding-window train/test sets with test size
`n_ahead`. For each split and each hyperparameter combination
(`alpha, rho, tau`) an ESN is trained via
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)
and forecasts are generated via
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md).

## Usage

``` r
tune_esn(
  y,
  n_ahead = 12,
  n_split = 5,
  alpha = seq(0.1, 1, by = 0.1),
  rho = seq(0.1, 1, by = 0.1),
  tau = c(0.1, 0.2, 0.4),
  min_train = NULL,
  ...
)
```

## Arguments

- y:

  Numeric vector containing the response variable.

- n_ahead:

  Integer value. The number of periods for forecasting (i.e. forecast
  horizon).

- n_split:

  Integer value. The number of rolling train/test splits.

- alpha:

  Numeric vector. The candidate leakage rates (smoothing parameters).

- rho:

  Numeric vector. The candidate spectral radii.

- tau:

  Numeric vector. The candidate reservoir scaling values.

- min_train:

  Integer value. Minimum training sample size for the first split.

- ...:

  Further arguments passed to
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).

## Value

An object of class `"tune_esn"` (a list) with:

- `pars`: A `tibble` with one row per hyperparameter combination and
  split. Columns include `alpha`, `rho`, `tau`, `split`, `train_start`,
  `train_end`, `test_start`, `test_end`, `mse`, `mae`, and `id`.

- `fcst`: A numeric matrix of point forecasts with
  `nrow(fcst) == nrow(pars)` and `ncol(fcst) == n_ahead`.

- `actual`: The original input series `y` (numeric vector), returned for
  convenience.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`print.esn()`](https://ahaeusser.github.io/echos/reference/print.esn.md),
[`summary.esn()`](https://ahaeusser.github.io/echos/reference/summary.esn.md),
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)

## Examples

``` r
xdata <- as.numeric(AirPassengers)
fit <- tune_esn(
  y = xdata,
  n_ahead = 12,
  n_split = 5,
  alpha = c(0.2, 0.5, 1),
  rho   = c(0.5, 1.0),
  tau   = c(0.2, 0.4),
  inf_crit = "bic"
)

fit$pars
#> # A tibble: 60 × 11
#>    alpha   rho   tau split train_start train_end test_start test_end   mse   mae
#>    <dbl> <dbl> <dbl> <int>       <int>     <int>      <int>    <int> <dbl> <dbl>
#>  1   0.2   0.5   0.2     1           1        84         85       96 2079.  32.4
#>  2   0.2   0.5   0.2     2           1        96         97      108 4143.  47.5
#>  3   0.2   0.5   0.2     3           1       108        109      120 3236.  46.5
#>  4   0.2   0.5   0.2     4           1       120        121      132 5204.  50.6
#>  5   0.2   0.5   0.2     5           1       132        133      144 6725.  58.9
#>  6   0.2   0.5   0.4     1           1        84         85       96 2288.  35.2
#>  7   0.2   0.5   0.4     2           1        96         97      108 2756.  38.0
#>  8   0.2   0.5   0.4     3           1       108        109      120 3133.  47.2
#>  9   0.2   0.5   0.4     4           1       120        121      132 4009.  48.1
#> 10   0.2   0.5   0.4     5           1       132        133      144 5852.  54.9
#> # ℹ 50 more rows
#> # ℹ 1 more variable: id <int>
```
