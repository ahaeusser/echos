# Tune Hyperparameters of an Echo State Network

Tune hyperparameters of an Echo State Network (ESN) based on rolling
out-of-sample forecast accuracy. The input series is split into
`n_split` expanding-window train/test sets with test size `n_ahead`. For
each split and each hyperparameter combination (`alpha, rho, tau`) an
ESN is trained via
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

A `tibble` with one row per hyperparameter combination and split. The
tibble contains the following columns:

- `alpha`: Numeric value. Leakage rate of the reservoir (smoothing
  parameter).

- `rho`: Numeric value. Spectral radius used to scale the reservoir
  weight matrix.

- `tau`: Numeric value. Reservoir scaling parameter used to determine
  reservoir size.

- `split`: Integer value. Index of the rolling train/test split.

- `train_start`: Integer value. Start index of the training window.

- `train_end`: Integer value. End index of the training window.

- `test_start`: Integer value. Start index of the test window.

- `test_end`: Integer value. End index of the test window.

- `mse`: Numeric value. Mean squared error on the test window
  (out-of-sample).

- `mae`: Numeric value. Mean absolute error on the test window
  (out-of-sample).

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
pars <- tune_esn(
  y = xdata,
  n_ahead = 12,
  n_split = 5,
  alpha = c(0.2, 0.5, 1),
  rho   = c(0.5, 1.0),
  tau   = c(0.2, 0.4),
  inf_crit = "bic"
)
```
