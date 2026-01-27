# Plot forecasts from a tuned ESN object

Plot actual values and the point forecasts from the best hyperparameter
combination selected via `tune.esn()` using the selected accuracy
metric. Forecasts are shown as separate line segments for each test
split, with vertical dashed lines marking the starts of test windows.

## Usage

``` r
# S3 method for class 'tune_esn'
plot(x, metric = "mse", ...)
```

## Arguments

- x:

  An object of class `tune_esn`. The result of a call to
  [`tune_esn()`](https://ahaeusser.github.io/echos/reference/tune_esn.md).

- metric:

  Character value. The metric used to select the best hyperparameter
  combination (`metric = c("mse", "mae")`.

- ...:

  Further arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Line chart of point forecast and actual values.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md),
[`is.tune_esn()`](https://ahaeusser.github.io/echos/reference/is.tune_esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`print.esn()`](https://ahaeusser.github.io/echos/reference/print.esn.md),
[`summary.esn()`](https://ahaeusser.github.io/echos/reference/summary.esn.md),
[`summary.tune_esn()`](https://ahaeusser.github.io/echos/reference/summary.tune_esn.md),
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md),
[`tune_esn()`](https://ahaeusser.github.io/echos/reference/tune_esn.md)

## Examples

``` r
xdata <- as.numeric(AirPassengers)
fit <- tune_esn(
  y = xdata,
  n_ahead = 12,
  n_split = 5,
  alpha = c(0.5, 1),
  rho   = c(1.0),
  tau   = c(0.4),
  inf_crit = "bic"
)

plot(fit)

```
