# Checks if object is of class "tune_esn"

Returns `TRUE` if the object is of class `tune_esn`.

## Usage

``` r
is.tune_esn(object)
```

## Arguments

- object:

  object to be tested.

## Value

Logical value. If `TRUE`, the object is of class `tune_esn`.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`plot.tune_esn()`](https://ahaeusser.github.io/echos/reference/plot.tune_esn.md),
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
is.tune_esn(fit)
#> [1] TRUE
```
