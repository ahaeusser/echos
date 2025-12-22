# Checks if object is of class "forecast_esn"

Returns `TRUE` if the object is of class `forecast_esn`.

## Usage

``` r
is.forecast_esn(object)
```

## Arguments

- object:

  object to be tested.

## Value

Logical value. If `TRUE`, the object is of class `forecast_esn`.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`print.esn()`](https://ahaeusser.github.io/echos/reference/print.esn.md),
[`summary.esn()`](https://ahaeusser.github.io/echos/reference/summary.esn.md),
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
xfcst <- forecast_esn(xmodel, n_ahead = 12)
is.forecast_esn(xfcst)
#> [1] TRUE
```
