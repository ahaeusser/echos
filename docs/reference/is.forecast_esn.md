# Checks if object is of class "forecast_esn"

Returns `TRUE` if the object is of class "forecast_esn".

## Usage

``` r
is.forecast_esn(object)
```

## Arguments

- object:

  object to be tested.

## Value

Logical value. If `TRUE`, the object is of class "forecast_esn".

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
xfcst <- forecast_esn(xmodel, n_ahead = 12)
is.forecast_esn(xfcst)
#> [1] TRUE
```
