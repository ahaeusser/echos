# Plot forecasts of a trained ESN model

Plot point forecasts and forecast intervals, actual values of a trained
ESN model. Optionally, test data (out-of-sample) and fitted values can
be added to the plot.

## Usage

``` r
# S3 method for class 'forecast_esn'
plot(x, test = NULL, fitted = TRUE, interval = TRUE, n_obs = NULL, ...)
```

## Arguments

- x:

  An object of class `forecast_esn`. The result of a call to
  [`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md).

- test:

  Numeric vector. Test data, i.e., out-of-sample actual values.

- fitted:

  Logical value. If `TRUE`, fitted values are added.

- interval:

  Logical value. If `TRUE`, forecast intervals are added.

- n_obs:

  Integer value. If `NULL`, all in-sample values are shown, otherwise
  only the last `n_obs`.

- ...:

  Currently not in use.

## Value

Line chart of point forecast and actual values.

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
xfcst <- forecast_esn(xmodel, n_ahead = 12)
plot(xfcst)

```
