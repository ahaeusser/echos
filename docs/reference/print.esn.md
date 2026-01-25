# Print model specification of the trained ESN model

Provides a compact overview of the model specification in the format
`ESN({n_states, alpha, rho}, {n_models, df})`.

## Usage

``` r
# S3 method for class 'esn'
print(x, ...)
```

## Arguments

- x:

  An object of class `esn`. The result of a call to
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).

- ...:

  Currently not in use.

## Value

Print specification of the trained ESN model.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`summary.esn()`](https://ahaeusser.github.io/echos/reference/summary.esn.md),
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md),
[`tune_esn()`](https://ahaeusser.github.io/echos/reference/tune_esn.md)

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
print(xmodel)
#> ESN({57, 1, 1}, {114, 16.68})
```
