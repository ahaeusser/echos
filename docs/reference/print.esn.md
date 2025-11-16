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

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
print(xmodel)
#> ESN({57, 1, 1}, {114, 16.68})
```
