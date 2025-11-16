# Plot internal states of a trained ESN model

Plot internal states (i.e., the reservoir) of a trained ESN model as
line chart.

## Usage

``` r
# S3 method for class 'esn'
plot(x, ...)
```

## Arguments

- x:

  An object of class `esn`. The result of a call to
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).

- ...:

  Further arguments passed to
  [`matplot()`](https://rdrr.io/r/graphics/matplot.html).

## Value

Line chart of internal states.

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
plot(xmodel)

```
