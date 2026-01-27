# Provide a summary of the hyperparameter tuning

Provide a summary of the tuned hyperparameters `alpha`, `rho` and `tau`.

## Usage

``` r
# S3 method for class 'tune_esn'
summary(object, metric = "mse", ...)
```

## Arguments

- object:

  An object of class `tune_esn`. The result of a call to
  [`tune_esn()`](https://ahaeusser.github.io/echos/reference/tune_esn.md).

- metric:

  Character value. The metric used to select the best hyperparameter
  combination (`metric = c("mse", "mae")`.

- ...:

  Currently not in use.

## Value

Print detailed model summary.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md),
[`is.tune_esn()`](https://ahaeusser.github.io/echos/reference/is.tune_esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`plot.tune_esn()`](https://ahaeusser.github.io/echos/reference/plot.tune_esn.md),
[`print.esn()`](https://ahaeusser.github.io/echos/reference/print.esn.md),
[`summary.esn()`](https://ahaeusser.github.io/echos/reference/summary.esn.md),
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

summary(fit)
#> # A tibble: 5 × 11
#>   alpha   rho   tau split train_start train_end test_start test_end   mse   mae
#>   <dbl> <dbl> <dbl> <int>       <int>     <int>      <int>    <int> <dbl> <dbl>
#> 1     1     1   0.4     1           1        84         85       96  471.  19.5
#> 2     1     1   0.4     2           1        96         97      108  376.  14.2
#> 3     1     1   0.4     3           1       108        109      120  526.  19.0
#> 4     1     1   0.4     4           1       120        121      132  547.  20.2
#> 5     1     1   0.4     5           1       132        133      144  396.  17.0
#> # ℹ 1 more variable: id <int>
```
