# Provide a detailed summary of the trained ESN model

Provide a detailed summary of the trained ESN model.

## Usage

``` r
# S3 method for class 'esn'
summary(object, ...)
```

## Arguments

- object:

  An object of class `esn`. The result of a call to
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).

- ...:

  Currently not in use.

## Value

Print detailed model summary.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`print.esn()`](https://ahaeusser.github.io/echos/reference/print.esn.md),
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md),
[`tune_esn()`](https://ahaeusser.github.io/echos/reference/tune_esn.md)

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
summary(xmodel)
#> 
#> --- Inputs -----------------------------------------------------
#> n_obs        = 144
#> n_diff       = 1
#> lags         = 1
#> 
#> --- Reservoir generation ---------------------------------------
#> n_states     = 57
#> alpha        = 1
#> rho          = 1
#> density      = 0.5
#> scale_inputs = [-0.5, 0.5]
#> scale_win    = [-0.5, 0.5]
#> scale_wres   = [-0.5, 0.5]
#> 
#> --- Model selection --------------------------------------------
#> n_models     = 114
#> df           = 16.68
#> lambda       = 0.0457
```
