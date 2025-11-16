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
