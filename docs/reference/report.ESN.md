# Provide a detailed summary of the trained ESN model

Provide a detailed summary of the trained ESN model. The function is a
wrapper for
[`summary.esn()`](https://ahaeusser.github.io/echos/reference/summary.esn.md).

## Usage

``` r
# S3 method for class 'ESN'
report(object, ...)
```

## Arguments

- object:

  An object of class `mdl_df`, containing an ESN model.

- ...:

  Currently not in use.

## Value

Print detailed model summary.

## See also

Other tidy functions:
[`ESN()`](https://ahaeusser.github.io/echos/reference/ESN.md),
[`filter_esn()`](https://ahaeusser.github.io/echos/reference/filter_esn.md),
[`fitted.ESN()`](https://ahaeusser.github.io/echos/reference/fitted.ESN.md),
[`forecast.ESN()`](https://ahaeusser.github.io/echos/reference/forecast.ESN.md),
[`glance.ESN()`](https://ahaeusser.github.io/echos/reference/glance.ESN.md),
[`model_sum.ESN()`](https://ahaeusser.github.io/echos/reference/model_sum.ESN.md),
[`reservoir()`](https://ahaeusser.github.io/echos/reference/reservoir.md),
[`residuals.ESN()`](https://ahaeusser.github.io/echos/reference/residuals.ESN.md),
[`tidy.ESN()`](https://ahaeusser.github.io/echos/reference/tidy.ESN.md)

## Examples

``` r
library(tsibble)
library(fable)
AirPassengers %>%
as_tsibble() %>%
model("ESN" = ESN(value)) %>%
report()
#> Series: value 
#> Model: ESN({57, 1, 1}, {114, 16.68}) 
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
