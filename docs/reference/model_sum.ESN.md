# Model specification of a trained ESN model

Provides a compact overview of the model specification in the format
`ESN({n_states, alpha, rho}, {n_models, df})`.

## Usage

``` r
# S3 method for class 'ESN'
model_sum(x)
```

## Arguments

- x:

  An object of class `mdl_df`, containing an ESN model.

## Value

Model summary extracted from the object.

## See also

Other tidy functions:
[`ESN()`](https://ahaeusser.github.io/echos/reference/ESN.md),
[`filter_esn()`](https://ahaeusser.github.io/echos/reference/filter_esn.md),
[`fitted.ESN()`](https://ahaeusser.github.io/echos/reference/fitted.ESN.md),
[`forecast.ESN()`](https://ahaeusser.github.io/echos/reference/forecast.ESN.md),
[`glance.ESN()`](https://ahaeusser.github.io/echos/reference/glance.ESN.md),
[`report.ESN()`](https://ahaeusser.github.io/echos/reference/report.ESN.md),
[`reservoir()`](https://ahaeusser.github.io/echos/reference/reservoir.md),
[`residuals.ESN()`](https://ahaeusser.github.io/echos/reference/residuals.ESN.md),
[`tidy.ESN()`](https://ahaeusser.github.io/echos/reference/tidy.ESN.md)

## Examples

``` r
library(tsibble)
library(fable)
AirPassengers %>%
as_tsibble() %>%
model("ESN" = ESN(value))
#> # A mable: 1 x 1
#>                               ESN
#>                           <model>
#> 1 <ESN({57, 1, 1}, {114, 16.68})>
```
