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
