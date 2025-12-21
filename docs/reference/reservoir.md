# Return the reservoir from a trained ESN as tibble

Return the reservoir (internal states) from a trained ESN as tibble. The
function works only for models of class `ESN`.

## Usage

``` r
reservoir(object)
```

## Arguments

- object:

  An object of class `mdl_df`, containing an ESN model.

## Value

A tibble containing the reservoir (internal states).

## See also

Other tidy functions:
[`ESN()`](https://ahaeusser.github.io/echos/reference/ESN.md),
[`filter_esn()`](https://ahaeusser.github.io/echos/reference/filter_esn.md),
[`fitted.ESN()`](https://ahaeusser.github.io/echos/reference/fitted.ESN.md),
[`forecast.ESN()`](https://ahaeusser.github.io/echos/reference/forecast.ESN.md),
[`glance.ESN()`](https://ahaeusser.github.io/echos/reference/glance.ESN.md),
[`model_sum.ESN()`](https://ahaeusser.github.io/echos/reference/model_sum.ESN.md),
[`report.ESN()`](https://ahaeusser.github.io/echos/reference/report.ESN.md),
[`residuals.ESN()`](https://ahaeusser.github.io/echos/reference/residuals.ESN.md),
[`tidy.ESN()`](https://ahaeusser.github.io/echos/reference/tidy.ESN.md)

## Examples

``` r
library(tsibble)
library(fable)
AirPassengers %>%
as_tsibble() %>%
model("ESN" = ESN(value)) %>%
reservoir()
#> # A tibble: 8,094 × 4
#>    model index state        value
#>    <chr> <int> <chr>        <dbl>
#>  1 ESN       1 state(01)  0      
#>  2 ESN       2 state(01)  0.0463 
#>  3 ESN       3 state(01) -0.0106 
#>  4 ESN       4 state(01) -0.0719 
#>  5 ESN       5 state(01) -0.00112
#>  6 ESN       6 state(01)  0.0387 
#>  7 ESN       7 state(01) -0.0422 
#>  8 ESN       8 state(01) -0.131  
#>  9 ESN       9 state(01) -0.0888 
#> 10 ESN      10 state(01)  0.0239 
#> # ℹ 8,084 more rows
```
