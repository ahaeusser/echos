# Extract residuals from a trained ESN

Extract residuals from a trained ESN as `tsibble`.

## Usage

``` r
# S3 method for class 'ESN'
residuals(object, ...)
```

## Arguments

- object:

  An object of class `mdl_df`, containing an ESN model.

- ...:

  Currently not in use.

## Value

Residuals extracted from the object.

## See also

Other tidy functions:
[`ESN()`](https://ahaeusser.github.io/echos/reference/ESN.md),
[`filter_esn()`](https://ahaeusser.github.io/echos/reference/filter_esn.md),
[`fitted.ESN()`](https://ahaeusser.github.io/echos/reference/fitted.ESN.md),
[`forecast.ESN()`](https://ahaeusser.github.io/echos/reference/forecast.ESN.md),
[`glance.ESN()`](https://ahaeusser.github.io/echos/reference/glance.ESN.md),
[`model_sum.ESN()`](https://ahaeusser.github.io/echos/reference/model_sum.ESN.md),
[`report.ESN()`](https://ahaeusser.github.io/echos/reference/report.ESN.md),
[`reservoir()`](https://ahaeusser.github.io/echos/reference/reservoir.md),
[`tidy.ESN()`](https://ahaeusser.github.io/echos/reference/tidy.ESN.md)

## Examples

``` r
library(tsibble)
library(fable)
AirPassengers %>%
as_tsibble() %>%
model("ESN" = ESN(value)) %>%
residuals()
#> # A tsibble: 144 x 3 [1M]
#> # Key:       .model [1]
#>    .model    index .resid
#>    <chr>     <mth>  <dbl>
#>  1 ESN    1949 Jan   NA  
#>  2 ESN    1949 Feb   NA  
#>  3 ESN    1949 Mrz   NA  
#>  4 ESN    1949 Apr   NA  
#>  5 ESN    1949 Mai   NA  
#>  6 ESN    1949 Jun   NA  
#>  7 ESN    1949 Jul   NA  
#>  8 ESN    1949 Aug   NA  
#>  9 ESN    1949 Sep   NA  
#> 10 ESN    1949 Okt  -20.8
#> # ℹ 134 more rows
```
