# Estimated coefficients

Return the estimated coefficients from a trained ESN as `tibble`.

## Usage

``` r
# S3 method for class 'ESN'
tidy(x, ...)
```

## Arguments

- x:

  An object of class `mdl_df`, containing an ESN model.

- ...:

  Currently not in use.

## Value

Coefficients extracted from the object.

## Examples

``` r
library(tsibble)
library(fable)
AirPassengers %>%
as_tsibble() %>%
model("ESN" = ESN(value)) %>%
tidy()
#> # A tibble: 58 × 3
#>    .model term        estimate
#>    <chr>  <chr>          <dbl>
#>  1 ESN    (Intercept)  0.161  
#>  2 ESN    state(01)   -0.128  
#>  3 ESN    state(02)    0.00831
#>  4 ESN    state(03)   -0.0973 
#>  5 ESN    state(04)    0.0945 
#>  6 ESN    state(05)    0.0159 
#>  7 ESN    state(06)    0.101  
#>  8 ESN    state(07)   -0.185  
#>  9 ESN    state(08)    0.308  
#> 10 ESN    state(09)   -0.135  
#> # ℹ 48 more rows
```
