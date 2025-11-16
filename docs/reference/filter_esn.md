# Filter ESN models

Filter an object of class `mdl_df` ("mable") to include ESN models only,
i.e., other models like ARIMA or ETS are excluded from the mable.

## Usage

``` r
filter_esn(object)
```

## Arguments

- object:

  An object of class `mdl_df`, containing an ESN model.

## Value

An object of class `mdl_df` in long-format.

## Examples

``` r
library(tsibble)
library(fable)
AirPassengers %>%
as_tsibble() %>%
model("ESN" = ESN(value)) %>%
filter_esn()
#> # A mable: 1 x 2
#> # Key:     model [1]
#>   model                           .spec
#>   <chr>                         <model>
#> 1 ESN   <ESN({57, 1, 1}, {114, 16.68})>
```
