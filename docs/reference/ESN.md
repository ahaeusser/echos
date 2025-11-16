# Train an Echo State Network

Train an Echo State Network (ESN) to a univariate time series. The
function automatically manages data pre-processing, reservoir generation
(i.e., internal states) and model estimation and selection. The function
is a wrapper for
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)
and intended to be used in combination with
[`fabletools::model()`](https://fabletools.tidyverts.org/reference/model.html).

## Usage

``` r
ESN(formula, ...)
```

## Arguments

- formula:

  Model specification (currently not in use).

- ...:

  Further arguments passed to
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).

## Value

An object of class `ESN`.

## Examples

``` r
library(tsibble)
#> 
#> Attaching package: 'tsibble'
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, union
library(fable)
AirPassengers %>%
as_tsibble() %>%
model("ESN" = ESN(value))
#> # A mable: 1 x 1
#>                               ESN
#>                           <model>
#> 1 <ESN({57, 1, 1}, {114, 16.68})>
```
