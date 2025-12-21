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

## See also

Other tidy functions:
[`filter_esn()`](https://ahaeusser.github.io/echos/reference/filter_esn.md),
[`fitted.ESN()`](https://ahaeusser.github.io/echos/reference/fitted.ESN.md),
[`forecast.ESN()`](https://ahaeusser.github.io/echos/reference/forecast.ESN.md),
[`glance.ESN()`](https://ahaeusser.github.io/echos/reference/glance.ESN.md),
[`model_sum.ESN()`](https://ahaeusser.github.io/echos/reference/model_sum.ESN.md),
[`report.ESN()`](https://ahaeusser.github.io/echos/reference/report.ESN.md),
[`reservoir()`](https://ahaeusser.github.io/echos/reference/reservoir.md),
[`residuals.ESN()`](https://ahaeusser.github.io/echos/reference/residuals.ESN.md),
[`tidy.ESN()`](https://ahaeusser.github.io/echos/reference/tidy.ESN.md)

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
