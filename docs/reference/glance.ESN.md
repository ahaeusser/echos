# Summary of trained models during random search

Return summary statistics from trained ESN models during random search
as `tibble`.

- `model`: Model identifier.

- `loglik`: Log-likelihood.

- `nobs`: Number of observations.

- `df`: Effective degrees of freedom.

- `lambda`: Regularization parameter.

- `aic`: Akaike Information Criterion.

- `aicc`: Corrected Akaike Information Criterion.

- `bic`: Bayesian Information Criterion.

- `hqc`: Hannan-Quinn Information Criterion.

- `mse`: Mean Squared Error.

- `mae`: Mean Absolute Error.

## Usage

``` r
# S3 method for class 'ESN'
glance(x, ...)
```

## Arguments

- x:

  An object of class `mdl_df`, containing an ESN model.

- ...:

  Currently not in use.

## Value

Summary statistics extracted from the object.

## See also

Other tidy functions:
[`ESN()`](https://ahaeusser.github.io/echos/reference/ESN.md),
[`filter_esn()`](https://ahaeusser.github.io/echos/reference/filter_esn.md),
[`fitted.ESN()`](https://ahaeusser.github.io/echos/reference/fitted.ESN.md),
[`forecast.ESN()`](https://ahaeusser.github.io/echos/reference/forecast.ESN.md),
[`model_sum.ESN()`](https://ahaeusser.github.io/echos/reference/model_sum.ESN.md),
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
model("ESN" = ESN(value)) %>%
glance()
#> # A tibble: 114 × 12
#>    .model model      loglik  nobs    df  lambda   aic  aicc   bic   hqc     mse
#>    <chr>  <chr>       <dbl> <int> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
#>  1 ESN    model(092)   189.   135  16.7 0.0457  -344. -339. -296. -325. 0.00357
#>  2 ESN    model(085)   198.   135  20.7 0.00912 -354. -346. -294. -330. 0.00312
#>  3 ESN    model(070)   186.   135  15.7 0.0704  -340. -335. -294. -321. 0.00374
#>  4 ESN    model(015)   185.   135  15.6 0.0757  -339. -334. -294. -320. 0.00378
#>  5 ESN    model(054)   184.   135  15.3 0.0872  -337. -333. -293. -319. 0.00385
#>  6 ESN    model(030)   183.   135  15.1 0.0927  -336. -332. -292. -318. 0.00388
#>  7 ESN    model(100)   182.   135  15.0 0.102   -335. -331. -291. -317. 0.00394
#>  8 ESN    model(112)   178.   135  14.2 0.146   -328. -324. -287. -311. 0.00418
#>  9 ESN    model(086)   178.   135  14.2 0.147   -328. -324. -287. -311. 0.00419
#> 10 ESN    model(028)   177.   135  14.0 0.157   -326. -323. -285. -310. 0.00424
#> # ℹ 104 more rows
#> # ℹ 1 more variable: mae <dbl>
```
