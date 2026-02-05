# Tune hyperparameters of an Echo State Network

Tune hyperparameters of an Echo State Network (ESN) based on time series
cross-validation (i.e., rolling forecast). The input series is split
into `n_split` expanding-window train/test sets with test size
`n_ahead`. For each split and each hyperparameter combination
(`alpha, rho, tau`) an ESN is trained via
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)
and forecasts are generated via
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md).

## Usage

``` r
tune_esn(
  y,
  n_ahead = 12,
  n_split = 5,
  alpha = seq(0.1, 1, by = 0.1),
  rho = seq(0.1, 1, by = 0.1),
  tau = c(0.1, 0.2, 0.4),
  min_train = NULL,
  ...
)
```

## Arguments

- y:

  Numeric vector containing the response variable.

- n_ahead:

  Integer value. The number of periods for forecasting (i.e. forecast
  horizon).

- n_split:

  Integer value. The number of rolling train/test splits.

- alpha:

  Numeric vector. The candidate leakage rates (smoothing parameters).

- rho:

  Numeric vector. The candidate spectral radii.

- tau:

  Numeric vector. The candidate reservoir scaling values.

- min_train:

  Integer value. Minimum training sample size for the first split.

- ...:

  Further arguments passed to
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).

## Value

An object of class `"tune_esn"` (a list) with:

- `pars`: A `tibble` with one row per hyperparameter combination and
  split. Columns include `alpha`, `rho`, `tau`, `split`, `train_start`,
  `train_end`, `test_start`, `test_end`, `mse`, `mae`, and `id`.

- `fcst`: A numeric matrix of point forecasts with
  `nrow(fcst) == nrow(pars)` and `ncol(fcst) == n_ahead`.

- `actual`: The original input series `y` (numeric vector), returned for
  convenience.

## References

- Häußer, A. (2026). Echo State Networks for Time Series Forecasting:
  Hyperparameter Sweep and Benchmarking. arXiv preprint
  arXiv:2602.03912, 2026. <https://arxiv.org/abs/2602.03912>

- Jaeger, H. (2001). The “echo state” approach to analysing and training
  recurrent neural networks with an erratum note. Bonn, Germany: German
  National Research Center for Information Technology GMD Technical
  Report, 148(34):13.

- Jaeger, H. (2002). Tutorial on training recurrent neural networks,
  covering BPPT, RTRL, EKF and the "echo state network" approach.

- Lukosevicius, M. (2012). A practical guide to applying echo state
  networks. In Neural Networks: Tricks of the Trade: Second Edition,
  pages 659–686. Springer.

- Lukosevicius, M. and Jaeger, H. (2009). Reservoir computing approaches
  to recurrent neural network training. Computer Science Review,
  3(3):127–149.

## See also

Other base functions:
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
[`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md),
[`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md),
[`is.tune_esn()`](https://ahaeusser.github.io/echos/reference/is.tune_esn.md),
[`plot.esn()`](https://ahaeusser.github.io/echos/reference/plot.esn.md),
[`plot.forecast_esn()`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md),
[`plot.tune_esn()`](https://ahaeusser.github.io/echos/reference/plot.tune_esn.md),
[`print.esn()`](https://ahaeusser.github.io/echos/reference/print.esn.md),
[`summary.esn()`](https://ahaeusser.github.io/echos/reference/summary.esn.md),
[`summary.tune_esn()`](https://ahaeusser.github.io/echos/reference/summary.tune_esn.md),
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)

## Examples

``` r
xdata <- as.numeric(AirPassengers)
fit <- tune_esn(
  y = xdata,
  n_ahead = 12,
  n_split = 5,
  alpha = c(0.5, 1),
  rho   = c(1.0),
  tau   = c(0.4),
  inf_crit = "bic"
)

fit$pars
#> # A tibble: 10 × 11
#>    alpha   rho   tau split train_start train_end test_start test_end   mse   mae
#>    <dbl> <dbl> <dbl> <int>       <int>     <int>      <int>    <int> <dbl> <dbl>
#>  1   0.5     1   0.4     1           1        84         85       96  769.  22.3
#>  2   0.5     1   0.4     2           1        96         97      108 1267.  28.3
#>  3   0.5     1   0.4     3           1       108        109      120 1276.  31.0
#>  4   0.5     1   0.4     4           1       120        121      132 1035.  24.3
#>  5   0.5     1   0.4     5           1       132        133      144 1038.  23.9
#>  6   1       1   0.4     1           1        84         85       96  471.  19.5
#>  7   1       1   0.4     2           1        96         97      108  376.  14.2
#>  8   1       1   0.4     3           1       108        109      120  526.  19.0
#>  9   1       1   0.4     4           1       120        121      132  547.  20.2
#> 10   1       1   0.4     5           1       132        133      144  396.  17.0
#> # ℹ 1 more variable: id <int>
```
