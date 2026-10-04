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

  Numeric vector containing the response variable (no missing values).

- n_ahead:

  Integer value. The number of periods for forecasting (i.e. forecast
  horizon).

- n_split:

  Integer value. The number of rolling train/test splits.

- alpha:

  Numeric vector of candidate leakage rates. Smaller values produce
  slower reservoir state updates; larger values make the reservoir react
  more strongly to recent inputs.

- rho:

  Numeric vector of candidate spectral radii. This parameter scales the
  recurrent reservoir weights and affects reservoir memory and
  stability.

- tau:

  Numeric vector of candidate reservoir scaling values. Used to
  determine the reservoir size when `n_states = NULL` in
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).

- min_train:

  Integer value. Minimum training sample size for the first split.

- ...:

  Further arguments passed to
  [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)
  (except `alpha`, `rho`, and `tau`, which are set by the tuning grid).

## Value

An object of class `"tune_esn"` (a list) with:

- `pars`: A `tibble` with one row per hyperparameter combination and
  split. Columns include `alpha`, `rho`, `tau`, `split`, `train_start`,
  `train_end`, `test_start`, `test_end`, `mse`, `mae`, and `id`.

- `fcst`: A numeric matrix of point forecasts with
  `nrow(fcst) == nrow(pars)` and `ncol(fcst) == n_ahead`.

- `actual`: The original input series `y` (numeric vector), returned for
  convenience.

## Details

`tune_esn()` performs grid-based hyperparameter tuning using
expanding-window time series cross-validation. The tuning grid is formed
from all combinations of `alpha`, `rho`, and `tau`. For each split, the
model is trained on observations from the beginning of the series up to
the split-specific training endpoint and evaluated on the following
`n_ahead` observations.

For every candidate configuration and split,
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)
is called with the corresponding `alpha`, `rho`, and `tau`; all other
arguments supplied through `...` are passed to
[`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md).
Forecasts are generated with
[`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md),
and the mean squared error (`mse`) and mean absolute error (`mae`) are
stored. The accompanying
[`summary()`](https://rdrr.io/r/base/summary.html) and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods use
these stored errors to identify and display the best-performing
hyperparameter configuration.

Runtime increases approximately linearly with the number of grid
combinations and the number of validation splits. Users should start
with coarse grids and increase the grid resolution only where needed.

## References

- Häußer, A. (2027). Echo State Networks for Time Series Forecasting:
  Hyperparameter Sweep and Benchmarking. Applied Soft Computing,
  204, 116346.
  [doi:10.1016/j.asoc.2026.116346](https://doi.org/10.1016/j.asoc.2026.116346)

- Häußer, A. (2026). echos: An R Package for Automatic Time Series
  Forecasting using Echo State Networks. The R Journal, 18(3), 120–143.
  [doi:10.32614/RJ-2026-040](https://doi.org/10.32614/RJ-2026-040)

- Jaeger, H. (2001). The “echo state” approach to analysing and training
  recurrent neural networks. GMD Report 148, German National Research
  Center for Information Technology.

- Jaeger, H. (2002). Tutorial on training recurrent neural networks,
  covering BPTT, RTRL, EKF and the "echo state network" approach. GMD
  Report 159, German National Research Center for Information
  Technology.

- Lukosevicius, M. (2012). A practical guide to applying echo state
  networks. In Neural Networks: Tricks of the Trade, 2nd edition, pages
  659–686. Springer.
  [doi:10.1007/978-3-642-35289-8_36](https://doi.org/10.1007/978-3-642-35289-8_36)

- Lukosevicius, M. and Jaeger, H. (2009). Reservoir computing approaches
  to recurrent neural network training. Computer Science Review,
  3(3):127–149.
  [doi:10.1016/j.cosrev.2009.03.005](https://doi.org/10.1016/j.cosrev.2009.03.005)

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
summary(fit)
#> # A tibble: 5 × 11
#>   alpha   rho   tau split train_start train_end test_start test_end   mse   mae
#>   <dbl> <dbl> <dbl> <int>       <int>     <int>      <int>    <int> <dbl> <dbl>
#> 1     1     1   0.4     1           1        84         85       96  471.  19.5
#> 2     1     1   0.4     2           1        96         97      108  376.  14.2
#> 3     1     1   0.4     3           1       108        109      120  526.  19.0
#> 4     1     1   0.4     4           1       120        121      132  547.  20.2
#> 5     1     1   0.4     5           1       132        133      144  396.  17.0
#> # ℹ 1 more variable: id <int>
plot(fit)

```
