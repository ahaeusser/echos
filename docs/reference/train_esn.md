# Train an Echo State Network

Train an Echo State Network (ESN) to a univariate time series. The
function automatically manages data pre-processing, reservoir generation
(i.e., internal states) and model estimation and selection.

## Usage

``` r
train_esn(
  y,
  lags = 1,
  inf_crit = "bic",
  n_diff = NULL,
  n_states = NULL,
  n_models = NULL,
  n_initial = NULL,
  n_seed = 42,
  alpha = 1,
  rho = 1,
  tau = 0.4,
  density = 0.5,
  lambda = c(1e-04, 2),
  scale_win = 0.5,
  scale_wres = 0.5,
  scale_inputs = c(-0.5, 0.5)
)
```

## Arguments

- y:

  Numeric vector containing the response variable.

- lags:

  Integer vector with the lag(s) associated with the input variable.

- inf_crit:

  Character value. The information criterion used for variable selection
  `inf_crit = c("aic", "aicc", "bic", "hqc")`.

- n_diff:

  Integer vector. The nth-differences of the response variable.

- n_states:

  Integer value. The number of internal states of the reservoir. If
  `n_states = NULL`, the reservoir size is determined by `tau*n_total`,
  where `n_total` is the time series length.

- n_models:

  Integer value. The maximum number of (random) models to train for
  model selection. If `n_models = NULL`, the number of models is defined
  as `n_states*2`.

- n_initial:

  Integer value. The number of observations of internal states for
  initial drop out (throw-off). If `n_initial = NULL`, the throw-off is
  defined as `n_total*0.05`, where `n_total` is the time series length.

- n_seed:

  Integer value. The seed for the random number generator (for
  reproducibility).

- alpha:

  Numeric value. The leakage rate (smoothing parameter) applied to the
  reservoir (value greater than 0 and less than or equal to 1).

- rho:

  Numeric value. The spectral radius for scaling the reservoir weight
  matrix (value often between 0 and 1, but values above 1 are possible).

- tau:

  Numeric value. The reservoir scaling parameter to determine the
  reservoir size based on the time series length (value greater than 0
  and less than or equal to 1).

- density:

  Numeric value. The connectivity of the reservoir weight matrix (dense
  or sparse) (value greater than 0 and less than or equal to 1).

- lambda:

  Numeric vector. Lower and upper bound of lambda sequence for ridge
  regression (numeric vector of length 2 with both values greater than 0
  and `lambda[1]` \< `lambda[2]`).

- scale_win:

  Numeric value. The lower and upper bound of the uniform distribution
  for scaling the input weight matrix (value greater than 0, weights are
  sampled from U(-`scale_win`, `scale_win`)).

- scale_wres:

  Numeric value. The lower and upper bound of the uniform distribution
  for scaling the reservoir weight matrix (value greater than 0, weights
  are sampled from U(-`scale_res`, `scale_res`) before applying `rho`
  and `density`).

- scale_inputs:

  Numeric vector. The lower and upper bound for scaling the time series
  data (numeric vector of length 2 with `scale_inputs[1]` \<
  `scale_inputs[2]` (often symmetric, e.g., `c(-0.5, 0.5)` or
  `c(-1, 1)`).

## Value

A `list` containing:

- `actual`: Numeric vector containing the actual values.

- `fitted`: Numeric vector containing the fitted values.

- `resid`: Numeric vector containing the residuals.

- `states_train`: Numeric matrix containing the internal states.

- `method`: A `list` containing several objects and meta information of
  the trained ESN (weight matrices, hyperparameters, model metrics,
  etc.).

## References

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
[`tune_esn()`](https://ahaeusser.github.io/echos/reference/tune_esn.md)

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
summary(xmodel)
#> 
#> --- Inputs -----------------------------------------------------
#> n_obs        = 144
#> n_diff       = 1
#> lags         = 1
#> 
#> --- Reservoir generation ---------------------------------------
#> n_states     = 57
#> alpha        = 1
#> rho          = 1
#> density      = 0.5
#> scale_inputs = [-0.5, 0.5]
#> scale_win    = [-0.5, 0.5]
#> scale_wres   = [-0.5, 0.5]
#> 
#> --- Model selection --------------------------------------------
#> n_models     = 114
#> df           = 16.68
#> lambda       = 0.0457
```
