# Synthetic time series data

A `tibble` with ten synthetic univariate time series. The dataset is
intended for examples, documentation, and tests in the `echos` package.
It contains deterministic wave patterns as well as stochastic benchmark
processes.

The dataset contains the following time series:

- Square Wave

- Sawtooth Wave

- Harmonic Wave

- Harmonic Wave w/ Trend

- Amplitude Modulated Wave

- Frequency Modulated Wave

- AR(1) Process

- MA(2) Process

- White Noise Process

- Random Walk Process

## Usage

``` r
data(synthetic_data)
```

## Format

An object of class `tibble` with 2,000 rows and 3 columns:

- `variable`: Unique identifier of the synthetic time series as
  `character`.

- `index`: Time index as `integer`.

- `value`: Observed value as `numeric`.

## Details

Each synthetic series contains 200 observations. The deterministic
series provide examples of common nonlinear and periodic patterns, while
the stochastic series provide simple autoregressive, moving-average,
white-noise, and random-walk benchmark processes.

## Examples

``` r
data(synthetic_data)
head(synthetic_data)
#> # A tibble: 6 × 3
#>   variable    index value
#>   <chr>       <int> <dbl>
#> 1 Square Wave     1     1
#> 2 Square Wave     2     1
#> 3 Square Wave     3     1
#> 4 Square Wave     4     1
#> 5 Square Wave     5     1
#> 6 Square Wave     6     1
```
