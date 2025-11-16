# Synthetic data

`tibble` with ten synthetic time series. The dataset contains the
following time series:

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

An object of class `tibble` with 2.000 rows and 3 columns:

- `variable`: Unique identifier as `character` (key variable).

- `index`: Index as `integer` (index variable).

- `value`: Value as `numeric` (measurement variable).

## Examples

``` r
data(synthetic_data)
```
