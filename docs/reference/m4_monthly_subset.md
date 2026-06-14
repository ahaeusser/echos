# Monthly subset of the M4 competition data

A `tsibble` with six monthly time series from the M4 Forecasting
Competition. The dataset is intended for examples, documentation, and
tests in the `echos` package. It is not the full M4 dataset.

The dataset contains the following time series:

- M21655 (Demographic), 1995 Jan - 2015 Mar

- M21683 (Demographic), 2000 Jan - 2023 Apr

- M2717 (Macro), 1996 Jan - 2016 Nov

- M28597 (Industry), 1996 Jan - 2016 Dec

- M42529 (Finance), 2001 Jan - 2009 Apr

- M4813 (Macro), 1994 Apr - 2006 May

## Usage

``` r
data(m4_monthly_subset)
```

## Format

A time series object of class `tsibble` with 1,152 rows and 4 columns:

- `series`: Unique identifier as `character` (key variable).

- `category`: Category, e.g. Demographic or Macro, as `factor`.

- `index`: Monthly date as `yearmonth` (index variable).

- `value`: Observed value as `numeric` (measurement variable).

## Source

[M4 Forecasting
Competition](https://github.com/Mcompetitions/M4-methods)

## Details

This dataset contains a small monthly subset of the M4 Forecasting
Competition data. It is included to demonstrate the tidy forecasting
interface of `echos` with `tsibble`, `fable`, and `fabletools`.

## Examples

``` r
data(m4_monthly_subset)
head(m4_monthly_subset)
#> # A tsibble: 6 x 4 [1M]
#> # Key:       series [1]
#>   series category       index value
#>   <chr>  <fct>          <mth> <dbl>
#> 1 M21655 Demographic 1995 Jan  4970
#> 2 M21655 Demographic 1995 Feb  5010
#> 3 M21655 Demographic 1995 Mrz  5060
#> 4 M21655 Demographic 1995 Apr  5010
#> 5 M21655 Demographic 1995 Mai  5610
#> 6 M21655 Demographic 1995 Jun  6040
```
