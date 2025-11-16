# M4 dataset

`tsibble` with six monthly time series from the M4 Forecasting
Competition. The datasets contains the following time series:

- M21655 (Demographic), 1995 Jan - 2015 Mar

- M21683 (Demographic), 2000 Jan - 2023 Apr

- M2717 (Macro), 1996 Jan - 2016 Nov

- M28597 (Industry), 1996 Jan - 2016 Dec

- M42529 (Finance), 2001 Jan - 2009 Apr

- M4813 (Macro), 1994 Apr - 2006 May

## Usage

``` r
data(m4_data)
```

## Format

A time series object of class `tsibble` with 1.152 rows and 4 columns:

- `series`: Unique identifier as `character` (key variable).

- `category`: Category (e.g., Demographic, Macro) as `factor`.

- `index`: Date as `yearmonth` (index variable).

- `value`: Value as `numeric` (measurement variable).

## Source

[M4 Forecasting
Competition](https://github.com/Mcompetitions/M4-methods)

## Examples

``` r
data(m4_data)
```
