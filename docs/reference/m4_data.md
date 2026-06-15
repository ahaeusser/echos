# Deprecated name for `m4_monthly_subset`

`m4_data` has been renamed to `m4_monthly_subset`. The old name is
retained for backward compatibility with earlier versions of `echos`,
but users should prefer `m4_monthly_subset` in new code.

## Usage

``` r
data(m4_data)
```

## Format

See
[`m4_monthly_subset`](https://ahaeusser.github.io/echos/reference/m4_monthly_subset.md).

## See also

[`m4_monthly_subset`](https://ahaeusser.github.io/echos/reference/m4_monthly_subset.md)

## Examples

``` r
data(m4_data)
head(m4_data)
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
