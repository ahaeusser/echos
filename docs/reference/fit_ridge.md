# Estimate a linear model via ridge regression

Estimate a linear model via Ordinary Least Squares (OLS). `fit_lm()` is
a wrapper function for
[`stats::lm.fit()`](https://rdrr.io/r/stats/lmfit.html) with some
additional output like goodness-of-fit metrics (e.g. information
criteria). The function is slightly faster than
[`stats::lm()`](https://rdrr.io/r/stats/lm.html), bit most importantly,
the resulting object size is much smaller.

## Usage

``` r
fit_ridge(x, y, lambda)
```

## Arguments

- x:

  Numeric matrix. The design matrix containing the predictor variables.

- y:

  Numeric matrix. The response variable(s).

- lambda:

  Numeric value. The regularization parameter.

## Value

A list containing the estimated coefficients, fitted values and some
goodness-of-fit metrics.
