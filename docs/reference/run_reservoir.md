# Run reservoir

Run reservoir creates the internal states for the ESN.

## Usage

``` r
run_reservoir(inputs, win, wres, alpha)
```

## Arguments

- inputs:

  Numeric matrix containing the input features.

- win:

  Numeric matrix. The input weight matrix.

- wres:

  Numeric matrix. The reservoir weight matrix.

- alpha:

  Numeric value. The leakage rate (smoothing parameter).

## Value

Numeric matrix with the internal states.
