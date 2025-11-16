# Run reservoir

Run reservoir creates the internal states for the ESN.

## Arguments

- input:

  Numeric matrix containing the input features

- win:

  Numeric matrix. The input weight matrix.

- wres:

  Numeric matrix. The reservoir weight matrix.

- alpha:

  Numeric value. The leakage rate (smoothing parameter).

## Value

states train Numeric matrix with the internal states.
