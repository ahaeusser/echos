# echos 1.0.2

## New features
* Added forecast intervals to `forecast_esn()`, `forecast.ESN()` and `plot.forecast_esn()`. Forecast intervals are generated by simulating future sample path based on a moving block bootstrap of the residuals and estimating the quantiles from the simulations.
* Added `plot.esn()` to visualize the internal states (i.e., the reservoir).
* Added `filter_esn()` to extract ESN models from a mable.
* Added `synthetic_data`, a dataset with synthetic time series data as tibble.

## Improvements
* Improved documentation
* Added unit tests
* Reduced dependencies

# echos 1.0.1
* Updates due to CRAN comments

# echos 1.0.0
* Initial CRAN submission
