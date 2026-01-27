# Package index

## Base functions

Functions and methods for basic time series forecasting

- [`train_esn()`](https://ahaeusser.github.io/echos/reference/train_esn.md)
  : Train an Echo State Network
- [`forecast_esn()`](https://ahaeusser.github.io/echos/reference/forecast_esn.md)
  : Forecast an Echo State Network
- [`tune_esn()`](https://ahaeusser.github.io/echos/reference/tune_esn.md)
  : Tune hyperparameters of an Echo State Network
- [`print(`*`<esn>`*`)`](https://ahaeusser.github.io/echos/reference/print.esn.md)
  : Print model specification of the trained ESN model
- [`summary(`*`<esn>`*`)`](https://ahaeusser.github.io/echos/reference/summary.esn.md)
  : Provide a detailed summary of the trained ESN model
- [`summary(`*`<tune_esn>`*`)`](https://ahaeusser.github.io/echos/reference/summary.tune_esn.md)
  : Provide a summary of the hyperparameter tuning
- [`is.esn()`](https://ahaeusser.github.io/echos/reference/is.esn.md) :
  Checks if object is of class "esn"
- [`is.forecast_esn()`](https://ahaeusser.github.io/echos/reference/is.forecast_esn.md)
  : Checks if object is of class "forecast_esn"
- [`is.tune_esn()`](https://ahaeusser.github.io/echos/reference/is.tune_esn.md)
  : Checks if object is of class "tune_esn"
- [`plot(`*`<esn>`*`)`](https://ahaeusser.github.io/echos/reference/plot.esn.md)
  : Plot internal states of a trained ESN model
- [`plot(`*`<forecast_esn>`*`)`](https://ahaeusser.github.io/echos/reference/plot.forecast_esn.md)
  : Plot forecasts of a trained ESN model
- [`plot(`*`<tune_esn>`*`)`](https://ahaeusser.github.io/echos/reference/plot.tune_esn.md)
  : Plot forecasts from a tuned ESN object
- [`run_reservoir`](https://ahaeusser.github.io/echos/reference/run_reservoir.md)
  : Run reservoir

## Tidy functions

Functions and methods in tidy time series forecasting (“fable
framework”)

- [`ESN()`](https://ahaeusser.github.io/echos/reference/ESN.md) : Train
  an Echo State Network
- [`forecast(`*`<ESN>`*`)`](https://ahaeusser.github.io/echos/reference/forecast.ESN.md)
  : Forecast an Echo State Network
- [`fitted(`*`<ESN>`*`)`](https://ahaeusser.github.io/echos/reference/fitted.ESN.md)
  : Extract fitted values from a trained ESN
- [`residuals(`*`<ESN>`*`)`](https://ahaeusser.github.io/echos/reference/residuals.ESN.md)
  : Extract residuals from a trained ESN
- [`model_sum(`*`<ESN>`*`)`](https://ahaeusser.github.io/echos/reference/model_sum.ESN.md)
  : Model specification of a trained ESN model
- [`tidy(`*`<ESN>`*`)`](https://ahaeusser.github.io/echos/reference/tidy.ESN.md)
  : Estimated coefficients
- [`glance(`*`<ESN>`*`)`](https://ahaeusser.github.io/echos/reference/glance.ESN.md)
  : Summary of trained models during random search
- [`report(`*`<ESN>`*`)`](https://ahaeusser.github.io/echos/reference/report.ESN.md)
  : Provide a detailed summary of the trained ESN model
- [`filter_esn()`](https://ahaeusser.github.io/echos/reference/filter_esn.md)
  : Filter ESN models
- [`reservoir()`](https://ahaeusser.github.io/echos/reference/reservoir.md)
  : Return the reservoir from a trained ESN as tibble

## Datasets

Datasets

- [`m4_data`](https://ahaeusser.github.io/echos/reference/m4_data.md) :
  M4 dataset
- [`synthetic_data`](https://ahaeusser.github.io/echos/reference/synthetic_data.md)
  : Synthetic data
