# Checks if object is of class "esn"

Returns `TRUE` if the object is of class "esn".

## Usage

``` r
is.esn(object)
```

## Arguments

- object:

  object to be tested.

## Value

Logical value. If `TRUE`, the object is of class "esn".

## Examples

``` r
xdata <- as.numeric(AirPassengers)
xmodel <- train_esn(y = xdata)
is.esn(xmodel)
#> [1] TRUE
```
