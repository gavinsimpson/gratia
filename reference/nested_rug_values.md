# Values for rug plot in nested form

Extracts original data for smooth terms, formats them in long/tidy
format, then nests the data column(s) such that the result is a nested
data frame with one row per smooth.

## Usage

``` r
nested_rug_values(object, terms = NULL, data = NULL, distinct = TRUE)
```

## Arguments

- object:

  a fitted GAM model

- terms:

  a vector of terms to include original data for. Passed to argument
  `terms` of
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)\].

- data:

  optional data frame

## Value

A nested tibble (data frame) with one row per smooth term.
