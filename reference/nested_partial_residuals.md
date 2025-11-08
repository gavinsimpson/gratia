# Partial residuals in nested form

Computes partial residuals for smooth terms, formats them in long/tidy
format, then nests the `partial_residual` column such that the result is
a nested data frame with one row per smooth.

## Usage

``` r
nested_partial_residuals(object, terms = NULL, data = NULL)
```

## Arguments

- object:

  a fitted GAM model

- terms:

  a vector of terms to include partial residuals for. Passed to argument
  `terms` of
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)\].

- data:

  optional data frame

## Value

A nested tibble (data frame) with one row per smooth term. Contains two
columns:

- `smooth` - a label indicating the smooth term

- `partial_residual` - a list column containing a tibble (data frame)
  with 1 column `partial_residual` containing the requested partial
  residuals for the indicated smooth.
