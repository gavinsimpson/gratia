# Add fitted values from a model to a data frame

Add fitted values from a model to a data frame

## Usage

``` r
add_fitted(data, model, value = ".value", ...)
```

## Arguments

- data:

  a data frame containing values for the variables used to fit the
  model. Passed to
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) as
  `newdata`.

- model:

  a fitted model for which a
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) method is
  available. S3 method dispatch is performed on the `model` argument.

- value:

  character; the name of the variable in which model predictions will be
  stored.

- ...:

  additional arguments passed to methods.

## Value

A data frame (tibble) formed from `data` and fitted values from `model`.
