# Add residuals from a model to a data frame

Add residuals from a model to a data frame

## Usage

``` r
add_residuals(data, model, value = ".residual", ...)
```

## Arguments

- data:

  a data frame containing values for the variables used to fit the
  model. Passed to
  [`stats::residuals()`](https://rdrr.io/r/stats/residuals.html) as
  `newdata`.

- model:

  a fitted model for which a
  [`stats::residuals()`](https://rdrr.io/r/stats/residuals.html) method
  is available. S3 method dispatch is performed on the `model` argument.

- value:

  character; the name of the variable in which model residuals will be
  stored.

- ...:

  additional arguments passed to methods.

## Value

A data frame (tibble) formed from `data` and residuals from `model`.
