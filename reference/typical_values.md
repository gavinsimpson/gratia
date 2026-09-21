# Typical values of model covariates

Typical values of model covariates

## Usage

``` r
typical_values(object, ...)

# S3 method for class 'gam'
typical_values(object, vars = everything(), data = NULL, ...)

# S3 method for class 'data.frame'
typical_values(object, vars = everything(), ...)
```

## Arguments

- object:

  a fitted GAM(M) model.

- ...:

  arguments passed to other methods.

- vars:

  terms to include or exclude from the returned object. Uses tidyselect
  principles.

- data:

  an optional data frame supplying covariate classes. By default, these
  are taken from the stored model frame; typical values come from the
  fitted model's covariate summaries.
