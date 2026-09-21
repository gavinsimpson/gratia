# All combinations of factor levels plus typical values of continuous variables

All combinations of factor levels plus typical values of continuous
variables

## Usage

``` r
data_combos(object, ...)

# S3 method for class 'gam'
data_combos(object, vars = everything(), complete = TRUE, data = NULL, ...)
```

## Arguments

- object:

  a fitted model object.

- ...:

  arguments passed to methods.

- vars:

  terms to include or exclude from the returned object. Uses tidyselect
  principles.

- complete:

  logical; should all combinations of factor levels be returned? If
  `FALSE`, only those combinations of levels observed in the model are
  retained.

- data:

  an optional data frame supplying covariate classes. By default, these
  are taken from the stored model frame; typical values come from the
  fitted model's covariate summaries.
