# All combinations of factor levels

All combinations of factor levels

## Usage

``` r
factor_combos(object, ...)

# S3 method for class 'gam'
factor_combos(object, vars = everything(), complete = TRUE, ...)
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
