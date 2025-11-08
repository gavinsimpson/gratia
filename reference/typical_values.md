# Typical values of model covariates

Typical values of model covariates

## Usage

``` r
typical_values(object, ...)

# S3 method for class 'gam'
typical_values(
  object,
  vars = everything(),
  envir = environment(formula(object)),
  data = NULL,
  ...
)

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

- envir:

  the environment within which to recreate the data used to fit
  `object`.

- data:

  an optional data frame of data used to fit the model if reconstruction
  of the data from the model doesn't work.
