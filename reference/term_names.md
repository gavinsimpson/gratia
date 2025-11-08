# Extract names of all variables needed to fit a GAM or a smooth

Extract names of all variables needed to fit a GAM or a smooth

## Usage

``` r
term_names(object, ...)

# S3 method for class 'gam'
term_names(object, ...)

# S3 method for class 'mgcv.smooth'
term_names(object, ...)

# S3 method for class 'gamm'
term_names(object, ...)
```

## Arguments

- object:

  a fitted GAM object (inheriting from class `"gam"` or an
  [mgcv::smooth.construct](https://rdrr.io/pkg/mgcv/man/smooth.construct.html)
  smooth object, inheriting from class `"mgcv.smooth"`.

- ...:

  arguments passed to other methods. Not currently used.

## Value

A vector of variable names required for terms in the model
