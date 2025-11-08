# Is a model multivariate?

Determines whether a fitted model (GAM) is truly multivariate or not.

## Usage

``` r
is_multivariate_y(model)
```

## Arguments

- model:

  a fitted model object; currently only for `"gam"` objects

## Value

A logical vector of length 1, indicating if `model` is multivariate
(`TRUE`), or otherwise (`FALSE`).
