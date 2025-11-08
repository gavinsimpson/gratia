# Exclude values that lie too far from the support of data

Identifies pairs of covariate values that lie too far from the original
data. The function is currently a basic wrapper around
[`mgcv::exclude.too.far()`](https://rdrr.io/pkg/mgcv/man/exclude.too.far.html).

## Usage

``` r
too_far(x, y, ref_1, ref_2, dist = NULL)
```

## Arguments

- x, y:

  numeric; vector of values of the covariates to compare with the
  observed data

- ref_1, ref_2:

  numeric; vectors of covariate values that represent the reference
  against which `x1 and `x2\` are compared

- dist:

  if supplied, a numeric vector of length 1 representing the distance
  from the data beyond which an observation is excluded. For example,
  you want to exclude values that lie further from an observation than
  10% of the range of the observed data, use `0.1`.

## Value

Returns a logical vector of the same length as `x1`.
