# How many smooths in a fitted model

How many smooths in a fitted model

## Usage

``` r
n_smooths(object)

# Default S3 method
n_smooths(object)

# S3 method for class 'gam'
n_smooths(object)

# S3 method for class 'gamm'
n_smooths(object)

# S3 method for class 'bam'
n_smooths(object)
```

## Arguments

- object:

  a fitted GAM or related model. Typically the result of a call to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html), or
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html).
