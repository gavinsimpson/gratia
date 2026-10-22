# Names of smooths in a GAM

Names of smooths in a GAM

## Usage

``` r
# S3 method for class 'gamlss'
smooths(object, ...)

# S3 method for class 'gjrm'
smooths(object, ...)

smooths(object, ...)

# Default S3 method
smooths(object, ...)

# S3 method for class 'gamm'
smooths(object, ...)

# S3 method for class 'gamm4'
smooths(object, ...)
```

## Arguments

- object:

  a fitted GAM or related model. Typically the result of a call to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html), or
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html).

- ...:

  Arguments passed to methods.

## Details

For GJRM models, labels follow equation and smooth order. Labels
repeated in later equations receive suffixes such as `s.1(x)`. These
labels can be supplied to
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) or
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
to select smooths across equations.
