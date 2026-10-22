# How many smooths in a fitted model

How many smooths in a fitted model

## Usage

``` r
# S3 method for class 'gamm4'
n_smooths(object)

# S3 method for class 'gjrm'
n_smooths(object)

# S3 method for class 'glmmTMB'
n_smooths(object)

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

## Details

For `glmmTMB` models, `n_smooths()` counts smooth terms across the
conditional, zero-inflation and dispersion predictors. Each fitted
smooth is counted once; native random effects are excluded. Models
without smooths return integer zero.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
