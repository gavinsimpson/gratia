# Variance components of smooths from smoothness estimates

A wrapper to
[`mgcv::gam.vcomp()`](https://rdrr.io/pkg/mgcv/man/gam.vcomp.html) which
returns the smoothing parameters expressed as variance components.

## Usage

``` r
variance_comp(object, ...)

# S3 method for class 'gam'
variance_comp(object, rescale = TRUE, coverage = 0.95, ...)
```

## Arguments

- object:

  an R object. Currently only models fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) are supported.

- ...:

  arguments passed to other methods

- rescale:

  logical; for numerical stability reasons the penalty matrices of
  smooths are rescaled before fitting. If `rescale = TRUE`, this
  rescaling is undone, resulting in variance components that are on
  their original scale. This is needed if comparing with other mixed
  model software, such as
  [`lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html).

- coverage:

  numeric; a value between 0 and 1 indicating the (approximate) coverage
  of the confidence interval that is returned.

## Details

This function is a wrapper to
[`mgcv::gam.vcomp()`](https://rdrr.io/pkg/mgcv/man/gam.vcomp.html) which
performs three additional services

- it suppresses the annoying text output that
  [`mgcv::gam.vcomp()`](https://rdrr.io/pkg/mgcv/man/gam.vcomp.html)
  prints to the terminal,

- returns the variance of each smooth as well as the standard deviation,
  and

- returns the variance components as a tibble.
