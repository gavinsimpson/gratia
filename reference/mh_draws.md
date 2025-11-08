# Posterior samples using a Gaussian approximation to the posterior distribution

Posterior samples using a Gaussian approximation to the posterior
distribution

## Usage

``` r
mh_draws(model, ...)

# S3 method for class 'gam'
mh_draws(
  model,
  n,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  index = NULL,
  ...
)
```

## Arguments

- model:

  a fitted R model. Currently only models fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html), or return an
  object that *inherits* from such objects are supported. Here,
  "inherits" is used in a loose fashion; models fitted by
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html) are support
  even though those models don't strictly inherit from class `"gam"` as
  far as [`inherits()`](https://rdrr.io/r/base/class.html) is concerned.

- ...:

  arguments passed to methods.

- n:

  numeric; the number of posterior draws to take.

- burnin:

  numeric; the length of any initial burn in period to discard. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).

- thin:

  numeric; retain only `thin` samples. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).

- t_df:

  numeric; degrees of freedom for static multivariate *t* proposal. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).

- rw_scale:

  numeric; factor by which to scale posterior covariance matrix when
  generating random walk proposals. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).

- index:

  numeric; vector of indices of coefficients to use. Can be used to
  subset the mean vector and covariance matrix extracted from `model`.
