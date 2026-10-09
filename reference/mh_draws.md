# Posterior samples using a Gaussian approximation to the posterior distribution

Posterior samples using a Gaussian approximation to the posterior
distribution

## Usage

``` r
# S3 method for class 'gamm4'
mh_draws(model, ...)

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

  a fitted model from
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html), or
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html), or a
  model inheriting from these classes. See
  [gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
  for sampling from gamm4 fits.

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

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
