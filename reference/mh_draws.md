# Posterior samples using a simple Metropolis Hastings sampler

Posterior samples using a simple Metropolis Hastings sampler

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

## Details

GAM-class fits with linear inequality constraints, including
[`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) fits, store
these as `Ain %*% coefficients >= bin`. Gaussian sampling uses the
fitted covariance matrix without enforcing these inequalities and issues
a warning. Sampled smooths can therefore violate the fitted shape
constraints. Pointwise intervals computed from the same Gaussian
covariance approximation also need not preserve those shapes.
Metropolis-Hastings sampling is unavailable for these fits because
[`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html) does not
enforce their inequalities.

If [`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) was fitted
with `bs > 0`, its `bs` component stores coefficient draws from the
native constrained bootstrap in columns. These can be supplied
explicitly as `draws = t(model$bs)` with `method = "user"`. The result
then describes bootstrap variation, rather than Gaussian posterior
sampling. Gratia does not automatically select these draws. The caller
is responsible for the interpretation and constraint feasibility of
user-supplied draws.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
