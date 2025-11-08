# Low-level Functions to generate draws from the posterior distribution of model coefficients

Low-level Functions to generate draws from the posterior distribution of
model coefficients

Generate posterior draws from a fitted model

## Usage

``` r
post_draws(model, ...)

# Default S3 method
post_draws(
  model,
  n,
  method = c("gaussian", "mh", "inla", "user"),
  mu = NULL,
  sigma = NULL,
  n_cores = 1L,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  index = NULL,
  frequentist = FALSE,
  unconditional = FALSE,
  parametrized = TRUE,
  mvn_method = c("mvnfast", "mgcv"),
  draws = NULL,
  seed = NULL,
  ...
)

generate_draws(model, ...)

# S3 method for class 'gam'
generate_draws(
  model,
  n,
  method = c("gaussian", "mh", "inla"),
  mu = NULL,
  sigma = NULL,
  n_cores = 1L,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  index = NULL,
  frequentist = FALSE,
  unconditional = FALSE,
  mvn_method = c("mvnfast", "mgcv"),
  seed = NULL,
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

- method:

  character; which algorithm to use to sample from the posterior.
  Currently implemented methods are: `"gaussian"` and `"mh"`.
  `"gaussian"` calls
  [`gaussian_draws()`](https://gavinsimpson.github.io/gratia/reference/gaussian_draws.md)
  which uses a Gaussian approximation to the posterior distribution.
  `"mh"` uses a simple Metropolis Hastings sampler which alternates
  static proposals based on a Gaussian approximation to the posterior,
  with random walk proposals. Note, setting `t_df` to a low value will
  result in heavier-tailed statistic proposals. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html) for more
  details.

- mu:

  numeric; user-supplied mean vector (vector of model coefficients).
  Currently ignored.

- sigma:

  matrix; user-supplied covariance matrix for `mu`. Currently ignored.

- n_cores:

  integer; number of CPU cores to use when generating multivariate
  normal distributed random values. Only used if
  `mvn_method = "mvnfast"` **and** `method = "gaussian"`.

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

- frequentist:

  logical; if `TRUE`, the frequentist covariance matrix of the parameter
  estimates is used. If `FALSE`, the Bayesian posterior covariance
  matrix of the parameters is used. See
  [`mgcv::vcov.gam()`](https://rdrr.io/pkg/mgcv/man/vcov.gam.html).

- unconditional:

  logical; if `TRUE` the Bayesian smoothing parameter uncertainty
  corrected covariance matrix is used, *if available* for `model`. See
  [`mgcv::vcov.gam()`](https://rdrr.io/pkg/mgcv/man/vcov.gam.html).

- parametrized:

  logical; use parametrized coefficients and covariance matrix, which
  respect the linear inequality constraints of the model. Only for
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html) model fits.

- mvn_method:

  character; one of `"mvnfast"` or `"mgcv"`. The default is uses
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html), which
  can be considerably faster at generate large numbers of MVN random
  values than [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html),
  but which might not work for some marginal fits, such as those where
  the covariance matrix is close to singular.

- draws:

  matrix; user supplied posterior draws to be used when
  `method = "user"`.

- seed:

  numeric; the random seed to use. If `NULL`, a random seed will be
  generated without affecting the current state of R's RNG.
