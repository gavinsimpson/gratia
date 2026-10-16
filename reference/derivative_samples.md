# Posterior expectations of derivatives from an estimated model

Posterior expectations of derivatives from an estimated model

## Usage

``` r
# S3 method for class 'gamm4'
derivative_samples(model, ...)

derivative_samples(model, ...)

# Default S3 method
derivative_samples(model, ...)

# S3 method for class 'gamm'
derivative_samples(model, ...)

# S3 method for class 'gam'
derivative_samples(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e+06,
  focal = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  n_grid = 200L,
  eps = NULL,
  envir = environment(formula(model))
)

# S3 method for class 'scam'
derivative_samples(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  chains = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1L,
  n_threads = 1L,
  burnin = 1000,
  thin = 1L,
  t_df = 40,
  rw_scale = 0.25,
  frequentist = FALSE,
  unconditional = FALSE,
  draws = NULL,
  diagnostics = FALSE,
  mh_backend = c("mgcv", "gratia", "family"),
  mvn_method = c("mvnfast", "mgcv"),
  block_size = 10000L,
  dense_limit = 5e+06,
  focal = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  n_grid = 200L,
  eps = NULL,
  envir = environment(formula(model))
)
```

## Arguments

- model:

  a fitted model to compute derivative samples for

- ...:

  arguments passed to other methods and on to
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)

- data:

  a data frame containing the values of the model covariates at which to
  evaluate derivatives of fitted values. If supplied, all but one
  variable must be held at a constant value.

- n_draws:

  positive integer; total retained draws across all chains. Required
  when generating draws. With `method = "user"`, the supplied matrix
  determines the count; an explicit count must agree with its rows.

- chains:

  positive integer or `NULL`; number of chains. `NULL` uses one chain,
  or the chain mapping in supplied draws. `n_draws` must be divisible by
  `chains`. Burn-in is applied separately to each MH chain.

- seed:

  numeric or `NULL`; seed for the complete sampling operation. An
  explicit seed restores the caller's random-number state on exit.

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

- n_cores:

  positive integer; maximum concurrent MH chains using mirai, capped at
  `chains`. Existing workers are reused; otherwise temporary workers are
  created and cleaned up. Only used by `method = "mh"`.

- n_threads:

  positive integer; threads within Gaussian sampling using mvnfast.
  Defaults to one. This is separate from concurrent MH chains.

- burnin:

  numeric; the length of any initial burn in period to discard. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).

- thin:

  positive integer; retain every `thin` iterations after burn-in.

- t_df:

  numeric; degrees of freedom for static multivariate *t* proposal. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).

- rw_scale:

  numeric; factor by which to scale posterior covariance matrix when
  generating random walk proposals. See
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).

- frequentist:

  logical; if `TRUE`, the frequentist covariance matrix of the parameter
  estimates is used. If `FALSE`, the Bayesian posterior covariance
  matrix of the parameters is used. See
  [`mgcv::vcov.gam()`](https://rdrr.io/pkg/mgcv/man/vcov.gam.html).

- unconditional:

  logical; if `TRUE` the Bayesian smoothing parameter uncertainty
  corrected covariance matrix is used, *if available* for `model`. See
  [`mgcv::vcov.gam()`](https://rdrr.io/pkg/mgcv/man/vcov.gam.html). SCAM
  models do not support this correction; use `unconditional = FALSE`.

- draws:

  matrix; user supplied posterior draws to be used when
  `method = "user"`.

- diagnostics:

  logical; calculate coefficient R-hat, effective sample sizes, and
  Monte Carlo standard errors using the optional posterior package.

- mh_backend:

  character; `"mgcv"` preserves
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).
  `"gratia"` uses compiled common-family likelihoods. `"family"` uses a
  compiled MH loop with callbacks to the fitted family object. Both
  support independent-observation bam fits. Smoothing parameters and
  parameters outside the fitted coefficient vector remain fixed. LSS
  scale and shape coefficients are sampled jointly with location. The
  family backend caches a full design matrix, smoothing penalty, and
  proposal factorisation once for all chains.

- mvn_method:

  character; one of `"mvnfast"` or `"mgcv"`. The default is uses
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html), which
  can be considerably faster at generate large numbers of MVN random
  values than [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html),
  but which might not work for some marginal fits, such as those where
  the covariance matrix is close to singular.

- block_size:

  positive integer; maximum observation rows per block in the gratia MH
  evaluator. Large designs are evaluated without storing a full matrix.

- dense_limit:

  numeric; maximum number of design-matrix elements to cache for the
  gratia MH backend. Use zero to force blockwise evaluation.

- focal:

  character; name of the focal variable. The response derivative of the
  response with respect to this variable will be returned. All other
  variables involved in the model will be held at constant values. This
  must be supplied.

- order:

  numeric; the order of derivative.

- type:

  character; the type of finite difference used. One of `"forward"`,
  `"backward"`, or `"central"`.

- scale:

  character; should the derivative be estimated on the response or the
  linear predictor (link) scale? One of `"response"` (the default), or
  `"linear_predictor"`.

- n_grid:

  positive integer; resolution for constructing evaluation data if
  `data` not supplied

- eps:

  a positive finite number giving the absolute finite-difference step,
  or `NULL` (the default) to choose it automatically. The automatic step
  is the fitted range of the differentiation coordinate multiplied by
  `.Machine$double.eps^(1 / (order + p))`, where `p = 2` for central
  differences and `p = 1` for forward or backward differences. A
  constant coordinate uses its absolute value (or one if zero) instead
  of its range. The step is rounded upwards and bounded below so that
  adding it to the coordinates produces a representable change. Stored
  transformed values or raw covariate summaries supply the range;
  supplied `data` are used if neither is available. This is a
  scale-aware heuristic, not an error bound; unusual function scales or
  domain boundaries may require explicit `eps`. For first central
  differences the two points are separated by `eps`; for second central
  differences they are each `eps` from the target.

- envir:

  the environment within which to recreate the data used to fit `model`.

## Value

A tibble, currently with the following variables:

- `.derivative`: the estimated partial derivative,

- additional columns containing the covariate values at which the
  derivative was evaluated.

## Details

For LSS models, response-scale derivatives differentiate the expected
response returned by
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
retaining the joint uncertainty in every parameter that affects that
expectation. Predictor-scale derivatives refer to the first linear
predictor. Smooth-component derivatives and derivatives of the response
expectation are different quantities.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson

## Examples

``` r

load_mgcv()
df <- data_sim("eg1", dist = "negbin", scale = 0.25, seed = 42)

# fit the GAM (note: for execution time reasons using bam())
m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df, family = nb(), method = "fREML")

# data slice through data along x2 - all other covariates will be set to
# typical values (value closest to median)
ds <- data_slice(m, x2 = evenly(x2, n = 200))

# samples from posterior of derivatives
fd_samp <- derivative_samples(m,
  data = ds, type = "central",
  focal = "x2", eps = 0.01, seed = 21, n_draws = 100
)

# plot the first 20 posterior draws
if (requireNamespace("ggplot2") && requireNamespace("dplyr")) {
  library("ggplot2")
  fd_samp |>
    dplyr::filter(.draw <= 20) |>
    ggplot(aes(x = x2, y = .derivative, group = .draw)) +
    geom_line(alpha = 0.5)
}
```
