# Low-level Functions to generate draws from the posterior distribution of model coefficients

Low-level Functions to generate draws from the posterior distribution of
model coefficients

Generate posterior draws from a fitted model

## Usage

``` r
# S3 method for class 'gamm4'
generate_draws(model, ...)

# S3 method for class 'gamm4'
post_draws(model, ...)

post_draws(model, ...)

# Default S3 method
post_draws(
  model,
  ...,
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
  index = NULL,
  parametrized = TRUE
)

generate_draws(model, ...)

# S3 method for class 'gam'
generate_draws(
  model,
  ...,
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
  index = NULL
)

# S3 method for class 'scam'
generate_draws(model, ...)
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

- index:

  numeric; vector of indices of coefficients to use. Can be used to
  subset the mean vector and covariance matrix extracted from `model`.

- parametrized:

  logical; for SCAM fits, transform Gaussian draws from the unrestricted
  fitting coefficients using the fitted positivity transform. This
  preserves the smooth's constraints. With `FALSE`, return Gaussian
  draws in the unrestricted fitting coordinates. Only for
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html) fits.

## Details

Sampling controls must be named. Removed arguments produce migration
errors; see
[`vignette("sampling-migration")`](https://gavinsimpson.github.io/gratia/articles/sampling-migration.md).
Explicit controls that the selected method cannot use are rejected.

For SCAM models, Gaussian draws approximate the distribution of the
unrestricted fitting coefficients using `Vp` (or `Ve` with
`frequentist = TRUE`), then apply scam's fitted exponential or `notExp`
transformation. This is an approximation in fitting coordinates, rather
than an exact constrained posterior. Supplied draws must already use the
transformed coordinates in `coef(model)` order. Numerically singular
covariance matrices are sampled by eigen decomposition; negative
eigenvalues within `100 * .Machine$double.eps * ncol(V)` times the
largest absolute eigenvalue are treated as zero. Larger negatives are
errors. For weakly identified exponential coefficients, a very large
fitting-coordinate variance can yield non-finite transformed draws.
Sampling then reports an error because this Gaussian approximation
cannot provide usable draws; supply finite transformed draws from a
suitable alternative sampler.

For multiple chains, `n_draws` is the total retained count and must be
divisible by `chains`. Coefficient matrices carry a chain mapping; use
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
before ordinary matrix operations remove that mapping. Higher-level
sampling tables include `.chain` and `.iteration` for multiple chains,
with `.draw` unique across chains. Default single-chain table columns
are unchanged. Use
[`sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md)
to extract coefficient diagnostics before response simulation adds
further noise.

The gratia MH backend supports Gaussian identity, Poisson log, binomial
logit, Gamma log, and negative binomial log fits. It evaluates the
fitted bases at original covariates, including for discrete bam fits.
GAMM components, AR1 dependence, `paraPen`, `H`, and fits made from
pre-fit `G` objects are unsupported.

The family MH backend calls the fitted family's `dev.resids()` and
`linkinv()` functions, or `ll()` for families with multiple linear
predictors. This retains response encodings and attributes such as
censoring indicators and the mappings between coefficients and
predictors. It supports compatible standard, extended, and general mgcv
families, including multinomial, Gaussian location-scale, and grouped
families. Quasi families, Cox proportional hazards models, GAMM
components, AR1 dependence, `paraPen`, `H`, and pre-fit `G` objects are
unsupported. The fitted coefficient covariance must be nonsingular. It
always caches the full design matrix; `block_size` and `dense_limit`
apply only to the gratia backend and are rejected for the family
backend. The new MH backends reject `bam` fits with nonzero prescribed
smooth contributions: mgcv fitting and prediction can use different
offsets for those fits. Supplied coefficient draws can still be
evaluated with the native prediction conventions.

All MH backends sample the conditional coefficient posterior with fitted
smoothing, dispersion, and additional family parameters fixed.
Parameters represented in the fitted coefficient vector are sampled,
including scale or covariance coefficients in general families.
`unconditional` applies to Gaussian draws only.

GAM-class fits with linear inequality constraints, including
[`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) fits, store
these as `Ain %*% coefficients >= bin`. Gaussian sampling uses the
fitted covariance matrix without enforcing these inequalities and issues
a warning. Sampled smooths can therefore violate the fitted shape
constraints. Pointwise intervals computed from the same Gaussian
covariance approximation also need not preserve those shapes. All MH
backends are unavailable for these fits because the samplers do not
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
