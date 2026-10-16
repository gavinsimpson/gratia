# Draw samples from the posterior distribution of an estimated model

Draw samples from the posterior distribution of an estimated model

## Usage

``` r
# S3 method for class 'lm'
posterior_samples(model, ..., n_draws = NULL, seed = NULL)

# S3 method for class 'glm'
posterior_samples(model, ..., n_draws = NULL, seed = NULL)

# S3 method for class 'glmmTMB'
posterior_samples(model, ..., n_draws = NULL, seed = NULL, conditioning = NULL)

# S3 method for class 'sdmTMB'
posterior_samples(model, ..., n_draws = NULL, seed = NULL, conditioning = NULL)

# S3 method for class 'gamlss'
posterior_samples(model, ..., n_draws = NULL, seed = NULL)

# S3 method for class 'gamm'
posterior_samples(model, ..., n_draws = NULL, seed = NULL, conditioning = NULL)

# S3 method for class 'lme'
posterior_samples(model, ..., n_draws = NULL, seed = NULL, conditioning = NULL)

# S3 method for class 'merMod'
posterior_samples(model, ..., n_draws = NULL, seed = NULL, conditioning = NULL)

# S3 method for class 'gamm4'
posterior_samples(model, ..., n_draws = NULL, seed = NULL, conditioning = NULL)

posterior_samples(model, ...)

# S3 method for class 'gam'
posterior_samples(
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
  weights = NULL,
  family_index = NULL
)

# S3 method for class 'scam'
posterior_samples(
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
  weights = NULL,
  family_index = NULL
)
```

## Arguments

- model:

  a fitted model of the supported types; see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- ...:

  arguments passed to other methods. For
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  these are passed on to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).
  For `posterior_samples()` these are passed on to
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md).
  For
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
  these are passed on to the relevant
  [`simulate()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  method.

- n_draws:

  positive integer; total retained draws across all chains. Required
  when generating draws. With `method = "user"`, the supplied matrix
  determines the count; an explicit count must agree with its rows.

- seed:

  numeric; a random seed for the simulations.

- conditioning:

  character or `NULL`; mixed-model conditioning mode; see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `data`, if available in `model`.

- chains:

  positive integer or `NULL`; number of chains. `NULL` uses one chain,
  or the chain mapping in supplied draws. `n_draws` must be divisible by
  `chains`. Burn-in is applied separately to each MH chain.

- method:

  character; which method should be used to draw samples from the
  posterior distribution. `"gaussian"` uses a Gaussian (Laplace)
  approximation to the posterior. `"mh"` uses a Metropolis Hastings
  sampler that alternates t proposals with proposals based on a shrunken
  version of the posterior covariance matrix. `"inla"` uses a variant of
  Integrated Nested Laplace Approximation due to Wood (2019), (currently
  not implemented). `"user"` allows for user-supplied posterior draws as
  a coefficient matrix.

- n_cores:

  positive integer; maximum concurrent MH chains using mirai, capped at
  `chains`. Existing workers are reused; otherwise temporary workers are
  created and cleaned up. Only used by `method = "mh"`.

- n_threads:

  positive integer; threads within Gaussian sampling using mvnfast.
  Defaults to one. This is separate from concurrent MH chains.

- burnin:

  numeric; number of samples to discard as the burnin draws. Only used
  with `method = "mh"`.

- thin:

  positive integer; retain every `thin` iterations after burn-in.

- t_df:

  numeric; degrees of freedom for t distribution proposals. Only used
  with `method = "mh"`.

- rw_scale:

  numeric; Factor by which to scale posterior covariance matrix when
  generating random walk proposals. Negative or non finite to skip the
  random walk step. Only used with `method = "mh"`.

- frequentist:

  logical; `TRUE` to use the frequentist covariance matrix of the
  parameter estimators, `FALSE` to use the Bayesian posterior covariance
  matrix of the parameters.

- unconditional:

  logical; if `TRUE` (and `frequentist == FALSE`) then the Bayesian
  smoothing parameter uncertainty corrected covariance matrix is used,
  if available.

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

- weights:

  numeric; response sampling weights for GAM methods. An explicit vector
  overrides the fitting weights; a single value applies to every
  observation. With `data = NULL`, the default is the fitted prior
  weights, except factor-valued binomial responses retain one Bernoulli
  trial per observation. Native
  [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) ignores fitting
  weights in its likelihood, so its default response weights are also
  ones; explicitly supplied sampling weights still override this
  default. With prediction data, weights default to a vector of ones.
  For ordinary Gaussian and Gamma responses, larger weights reduce
  response variance (precision weights). For binomial responses, weights
  give integer trial counts. They do not alter the fitted coefficient
  covariance. Include positions restored by `na.exclude` when supplying
  weights for fitting rows.

- family_index:

  numeric; optional family-list positions for grouped family
  ([`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html)) response
  draws, one integer per evaluation row. Membership can instead be
  recovered from the response-index expression in `data`; observed
  response values are not required.

## Value

A tibble (data frame) in long format, with columns:

- `.row`: row of the evaluation data;

- `.draw`: globally unique draw identifier;

- `.response`: simulated response. Where needed, `.parameter` identifies
  the response or grouped component. Multiple-chain output also includes
  `.chain` and `.iteration`. Attached coefficient diagnostics are
  available through
  [`sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md).

## Details

The `lm`, `glm`, `glmmTMB`,
[`GJRM::gamlss`](https://rdrr.io/pkg/GJRM/man/gamlss.html), `sdmTMB`,
and full mixed-model diagnostic samplers currently use the fitting
observations only. They draw coefficients together using Gaussian
approximations. See
[`posterior_predictive_plot()`](https://gavinsimpson.github.io/gratia/reference/posterior_predictive_plot.md)
for which additional distribution parameters remain fixed and how random
effects are treated for each fitter.

With no explicit `data`, observation rows follow the fitted model's
`na.action`. `na.exclude` restores excluded rows as `NA` in every draw;
`na.omit` returns retained rows only. `.row` indexes the fitting data
after any `subset`. With explicit `data`, `.row` indexes that data,
missing responses do not prevent prediction, and missing required
predictors give `NA` draws. An explicit prediction `na.action` in `...`
is honoured without renumbering retained rows. Restoration does not
consume random numbers.

For Gaussian identity-link `bam` fits, first-order autoregressive (AR1)
errors make consecutive responses correlated. Responses are drawn
together to retain this fitted correlation, restarting at each
`AR.start` segment. These draws currently use the fitting observations
only. For other families or links, the AR1 correction applies to working
residuals used during fitting. It does not specify a joint distribution
of actual responses, so sampling produces an error.

Location-scale-shape families
[`mgcv::gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html),
[`mgcv::gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html),
[`mgcv::gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html),
[`mgcv::gevlss()`](https://rdrr.io/pkg/mgcv/man/gevlss.html),
[`mgcv::shash()`](https://rdrr.io/pkg/mgcv/man/shash.html),
[`mgcv::twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html) and
[`mgcv::ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html) support
fitting observations, subsets and new covariates. Every distribution
parameter is computed from the same joint coefficient draw. Formula
offsets and prescribed smooth contributions are retained. Evaluation
data need predictors and formula-offset variables, but no observed
response. In [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html), the
second response parameter is inverse standard deviation: sampling uses
its reciprocal, divided by the square root of the sampling weight. Other
LSS generators currently ignore sampling weights, following their mgcv
implementations. Parameters represented by coefficients vary jointly;
other family parameters remain fixed.

Cox survival models require evaluation times and an observation process
and do not have a response generator for this interface. Censored-family
draws are latent, uncensored responses; reproducing observed censoring
requires an observation process and remains unsupported in posterior
predictive checks. Grouped-family posterior response draws support
fitting observations and supplied data with known family membership.
Component nuisance parameters remain fixed. `.parameter` identifies the
component used for each row. Censored grouped components require an
observation process; use
[`simulate()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
for their latent, uncensored responses.

## Note

With `data = NULL`, fitting offsets are retained, including offsets
supplied through the fitting function's `offset` argument. Explicit
prediction data follow
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html):
offsets in formulas are evaluated from the prediction data, while
offsets supplied through the fitting function's `offset` argument are
not transferred to new observations. Include offsets in the formula when
predicting at new data; the required offset variables must then be
present in `data`.

## References

Wood, S.N., (2020). Simplified integrated nested Laplace approximation.
*Biometrika* **107**, 223–230.
[doi:10.1093/biomet/asz044](https://doi.org/10.1093/biomet/asz044)

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson
