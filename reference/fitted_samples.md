# Draw fitted values from the posterior distribution

Distribution parameters or expected responses drawn from the posterior
distribution of a fitted model.

## Usage

``` r
# S3 method for class 'gamm4'
fitted_samples(model, ...)

fitted_samples(model, ...)

# S3 method for class 'gam'
fitted_samples(
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
  scale = c("response", "linear_predictor"),
  family_index = NULL,
  quantity = c("parameters", "expectation")
)

# S3 method for class 'scam'
fitted_samples(
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
  scale = c("response", "linear_predictor"),
  quantity = c("parameters", "expectation")
)
```

## Arguments

- model:

  a fitted model of the supported types; see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- ...:

  arguments passed to other methods. For `fitted_samples()`, these are
  passed on to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).
  For
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  these are passed on to `fitted_samples()`. For
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
  these are passed on to the relevant
  [`simulate()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  method.

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `data`, if available in `model`.

- n_draws:

  positive integer; total retained draws across all chains. Required
  when generating draws. With `method = "user"`, the supplied matrix
  determines the count; an explicit count must agree with its rows.

- chains:

  positive integer or `NULL`; number of chains. `NULL` uses one chain,
  or the chain mapping in supplied draws. `n_draws` must be divisible by
  `chains`. Burn-in is applied separately to each MH chain.

- seed:

  numeric; a random seed for the simulations.

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

- scale:

  character; `"response"` returns natural distribution parameters;
  `"link"`, `"linear_predictor"` and `"linear predictor"` return their
  predictors. Expectations require `"response"`.

- family_index:

  numeric; optional family-list positions for grouped family
  ([`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html)) response
  predictions, with one integer per requested row. By default membership
  is recovered from the response/index expression in `data`, or the
  fitted membership when `data` is omitted. This is useful when
  predicting without responses or with a complex response expression. An
  explicit index must agree with membership recoverable from `data`.

- quantity:

  character; `"parameters"` (the default) returns all distribution
  parameters represented by predictors, including intercept-only
  predictors. `"expectation"` returns the expected response, using every
  parameter on which it depends, on the response scale only.

## Value

A tibble (data frame) in long format, with columns:

- `.row`: row of the evaluation data;

- `.draw`: globally unique draw identifier;

- `.parameter`: the distribution parameter name, or `"expectation"`;

- `.fitted`: the parameter, predictor or expected response requested.
  Multiple-chain output also includes `.chain` and `.iteration`.
  Attached coefficient diagnostics are available through
  [`sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md).

## Details

Parameter names and definitions are available through
[`family_parameters()`](https://gavinsimpson.github.io/gratia/reference/family_parameters.md).
Gaussian LSS returns `mean` and `sd`, GEV returns `location`, `scale`
and `shape`, and sinh-arcsinh returns `location`, `scale`, `skewness`
and `tailweight`. These shape parameters are not response moments.
Natural parameters can differ from native mgcv predictions and
[`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
values. Every parameter in one draw uses the same joint coefficient
draw. Global parameters outside the coefficient vector remain fixed and
are described in the metadata, rather than returned as fitted
parameters.

Use `quantity = "expectation"` for expected responses labelled
`"expectation"`. This includes Gumbel, GEV and sinh-arcsinh mean
adjustments and the zero-truncated Poisson adjustment in
[`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html). A GEV
expectation does not exist at shape \>= 1; parameter and response draws
remain available. Categorical expectations are category probabilities,
not numeric category scores. `.category` identifies categories, and `.y`
identifies multivariate responses. Multinomial link output includes a
zero reference predictor. Ordinal parameter output is the latent
location predictor; its thresholds are global parameters.

The `prediction_info` attribute records the quantity, scale and resolved
parameter descriptions. Unlike
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
these draws add no response noise.
[`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
instead simulates observations with coefficients held at their fitted
values.

For grouped families, coefficient draws preserve the joint model
covariance; each row is then transformed using its own fitted component
family. Explicit prediction data must identify each row's family: no
training-order fallback is used. A two-column response matrix supplies
its second column; a numeric column named for the response supplies
indices directly. For a response such as `cbind(y, index)`, the `index`
expression can be supplied without `y`. Missing indices behave like
missing required predictors. Link-scale draws do not require family
membership.

Grouped parameter output includes the latent location for ordinal
components. Grouped expectations include scalar fitted means and ordinal
category probabilities, with `.category` identifying the ordinal
category and `NA` for scalar components. Distributional nuisance
parameters are held at their fitted values. This does not add support
for general/location-scale families that
[`gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html) itself rejects, nor
repair upstream fitting failures (including fixed-theta
negative-binomial components and some
[`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html) fits in some mgcv
versions).

Observation rows use the same missing-value convention as
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md):
default results restore `na.exclude` observations in every draw, while
explicit `data` are evaluated in their own row order.

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

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

fs <- fitted_samples(m1, n_draws = 5, seed = 42)
# \donttest{
fs
#> # A tibble: 5,000 x 4
#>     .row .draw .parameter .fitted
#>    <int> <int> <chr>        <dbl>
#>  1     1     1 mean          6.34
#>  2     2     1 mean          5.08
#>  3     3     1 mean          6.84
#>  4     4     1 mean          7.71
#>  5     5     1 mean          9.23
#>  6     6     1 mean          8.03
#>  7     7     1 mean          6.19
#>  8     8     1 mean          7.28
#>  9     9     1 mean         14.0 
#> 10    10     1 mean         12.7 
#> # i 4,990 more rows
# }

# can generate own set of draws and use them
drws <- generate_draws(m1, n_draws = 2, seed = 24)
fs2 <- fitted_samples(m1, method = "user", draws = drws)
# \donttest{
fs2
#> # A tibble: 2,000 x 4
#>     .row .draw .parameter .fitted
#>    <int> <int> <chr>        <dbl>
#>  1     1     1 mean          6.30
#>  2     2     1 mean          5.12
#>  3     3     1 mean          7.40
#>  4     4     1 mean          7.42
#>  5     5     1 mean          9.40
#>  6     6     1 mean          8.04
#>  7     7     1 mean          5.83
#>  8     8     1 mean          7.30
#>  9     9     1 mean         14.3 
#> 10    10     1 mean         13.1 
#> # i 1,990 more rows
# }
```
