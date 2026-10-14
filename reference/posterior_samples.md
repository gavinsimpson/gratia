# Draw samples from the posterior distribution of an estimated model

Draw samples from the posterior distribution of an estimated model

## Usage

``` r
# S3 method for class 'lm'
posterior_samples(model, n = 1L, seed = NULL, ...)

# S3 method for class 'glm'
posterior_samples(model, n = 1L, seed = NULL, ...)

# S3 method for class 'glmmTMB'
posterior_samples(model, n = 1L, seed = NULL, conditioning = NULL, ...)

# S3 method for class 'sdmTMB'
posterior_samples(model, n = 1L, seed = NULL, conditioning = NULL, ...)

# S3 method for class 'gamlss'
posterior_samples(model, n = 1L, seed = NULL, ...)

# S3 method for class 'gamm'
posterior_samples(model, n = 1L, seed = NULL, conditioning = NULL, ...)

# S3 method for class 'lme'
posterior_samples(model, n = 1L, seed = NULL, conditioning = NULL, ...)

# S3 method for class 'merMod'
posterior_samples(model, n = 1L, seed = NULL, conditioning = NULL, ...)

# S3 method for class 'gamm4'
posterior_samples(model, n = 1L, seed = NULL, ..., conditioning = NULL)

posterior_samples(model, ...)

# S3 method for class 'gam'
posterior_samples(
  model,
  n = 1,
  data = newdata,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  freq = FALSE,
  unconditional = FALSE,
  weights = NULL,
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  ...,
  newdata = NULL,
  ncores = NULL
)

# S3 method for class 'scam'
posterior_samples(
  model,
  n = 1,
  data = NULL,
  seed = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  freq = FALSE,
  unconditional = FALSE,
  weights = NULL,
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  ...
)
```

## Arguments

- model:

  a fitted model of the supported types; see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- n:

  numeric; the number of posterior samples to return.

- seed:

  numeric; a random seed for the simulations.

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

- conditioning:

  character or `NULL`; mixed-model conditioning mode; see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `data`, if available in `model`.

- method:

  character; which method should be used to draw samples from the
  posterior distribution. `"gaussian"` uses a Gaussian (Laplace)
  approximation to the posterior. `"mh"` uses a Metropolis Hastings
  sampler that alternates t proposals with proposals based on a shrunken
  version of the posterior covariance matrix. `"inla"` uses a variant of
  Integrated Nested Laplace Approximation due to Wood (2019), (currently
  not implemented). `"user"` allows for user-supplied posterior draws
  (currently not implemented).

- n_cores:

  number of cores for generating random variables from a multivariate
  normal distribution. Passed to
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html).
  Parallelization will take place only if OpenMP is supported (but
  appears to work on Windows with current `R`).

- burnin:

  numeric; number of samples to discard as the burnin draws. Only used
  with `method = "mh"`.

- thin:

  numeric; the number of samples to skip when taking `n` draws. Results
  in `thin * n` draws from the posterior being taken. Only used with
  `method = "mh"`.

- t_df:

  numeric; degrees of freedom for t distribution proposals. Only used
  with `method = "mh"`.

- rw_scale:

  numeric; Factor by which to scale posterior covariance matrix when
  generating random walk proposals. Negative or non finite to skip the
  random walk step. Only used with `method = "mh"`.

- freq:

  logical; `TRUE` to use the frequentist covariance matrix of the
  parameter estimators, `FALSE` to use the Bayesian posterior covariance
  matrix of the parameters.

- unconditional:

  logical; if `TRUE` (and `freq == FALSE`) then the Bayesian smoothing
  parameter uncertainty corrected covariance matrix is used, if
  available.

- weights:

  numeric; response sampling weights for GAM methods. An explicit vector
  overrides the fitting weights; a single value applies to every
  observation. With `data = NULL`, the default is the fitted prior
  weights, except factor-valued binomial responses retain one Bernoulli
  trial per observation. With prediction data, the default is a vector
  of ones. For ordinary Gaussian and Gamma responses, larger weights
  reduce response variance (precision weights). For binomial responses,
  weights give integer trial counts. They do not alter the fitted
  coefficient covariance. Include positions restored by `na.exclude`
  when supplying weights for fitting rows.

- draws:

  matrix; user supplied posterior draws to be used when
  `method = "user"`.

- mvn_method:

  character; one of `"mvnfast"` or `"mgcv"`. The default is uses
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html), which
  can be considerably faster at generate large numbers of MVN random
  values than [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html),
  but which might not work for some marginal fits, such as those where
  the covariance matrix is close to singular.

- newdata:

  Deprecated: use `data` instead.

- ncores:

  Deprecated; use `n_cores` instead. The number of cores for generating
  random variables from a multivariate normal distribution. Passed to
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html).
  Parallelization will take place only if OpenMP is supported (but
  appears to work on Windows with current `R`).

## Value

A tibble (data frame) with 3 columns containing the posterior predicted
values in long format. The columns are

- `row` (integer) the row of `data` that each posterior draw relates to,

- `draw` (integer) an index, in range `1:n`, indicating which draw each
  row relates to,

- `response` (numeric) the predicted response for the indicated row of
  `data`.

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
