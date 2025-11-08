# Draw samples from the posterior distribution of an estimated model

Draw samples from the posterior distribution of an estimated model

## Usage

``` r
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

  a fitted model of the supported types

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
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) method.

- n:

  numeric; the number of posterior samples to return.

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `data`, if available in `model`.

- seed:

  numeric; a random seed for the simulations.

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

  numeric; a vector of prior weights. If `data` is null then defaults to
  `object[["prior.weights"]]`, otherwise a vector of ones.

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

## Note

Models with offset terms supplied via the `offset` argument to
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) etc. are ignored
by
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).
As such, this kind of offset term is also ignored by
`posterior_samples()`. Offset terms that are included in the model
formula supplied to
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) etc are not
ignored and the posterior samples produced will reflect those offset
term values. This has the side effect of requiring any new data values
provided to `posterior_samples()` via the `data` argument must include
the offset variable.

## References

Wood, S.N., (2020). Simplified integrated nested Laplace approximation.
*Biometrika* **107**, 223–230.
[doi:10.1093/biomet/asz044](https://doi.org/10.1093/biomet/asz044)

## Author

Gavin L. Simpson
