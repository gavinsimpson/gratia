# Posterior draws for individual smooths

Returns draws from the posterior distributions of smooth functions in a
GAM. Useful, for example, for visualising the uncertainty in individual
estimated functions.

## Usage

``` r
# S3 method for class 'gamm4'
smooth_samples(model, ...)

smooth_samples(model, ...)

# S3 method for class 'gam'
smooth_samples(
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
  select = NULL,
  n_grid = 200L,
  partial_match = NULL,
  envir = NULL
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
  For
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  these are passed on to
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md).
  For
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

- select:

  character; select which smooth's posterior to draw from. The default
  (`NULL`) means the posteriors of all smooths in `model` will be
  sampled from. If supplied, a character vector of requested terms.
  0.8.1 or earlier is used, whereby a separate call the the random
  number generator (RNG) is performed for each smooth. If FALSE, a
  single call to the RNG is performed for all model parameters

- n_grid:

  positive integer; resolution for constructing evaluation data if
  `data` not supplied

- partial_match:

  logical; should smooths be selected by partial matches with `select`?
  If `TRUE`, `select` can only be a single string to match against.

- envir:

  an optional environment for looking up data, functions and constants.
  Defaults to the model's evaluation environment.

## Value

A tibble with additional classes `"smooth_samples"` and
\`"posterior_samples".

For the `"gam"` method, the columns currently returned (not in this
order) are:

- `.smooth`; character vector. Indicates the smooth function for that
  particular draw,

- `.term`; character vector. Similar to `smooth`, but will contain the
  full label for the smooth, to differentiate factor-by smooths for
  example.

- `.by`; character vector. If the smooth involves a `by` term, the by
  variable will be named here, `NA_character_` otherwise.

- `.row`; integer. A vector of values `seq_len(nrow(data))`, repeated if
  `n_draws > 1L`. Indexes the row in `data` for that particular draw.

- `.draw`; integer. A vector of integer values indexing the particular
  posterior draw that each row belongs to.

- `.value`; numeric. The value of smooth function for this posterior
  draw and covariate combination.

- `xxx`; numeric. A series of one or more columns containing data
  required for the smooth, named as per the variables involved in the
  respective smooth.

- Additional columns will be present in the case of factor by smooths,
  which will contain the level for the factor named in `by_variable` for
  that particular posterior draw.

## Details

For LSS models, each smooth is a contribution to its own linear
predictor. Select its full smooth label to distinguish predictors, for
example `s(x)` and `s.1(x)`. These are component draws on the predictor
scale; use
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
for the expected response and
[`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
for its derivative. Coefficients are drawn jointly across all
predictors.

## Warning

The set of variables returned and their order in the tibble is subject
to change in future versions. Don't rely on position.

## Parallel calculation

With mirai and carrier installed, configure workers with
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html) to
distribute calculations across selected smooths.
[`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
distributes factor-level comparisons instead. Without configured
workers, or with only one calculation, evaluation is serial. These
functions leave the caller's worker pool running.

Model selection, evaluation of model expressions and random draws are
prepared in the caller. Workers receive the coordinates and coefficient
columns needed for their calculation. Custom prediction methods are
evaluated in the caller, including custom tensor margins and
factor-smooth bases; workers receive their prediction matrices.
Simultaneous intervals use successive draws in selection order, so
[`set.seed()`](https://rdrr.io/r/base/Random.html) gives the same
results with or without workers, independently of their number or
completion order.
[`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
uses [`set.seed()`](https://rdrr.io/r/base/Random.html); the other
functions also accept `seed`, which restores the caller's random number
state on exit.

`smooth_samples()` draws coefficients jointly once, then reuses those
draws for every smooth, preserving dependence and chain identities.
Sampling finishes before smooth calculations start, so MH chains and
smooth workers are not nested. Sampling thread controls keep their
existing meanings. Workers use their configured BLAS thread settings;
when using a threaded BLAS, limit its threads before starting daemons to
avoid competing threads.

Parallel work is most useful for expensive bases or interval
calculations. Small grids can be slower because of transfer and
scheduling costs. Large grids and many draws also require memory for
worker inputs and returned results, in addition to the final output.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 400, seed = 2)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

sms <- smooth_samples(m1, select = "s(x0)", n_draws = 5, seed = 42)
# \donttest{
sms
#> # A tibble: 1,000 x 8
#>    .smooth .term .type .by    .row .draw .value      x0
#>    <chr>   <chr> <chr> <chr> <int> <int>  <dbl>   <dbl>
#>  1 s(x0)   s(x0) TPRS  NA        1     1 -0.357 0.00711
#>  2 s(x0)   s(x0) TPRS  NA        1     2 -0.465 0.00711
#>  3 s(x0)   s(x0) TPRS  NA        1     3 -0.720 0.00711
#>  4 s(x0)   s(x0) TPRS  NA        1     4 -1.27  0.00711
#>  5 s(x0)   s(x0) TPRS  NA        1     5 -1.18  0.00711
#>  6 s(x0)   s(x0) TPRS  NA        2     1 -0.365 0.0121 
#>  7 s(x0)   s(x0) TPRS  NA        2     2 -0.464 0.0121 
#>  8 s(x0)   s(x0) TPRS  NA        2     3 -0.708 0.0121 
#>  9 s(x0)   s(x0) TPRS  NA        2     4 -1.24  0.0121 
#> 10 s(x0)   s(x0) TPRS  NA        2     5 -1.16  0.0121 
#> # i 990 more rows
# }

## A factor by example (with a spurious covariate x0)
dat <- data_sim("eg4", n = 1000, seed = 2)

## fit model...
m2 <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)
sms <- smooth_samples(m2, n_draws = 5, seed = 42)
draw(sms)
```
