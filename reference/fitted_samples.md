# Draw fitted values from the posterior distribution

Expectations (fitted values) of the response drawn from the posterior
distribution of fitted model using a Gaussian approximation to the
posterior or a simple Metropolis Hastings sampler.

## Usage

``` r
# S3 method for class 'gamm4'
fitted_samples(model, ...)

fitted_samples(model, ...)

# S3 method for class 'gam'
fitted_samples(
  model,
  n = 1,
  data = newdata,
  seed = NULL,
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  freq = FALSE,
  unconditional = FALSE,
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  family_index = NULL,
  ...,
  newdata = NULL,
  ncores = NULL
)

# S3 method for class 'scam'
fitted_samples(
  model,
  n = 1,
  data = NULL,
  seed = NULL,
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n_cores = 1,
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  freq = FALSE,
  unconditional = FALSE,
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  ...
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

- n:

  numeric; the number of posterior samples to return.

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `data`, if available in `model`.

- seed:

  numeric; a random seed for the simulations.

- scale:

  character; either `"response"` for expectations of the response or
  `"linear_predictor"` for values before the inverse link
  transformation. For multinomial models both scales return all
  categories, labelled `response1`, `response2`, and so on. On the
  linear predictor scale, `response1` is the reference category with
  predictor zero; the other values are log odds relative to it. On the
  response scale, the values are category probabilities.

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

- family_index:

  numeric; optional family-list positions for grouped family
  ([`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html)) response
  predictions, with one integer per requested row. By default membership
  is recovered from the response/index expression in `data`, or the
  fitted membership when `data` is omitted. This is useful when
  predicting without responses or with a complex response expression. An
  explicit index must agree with membership recoverable from `data`.

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

For general families with several distributional parameters,
response-scale draws currently support
[`mgcv::gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) means,
[`mgcv::mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html) response means,
and [`mgcv::multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html)
category probabilities. Other general families, including
[`mgcv::ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html), produce an
error on the response scale because their first parameter is not
necessarily the expected response. On the linear predictor scale, these
other families return the first predictor under the existing `location`
label, including its offsets. The same label identifies the mean
predictor for [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html);
scale predictors are omitted.

For grouped families, coefficient draws preserve the joint model
covariance; each row is then transformed using its own fitted component
family. Explicit prediction data must identify each row's family: no
training-order fallback is used. A two-column response matrix supplies
its second column; a numeric column named for the response supplies
indices directly. For a response such as `cbind(y, index)`, the `index`
expression can be supplied without `y`. Missing indices behave like
missing required predictors. Link-scale draws do not require family
membership.

Grouped response-scale draws currently support scalar fitted means,
including families with a scalar prediction transform such as
[`mgcv::ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html). They do not
support [`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html)
category-probability output; use the link scale for grouped ordinal
models. Distributional nuisance parameters are held at their fitted
values. This does not add support for general/location-scale families
that [`gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html) itself rejects,
nor repair upstream fitting failures (including fixed-theta
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

fs <- fitted_samples(m1, n = 5, seed = 42)
# \donttest{
fs
#> # A tibble: 5,000 x 4
#>     .row .draw .parameter .fitted
#>    <int> <int> <chr>        <dbl>
#>  1     1     1 location      6.34
#>  2     2     1 location      5.08
#>  3     3     1 location      6.84
#>  4     4     1 location      7.71
#>  5     5     1 location      9.23
#>  6     6     1 location      8.03
#>  7     7     1 location      6.19
#>  8     8     1 location      7.28
#>  9     9     1 location     14.0 
#> 10    10     1 location     12.7 
#> # i 4,990 more rows
# }

# can generate own set of draws and use them
drws <- generate_draws(m1, n = 2, seed = 24)
fs2 <- fitted_samples(m1, method = "user", draws = drws)
# \donttest{
fs2
#> # A tibble: 2,000 x 4
#>     .row .draw .parameter .fitted
#>    <int> <int> <chr>        <dbl>
#>  1     1     1 location      6.30
#>  2     2     1 location      5.12
#>  3     3     1 location      7.40
#>  4     4     1 location      7.42
#>  5     5     1 location      9.40
#>  6     6     1 location      8.04
#>  7     7     1 location      5.83
#>  8     8     1 location      7.30
#>  9     9     1 location     14.3 
#> 10    10     1 location     13.1 
#> # i 1,990 more rows
# }
```
