# Simulate responses from a fitted model

Generate replicated responses using fitted parameter values.
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
also includes uncertainty in the coefficients. See
[diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

## Usage

``` r
# S3 method for class 'gamm4'
simulate(object, nsim = 1, seed = NULL, conditioning = NULL, ...)

# S3 method for class 'gamlss'
simulate(object, nsim = 1L, seed = NULL, ...)

# S3 method for class 'gam'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL,
  family_index = NULL,
  envir = NULL,
  marginalise = NULL
)

# S3 method for class 'gamm'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL,
  conditioning = NULL,
  envir = NULL
)

# S3 method for class 'scam'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL,
  envir = NULL
)
```

## Arguments

- object:

  a fitted GAM, typically the result of a call to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html), or
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html).

- nsim:

  numeric; number of response simulations to return.

- seed:

  numeric; a random seed for the simulations.

- conditioning:

  character or `NULL`; optional mixed-model conditioning mode. gamm4
  defaults to `"conditional"`, holding all fitted effects fixed.
  `"gam_component"` explicitly uses its extracted GAM. The existing gamm
  simulator retains its GAM-component default. Full mixed-model modes
  currently use fitting observations.

- ...:

  arguments passed to methods. `simulate.gam()` and `simulate.scam()`
  pass `...` on to
  [`predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html). As
  such you can pass additional arguments such as `terms`, `exclude`, to
  select which model terms are included in the predictions. This may be
  useful, for example, for excluding the effects of random effect terms.

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `newdata`, if available in `object`.

- weights:

  numeric; response sampling weights. An explicit vector overrides the
  fitting weights; a single value applies to every observation. With
  `data = NULL`, the default is the fitted prior weights, except
  factor-valued binomial responses retain one Bernoulli trial per
  observation. Native
  [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) ignores fitting
  weights in its likelihood, so its default response weights are ones;
  explicit sampling weights still override this. With prediction data,
  the default is a vector of ones. For ordinary Gaussian and Gamma
  responses, larger weights reduce response variance (precision
  weights). For binomial responses, weights give integer trial counts.
  Include positions restored by `na.exclude` when supplying weights for
  fitting rows.

- newdata:

  Deprecated. Use `data` instead.

- family_index:

  numeric; for grouped-family models, one integer family index per
  requested prediction row. See Details.

- envir:

  environment or `NULL`; functions and constants needed to evaluate
  model expressions in prediction data. With `NULL`, use the model's
  formula environment. Supply raw covariates in `data`.

- marginalise:

  `NULL` retains the usual predictions. `TRUE` selects all supported
  `re` and `fs` terms; character vectors select exact smooth labels.
  Fitted predictions integrate response means over new zero-mean
  Gaussian effects. Response samplers draw new effects before sampling
  responses. Smoothing parameters and dispersion remain fixed. Only
  ordinary [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) fits with a scalar
  response mean are supported.

## Value

(Currently) A data frame with `nsim` columns.

## Details

For `simulate.gam()` to function, the `family` component of the fitted
model must contain, or be updatable to contain, the required random
number generator. See
[`mgcv::fix.family.rd()`](https://rdrr.io/pkg/mgcv/man/fix.family.link.html).

With `marginalise`, each replicate draws new coefficients for selected
`re` and `fs` terms before drawing responses. Other coefficients and all
variance parameters stay fixed. Supply grouping columns for new data;
repeated group labels share effects within a replicate. Unfamiliar
labels are allowed only when those factors occur exclusively in selected
terms. The `random_effect_info` attribute records the fixed covariance
estimates.

With `data = NULL`, `na.exclude` restores excluded observations as `NA`
in every simulation; `na.omit` returns retained observations only.
Positions refer to the fitting data after any `subset`. Explicit `data`
are evaluated independently of training exclusions: missing responses do
not prevent simulation, while missing required predictors or weights
yield `NA`. Prediction `na.action` in `...` is honoured. Missing rows
are never passed to the response random-number generator.

Inverse Gaussian models use a native generator with observation
dispersion equal to the model dispersion divided by the prior weight,
consistently with their CDF and quantile helpers. Weights must be
non-negative; a zero weight with positive dispersion uses the limiting
point mass at zero. An existing random generator supplied in the
family's `rd` component is preserved.

For models fitted with
[`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html), each
observation is simulated from its component family using that
component's fitted scale and distribution parameters. With
`data = NULL`, fitted family membership is used. For explicit `data`,
supply `family_index` or the index in the model's response expression:
the second column of a matrix response, the second argument of
`cbind(y, index)`, or a numeric index vector in place of a named matrix
response. Actual response observations are not needed. Missing indices
follow prediction `na.action`; invalid indices are errors. Explicit
indices must agree with recoverable ones.

Grouped simulations return one numeric response per observation:
binomial proportions (with integer trial counts supplied in `weights`),
count responses, or ordered-category codes, as appropriate. Component
random generators retain their usual treatment of prior weights.
Ordered-categorical and `ziP` components are simulated from their latent
predictors. General families with multiple linear predictors and matrix
responses encoding censoring or binomial counts are outside the scope
supported by [`gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html). Scalar
censored-family components produce draws from their uncensored latent
distributions.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

sims <- simulate(m1, nsim = 5, seed = 42)
head(sims)
#>       sim_1     sim_2     sim_3     sim_4     sim_5
#> 1 11.445470 11.374304 10.098681  7.264881  8.796630
#> 2  6.510912  5.909584  9.057362  7.698084 11.444781
#> 3  3.837995  3.230610  3.550240  3.759380  4.774581
#> 4 12.361830 11.209226 10.714215 11.861957 10.746417
#> 5 14.851461 12.911440 11.356984 15.783913 15.106270
#> 6  5.921276  4.158963  5.520856  7.973614  9.654888
```
