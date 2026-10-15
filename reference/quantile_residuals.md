# PIT and randomised quantile residuals

Map observations to probabilities under the fitted response
distribution. These probability integral transform (PIT) residuals have
a uniform reference distribution under a correctly specified continuous
model with known parameters. Quantile residuals transform them to
standard-normal scores. Discrete observations require randomisation over
their probability mass.

## Usage

``` r
# S3 method for class 'gamm4'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'gamlss'
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# S3 method for class 'gamm'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'gratia_diagnostic_proxy'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'lme'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'merMod'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# S3 method for class 'gam'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'glm'
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# S3 method for class 'lm'
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# Default S3 method
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'glmmTMB'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'gratia_glmmtmb_diagnostic'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  ...
)

# S3 method for class 'sdmTMB'
quantile_residuals(
  model,
  type = c("pit", "quantile"),
  seed = NULL,
  conditioning = NULL,
  component = "response",
  field_method = c("mle-mvn", "mle-eb", "mle-mvn-integrated"),
  ...
)
```

## Arguments

- model:

  a fitted model object.

- type:

  character; `"pit"` returns probabilities and `"quantile"` returns
  standard-normal scores.

- seed:

  integer or `NULL`; random seed for randomised residuals. With `NULL`,
  calculations use the current random-number state. An explicit seed
  preserves the caller's state, including when none was previously
  initialised.

- conditioning:

  character or `NULL`; hold fitted random effects fixed
  (`"conditional"`), average over new effects (`"marginal"`), or use the
  extracted GAM (`"gam_component"`), where supported. See
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- ...:

  arguments passed to other methods.

- component:

  character; which delta sdmTMB component to check; see
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md).

- field_method:

  For sdmTMB diagnostics, `"mle-mvn"` (the default) follows sdmTMB's
  recommended residual calculation. Draw the fitted random effects
  together once from their approximate posterior, then use that same
  draw to evaluate the response CDF and generate every reference
  response. Fixed coefficients and covariance parameters remain at their
  maximum likelihood estimates. The independent seed in
  `gratia.diagnostics.field_seed` (default

  1.  keeps different diagnostic panels on the same draw of the effects.
      `"mle-eb"` uses the fitted empirical-Bayes effects without drawing
      their uncertainty; sdmTMB does not recommend this mode for
      goodness-of-fit testing. `"mle-mvn-integrated"` draws new values
      of the fitted effects for each simulation, from their approximate
      posterior. Residuals use the rank of each response among the
      simulated reference responses. This averages over uncertainty in
      the fitted effects, but reuses the data used in fitting and can
      make goodness-of-fit checks less likely to detect departures.
      These residuals differ from sdmTMB's calculation at a single draw
      of the effects. With `conditioning = "marginal"`, new spatial,
      temporal and group effects are drawn from their fitted prior
      distributions, while smooth effects remain at fitted values;
      `field_method` does not add another draw of the fields. These
      approximations do not guarantee that goodness-of-fit tests have
      their intended false-positive rate after parameter estimation.
      Posterior predictive checks separately include coefficient and
      field uncertainty; see
      [`posterior_predictive_plot()`](https://gavinsimpson.github.io/gratia/reference/posterior_predictive_plot.md).
      Student-t responses use the fitted location, scale and fixed or
      estimated degrees of freedom. The scale is not the response
      standard deviation. Pearson residuals require degrees of freedom
      greater than two, because their response variance is otherwise
      infinite. PIT, quantile residuals and posterior predictive checks
      remain available when this variance is infinite.

## Details

With `na.exclude`, excluded observations are restored as `NA` residuals;
with `na.omit`, only observations used for fitting are returned.

For [`mgcv::cnorm()`](https://rdrr.io/pkg/mgcv/man/cnorm.html),
[`mgcv::clog()`](https://rdrr.io/pkg/mgcv/man/clog.html) and
[`mgcv::cpois()`](https://rdrr.io/pkg/mgcv/man/cpois.html) models,
censored observations have PIT residuals sampled uniformly between
`F(l)` and `F(u)`. Here `l` and `u` bound the censoring interval and `F`
is the fitted cumulative distribution function (CDF) of the underlying
response. Left and right censoring use probabilities zero and one for
the unbounded end, respectively. Quantile residuals apply
[`qnorm()`](https://rdrr.io/r/stats/Normal.html) to these PIT values.
Uncensored continuous observations use `F(y)`; uncensored Poisson
observations are randomised between `F(y - 1)` and `F(y)`. Use
non-integer censoring limits for
[`cpois()`](https://rdrr.io/pkg/mgcv/man/cpois.html), as recommended by
mgcv.

For grouped-family GAMs fitted with
[`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html), each
observation uses its component's CDF, fitted nuisance parameters,
dispersion and weights. Residuals are returned in fitting order. All
active components must supply supported CDFs and probability limits for
discrete or censored observations. Unsupported components give an error;
[`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
can instead select deviance residuals automatically. See
[`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
for supported families. Tweedie zeros have positive probability mass:
their PITs are randomised between `F(0-) = 0` and `F(0)`. Positive
observations use `F(y)`.

When a CDF can calculate log probabilities in both tails, quantile
residuals use the log probability in the smaller tail directly. This
avoids infinities caused by rounding probabilities to zero or one.
Probabilities are not clipped, so true zero-probability tails still give
infinite residuals for ordinary models; grouped diagnostics report
non-finite residuals as errors. PITs are returned as ordinary
probabilities and may still round to zero or one in extreme tails.
Applying [`qnorm()`](https://rdrr.io/r/stats/Normal.html) to returned
PITs can therefore be less accurate than requesting quantile residuals
directly. CDFs without log probabilities retain ordinary-probability
calculations.

Marginal checks may calculate residuals from ranks in joint response
simulations when no analytic marginal CDF is available. See
[diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md)
for the rank definition, conditioning choices and limitations after
fitting.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
