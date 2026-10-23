# Generate fitted values from a estimated GAM

Generate fitted values from a estimated GAM

## Usage

``` r
fitted_values(object, ...)

# S3 method for class 'gam'
fitted_values(
  object,
  data = NULL,
  scale = c("response", "link", "linear predictor"),
  ci_level = 0.95,
  envir = NULL,
  ...,
  interval = c("confidence", "simultaneous"),
  n_sim = 10000,
  n_cores = 1,
  seed = NULL,
  unconditional = FALSE,
  interval_method = c("auto", "gaussian", "bootstrap"),
  quantity = c("parameters", "expectation"),
  family_index = NULL,
  marginalise = NULL,
  integration = c("quadrature", "monte_carlo"),
  n_quad = 40L,
  n_integrate = 5000L
)

# S3 method for class 'gamm'
fitted_values(object, ...)

# S3 method for class 'scam'
fitted_values(object, ..., unconditional = FALSE)

# S3 method for class 'gamm4'
fitted_values(object, ...)
```

## Arguments

- object:

  a fitted model. Currently only models fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) are supported.

- ...:

  arguments passed to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).
  Note that `type`, `newdata`, and `se.fit` are already used and passed
  on to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).

- data:

  optional data frame of covariate values for which fitted values are to
  be returned.

- scale:

  character; what scale should the fitted values be returned on?
  `"linear predictor"` is a synonym for `"link"` if you prefer that
  terminology. `"linear_predictor"` is also accepted. Expectations
  require `"response"`; parameter predictions use natural values or
  their predictors.

- ci_level:

  numeric; a value between 0 and 1 indicating the coverage of the
  credible interval.

- envir:

  an optional environment supplying functions and constants used in
  model expressions. The available model formula environment is used
  when `NULL`. Covariate observations should be supplied in `data`.

- interval:

  character; `"confidence"` (the default) gives pointwise intervals.
  `"simultaneous"` gives simultaneous intervals for predictions at the
  supplied covariate combinations.

- n_sim:

  positive integer; number of coefficient draws used for simultaneous
  intervals. Ignored for pointwise intervals.

- n_cores:

  positive integer; number of cores used by
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html) for
  simultaneous intervals. Parallel execution requires OpenMP support.

- seed:

  integer or `NULL`; optional random seed for simultaneous intervals. An
  explicit seed preserves the caller's random number state. With `NULL`,
  the current random number state is used and advanced.

- unconditional:

  logical; include smoothing parameter uncertainty in the Bayesian
  covariance matrix, if available. Otherwise a warning is issued and the
  uncorrected covariance is used.

- interval_method:

  character; `"auto"` uses stored scasm bootstrap draws for pointwise
  intervals when available and Gaussian intervals otherwise.
  `"gaussian"` requests covariance-based limits and `"bootstrap"`
  requires stored draws. Simultaneous bootstrap bands are unsupported.
  Shape-enforced intervals are available for smooth components through
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md).

- quantity:

  character; `"parameters"` (the default) returns all distribution
  parameters represented by predictors, including intercept-only
  predictors. `"expectation"` returns the expected response, using every
  parameter on which it depends, on the response scale only.

- family_index:

  numeric; optional family-list positions for grouped family
  ([`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html)) response
  predictions, with one integer per requested row. By default membership
  is recovered from the response/index expression in `data`, or the
  fitted membership when `data` is omitted. This is useful when
  predicting without responses or with a complex response expression. An
  explicit index must agree with membership recoverable from `data`.

- marginalise:

  `NULL` retains the usual predictions. `TRUE` selects all supported
  `re` and `fs` terms; character vectors select exact smooth labels.
  Fitted predictions integrate response means over new zero-mean
  Gaussian effects. Response samplers draw new effects before sampling
  responses. Smoothing parameters and dispersion remain fixed. Only
  ordinary [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) fits with a scalar
  response mean are supported.

- integration:

  Integration method when `marginalise` is supplied: `"quadrature"` (the
  default) or `"monte_carlo"`. Both integrate over the Gaussian
  distribution of the selected effects at each row.

- n_quad:

  Initial Gauss-Hermite point count, between 2 and 1000. Successive
  doubled orders, up to four times this count, and an interlaced rule
  with one fewer point must agree within `1e-8 + 1e-6 * abs(value)` for
  means and derivatives. Where the random-effect variance exceeds one,
  an independent adaptive rule also checks agreement. Failure to agree
  reports an error. Identity, log and probit links use their closed-form
  Gaussian integrals; their metadata records zero quadrature points and
  zero integration error.

- n_integrate:

  Number of Monte Carlo integration draws, at least two. These are
  shared across rows and posterior coefficient draws. This count is
  separate from `n_draws`. Ignored for quadrature.

## Value

A tibble (data frame) whose first *m* columns contain either the data
used to fit the model (if `data` was `NULL`), or the variables supplied
to `data`. The result also includes:

- `.parameter`: parameter name, or `"expectation"`,

- `.fitted`: the requested quantity on the specified scale,

- `.se`: standard error; see `prediction_info` for its scale,

- `.lower_ci`, `.upper_ci`: the limits of the credible interval on the
  fitted values, on the specified scale.

Models fitted with certain families will include additional variables

- [`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) models: when
  `quantity = "expectation"`, the returned object will contain a `.row`
  column and a `.category` column, which indicate to which row of the
  `data` each row of the returned object belongs. Additionally, there
  will be `nrow(data) * n_categories` rows in the returned object; each
  row is the predicted probability for a single category of the
  response.

## Details

With `marginalise`, predictions are response means averaged over the
selected random-effect distributions, using the methods described in
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md).
The point estimate uses fitted coefficients; pointwise intervals use the
derivative of the integrated mean and the coefficient covariance. `.se`
is on the response scale. These normal intervals are not clipped to the
response range. Simultaneous and bootstrap marginal intervals are not
yet supported. The `integration_info` attribute records numerical
accuracy and fixed variance estimates.

`quantity = "parameters"` returns all distribution parameters
represented by predictors, including intercept-only predictors. See
[`family_parameters()`](https://gavinsimpson.github.io/gratia/reference/family_parameters.md)
for their definitions. These are natural distribution parameters, which
can differ from mgcv's native response predictions: Gaussian LSS returns
standard deviation, GEV returns location rather than expectation, and
sinh-arcsinh returns shape parameters rather than response moments.

`quantity = "expectation"` returns the expected response on the response
scale, labelled `"expectation"`. Categorical expectations are
probabilities of category membership; multivariate models return a mean
per response. `.category` and `.y` identify the category or response
separately from `.parameter`. Global parameters are described but held
fixed when calculating coefficient uncertainty. Coupled expectations use
the full coefficient covariance and a delta-method Jacobian. Their
intervals are pointwise.

The `prediction_info` attribute records the quantity, scale,
standard-error scale and parameter descriptions. Parameter standard
errors retain their link-scale meaning; category-probability and
expectation standard errors are on the response scale. Unsupported
simultaneous or bootstrap transformations are rejected rather than
combining marginal parameter intervals.

Native scasm fits with stored bootstrap draws use mgcv's pointwise
limits by default, including its `svar.only` correction for smoothing
bias. `interval_method = "gaussian"` restores covariance-based limits.
`.se` retains its existing covariance-based meaning. No replicates are
generated. Bootstrap limits use existing scalar and location-scale
parameter transformations. Unsupported nonlinear family outputs give an
error for an explicit bootstrap request; automatic selection warns and
uses Gaussian limits. Simultaneous bootstrap bands are not supported.
Automatic simultaneous intervals warn when stored bootstrap draws are
left unused. The `interval_info` attribute records the method and
correction.

Formula offsets are included in predictions. Offsets supplied only
through the fitting function's `offset` argument are omitted, including
when `data = NULL`: this function evaluates explicit prediction rows
internally. In contrast,
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
retains fitting-argument offsets when its `data` argument is `NULL`. Put
offsets in the formula to evaluate them at new observations.

With `data = NULL`, results follow the model's `na.action`: `na.exclude`
restores excluded observations as `NA` fitted values, standard errors
and interval bounds; `na.omit` returns only retained observations.
Covariates unavailable in the stored model frame are restored as typed
`NA` values. `.row` indexes the fitting data after any `subset` was
applied.

Supplying `data` requests predictions at those rows, independently of
training exclusions. Missing responses do not prevent prediction.
Missing required predictors produce `NA` results by default. An explicit
`na.action` passed through `...` is honoured; with `na.omit`, `.row`
retains the positions in the supplied data, rather than numbering the
remaining rows consecutively.

For supplied prediction data, an informative message identifies
combinations of categorical predictor levels absent from the retained
fitting observations. Predictions are unchanged: an unobserved
combination is not necessarily non-estimable, particularly in additive
models. Numeric-by-factor range extrapolation is not checked. Missing
predictors and genuinely new factor levels retain their usual prediction
behavior.

Simultaneous intervals jointly cover the model's underlying expected
responses, or linear predictors on the link scale, at all valid
covariate combinations evaluated in this call, with approximate
posterior probability `ci_level`. The fitted values are estimates of
these quantities. The combinations may include factor levels and need
not form an ordered sequence, curve, or grid. This interpretation also
applies to two predictions. With `data = NULL`, the simultaneous set
comprises the retained fitting covariate combinations. Missing
predictions do not enter this set. Coverage is not asserted outside this
set or between its points. Separate calls define separate simultaneous
sets. These are uncertainty intervals for model predictions, not
prediction intervals for future observations.

Simultaneous intervals use joint Gaussian coefficient draws and a
critical value based on the maximum absolute standardized deviation over
the supplied rows. Their limits are computed on the link scale and
transformed if `scale = "response"`; `.se` remains on the link scale.
The simulation covariance may be positive semidefinite. Supported
families are Gaussian, Poisson, binomial, Gamma, inverse Gaussian, quasi
families, negative binomial, Tweedie, beta regression, and scaled t,
fitted by [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) (including the `gam`
component of a [`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) fit),
and by `scam()`. For SCAM models, the Gaussian approximation uses
transformed coefficients conditional on the fitted smoothing parameters.
Other families support pointwise intervals only.

Response-scale simultaneous intervals support the identity, log, logit,
probit, cloglog, cauchit, square-root, inverse, and inverse-squared
links. Intervals crossing an inverse-link domain boundary are rejected;
use `scale = "link"` for these intervals or for other links.

`terms` and `exclude` in `...` retain
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
semantics and apply to both prediction and interval calculation. If
terms are selected or excluded, intervals concern that selected
quantity, not necessarily the full expected response. Use the link scale
for sums of smooth contributions, and consider the intercept and offset
treatment when selecting terms.

## Note

For parameter output, the `.se` component is on the *link* (*linear
predictor*) scale. Multinomial probabilities on the response scale and
all expected responses have response-scale standard errors.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Examples

``` r
load_mgcv()
sim_df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = sim_df, method = "REML")
fv <- fitted_values(m)
fv
#> # A tibble: 400 x 10
#>     .row       x0        x1         x2       x3 .parameter  .fitted      .se
#>    <int>    <dbl>     <dbl>      <dbl>    <dbl> <chr>         <dbl>    <dbl>
#>  1     1 0.184882 0.617142  0.415244   0.132410 mean        8.73875 0.354677
#>  2     2 0.702374 0.569064  0.531439   0.365331 mean        7.62581 0.337779
#>  3     3 0.573326 0.153970  0.00324621 0.454532 mean        3.12106 0.591862
#>  4     4 0.168052 0.0348332 0.252100   0.537114 mean       11.1124  0.402378
#>  5     5 0.943839 0.997953  0.155229   0.185495 mean       14.0533  0.452947
#>  6     6 0.943475 0.835574  0.878840   0.449276 mean        6.13080 0.364521
#>  7     7 0.129159 0.586562  0.203511   0.256527 mean       12.4838  0.355808
#>  8     8 0.833449 0.339117  0.583528   0.618458 mean        6.25215 0.344700
#>  9     9 0.468019 0.166883  0.804473   0.880744 mean        4.21463 0.372003
#> 10    10 0.549984 0.807410  0.264717   0.317747 mean       15.5283  0.369999
#> # i 390 more rows
#> # i 2 more variables: .lower_ci <dbl>, .upper_ci <dbl>

# Simultaneous intervals for two arbitrary covariate combinations
fitted_values(m, data = sim_df[c(1, 20), ], interval = "simultaneous",
  n_sim = 1000, seed = 42)
#> # A tibble: 2 x 16
#>    .row       y        x0       x1       x2       x3       f       f0      f1
#>   <int>   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>   <dbl>    <dbl>   <dbl>
#> 1     1 7.12834 0.184882  0.617142 0.415244 0.132410 8.38643 1.09743  3.43592
#> 2     2 2.93507 0.0749794 0.797900 0.874521 0.203225 5.57766 0.466765 4.93227
#> # i 7 more variables: f2 <dbl>, f3 <dbl>, .parameter <chr>, .fitted <dbl>,
#> #   .se <dbl>, .lower_ci <dbl>, .upper_ci <dbl>

# A selected sum of smooth contributions on the link scale
fitted_values(m, data = sim_df[c(1, 20), ], scale = "link",
  terms = c("s(x2)", "s(x3)"), interval = "simultaneous",
  n_sim = 1000, seed = 42)
#> # A tibble: 2 x 16
#>    .row       y        x0       x1       x2       x3       f       f0      f1
#>   <int>   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>   <dbl>    <dbl>   <dbl>
#> 1     1 7.12834 0.184882  0.617142 0.415244 0.132410 8.38643 1.09743  3.43592
#> 2     2 2.93507 0.0749794 0.797900 0.874521 0.203225 5.57766 0.466765 4.93227
#> # i 7 more variables: f2 <dbl>, f3 <dbl>, .parameter <chr>, .fitted <dbl>,
#> #   .se <dbl>, .lower_ci <dbl>, .upper_ci <dbl>
```
