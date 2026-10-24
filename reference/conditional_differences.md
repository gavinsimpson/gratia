# Conditional differences of fitted values from a GAM

Compare full fitted values between pairs of factor levels, conditional
on covariate values. Parametric effects and smooths are included unless
excluded explicitly. Unlike
[`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md),
these are differences of predictions, rather than differences of
selected smooth functions.

## Usage

``` r
conditional_differences(
  model,
  by,
  condition = NULL,
  data = NULL,
  scale = c("response", "link", "linear_predictor"),
  ...
)

# S3 method for class 'gam'
conditional_differences(
  model,
  by,
  condition = NULL,
  data = NULL,
  scale = c("response", "link", "linear_predictor"),
  n_vals = NULL,
  ci_level = 0.95,
  complete = TRUE,
  unconditional = FALSE,
  uncertainty = c("delta", "simulation"),
  n_sim = 10000,
  seed = NULL,
  n_cores = 1,
  method = c("gaussian", "mh", "inla", "user"),
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  burnin = 1000,
  thin = 1,
  t_df = 40,
  rw_scale = 0.25,
  envir = NULL,
  ...,
  interval = c("confidence", "simultaneous"),
  simultaneous_scope = c("contrast", "all")
)

# S3 method for class 'gamm4'
conditional_differences(model, ...)

# S3 method for class 'scam'
conditional_differences(model, ..., unconditional = FALSE)
```

## Arguments

- model:

  a fitted GAM object.

- by:

  character vector naming factors that define comparison groups. For
  multiple factors, groups are joint combinations of their levels and
  all pairs of groups are compared. To compare one factor within levels
  of another, put the latter in `condition` instead.

- condition:

  character vector or list specifying evaluation covariates, as in
  [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md).
  The first non-comparison covariate supplies the plotting axis. Up to
  four non-comparison conditions are supported. Named elements for
  factors in `by` restrict their comparison levels, e.g.
  `list("x", treatment = c("control", "high"))`. Other covariates are
  held at
  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md).
  At least one non-comparison condition is required. Group two numeric
  covariates in the first non-comparison element to request a surface,
  for example `list(c("x1", "x2"), "stratum")`. A nested list can supply
  explicit axis values or helpers, as in
  [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md).
  Both axes count towards the four-covariate limit. Remaining conditions
  supply facets, skipping categorical colour.

- data:

  optional data frame used to generate condition values and determine
  covariate classes. Explicit values in `condition` take precedence.
  Other covariates use the fitted model's typical values. The rows of
  `data` are not used directly as a prediction grid.

- scale:

  character; which scale should predictions be returned on?

- ...:

  arguments passed to prediction, such as `exclude`. Arguments `type`,
  `newdata`, `se.fit`, and `terms` are not supported here.

- n_vals:

  positive integer or `NULL`; number of values to generate along each
  numeric axis. For
  [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  and `conditional_differences()`, `NULL` (the default) uses 100 for
  curves and 50 per axis for surfaces. Explicit values or helpers in
  `condition` override generated values. Other numeric conditions use
  their five-number summaries unless explicit values or helpers are
  supplied.

- ci_level:

  numeric; a number on interval (0,1) giving the coverage for credible
  intervals.

- complete:

  logical; include all combinations of comparison and conditioning
  factors? With `FALSE`, retain only combinations observed in the fitted
  data, and compare groups only within shared conditioning strata. This
  does not restrict numeric grids to overlapping observed ranges.

- unconditional:

  logical; include smoothing-parameter uncertainty in the Bayesian
  covariance, if available, for delta intervals and Gaussian draws. This
  argument does not modify MH or user-supplied draws.

- uncertainty:

  character; `"delta"` uses analytic link-scale or delta-method
  response-scale standard errors and normal intervals. `"simulation"`
  uses shared posterior coefficient draws, with equal-tailed quantiles
  for pointwise intervals or calibrated symmetric simultaneous bands.

- n_sim:

  integer; number of draws for posterior simulation or calibration of
  delta-method simultaneous intervals. Ignored for pointwise delta
  intervals and for simulation with `method = "user"`, which uses all
  supplied draws.

- seed:

  integer or `NULL`; seed for posterior simulation or delta-method
  simultaneous calibration. An explicit seed preserves the caller's RNG
  state. With `NULL`, delta-method calibration advances the current RNG
  state; posterior simulation follows
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  seed handling.

- n_cores:

  integer; number of cores used by
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html) for
  delta-method simultaneous calibration, or passed to
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md).

- method:

  character; posterior sampler used by
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md).
  `"gaussian"` uses the Gaussian posterior approximation; `"mh"` uses
  Metropolis-Hastings; `"user"` uses supplied coefficient draws.
  `"inla"` is reserved by the sampling machinery but is not implemented.

- draws:

  matrix of posterior coefficient draws, one draw per row in model
  coefficient order, used with `method = "user"`. At least two are
  required.

- mvn_method:

  character; one of `"mvnfast"` or `"mgcv"`. The default is uses
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html), which
  can be considerably faster at generate large numbers of MVN random
  values than [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html),
  but which might not work for some marginal fits, such as those where
  the covariance matrix is close to singular.

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

- envir:

  an optional environment supplying functions and constants used in
  model expressions. The available model formula environment is used
  when `NULL`. Covariate observations should be supplied in `data`.

- interval:

  character; `"confidence"` (the default) gives pointwise intervals;
  `"simultaneous"` gives simultaneous intervals over the evaluated
  covariate combinations.

- simultaneous_scope:

  character; `"contrast"` calibrates a separate band for each pair,
  jointly over all its evaluation points and conditioning strata.
  `"all"` calibrates one band jointly over every returned comparison and
  evaluation point. Ignored for pointwise intervals.

## Value

A tibble of class `"conditional_differences"`, with `.contrast`,
`.diff`, `.se`, `.lower_ci`, `.upper_ci`, and evaluation covariates.
`.se` is on the requested scale; for simulation it is the standard
deviation of difference draws. A single comparison factor has `.level_1`
and `.level_2` columns. Multiple factors have `<factor>_1` and
`<factor>_2` columns. Attributes record `by`, `condition`, `scale`,
`uncertainty`, `ci_level`, `exclude`, `interval`, `simultaneous_scope`,
and plotting `channels`. Simultaneous results also contain `.crit`, the
calibrated critical value, constant within each coverage set.

## Details

Differences are group 1 minus group 2, with groups ordered by model
factor levels. Both groups use the same values of all other covariates.
On the response scale the inverse link is applied to each prediction
before subtraction. Shared terms can therefore affect response-scale
differences even when they cancel on the link scale.

Intervals describe uncertainty in conditional mean differences, not
observation noise. Simulation uses
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
once for the entire prediction grid, retaining dependence between
predictions by subtracting within draws. The estimate is always the
difference of fitted predictions. Pointwise simulation intervals need
not be centred on this estimate.

Simultaneous intervals use the `ci_level` quantile of the maximum
absolute standardized deviation over the selected coverage set. They are
symmetric about `.diff`, with half-width `.crit * .se`. For
`uncertainty = "delta"`, one shared batch of zero-mean Gaussian
coefficient deviations is generated from the Bayesian covariance matrix.
Response-scale differences use a first-order Taylor approximation. The
covariance must be positive semidefinite; singular covariance matrices
are supported. Sampler arguments `method`, `draws`, `mvn_method`,
`burnin`, `thin`, `t_df`, and `rw_scale` apply only to
`uncertainty = "simulation"`.

For `uncertainty = "simulation"`, deviations are posterior difference
draws minus `.diff`, standardized by their sample standard deviations.
Draws are transformed to the response scale before differencing; invalid
inverse-link domains or response means cause an error rather than
discarded draws. Zero-variance draws that disagree with `.diff` cannot
calibrate a band and cause an error. Deterministic rows agreeing with
`.diff` have zero width.

Simultaneous coverage is approximate posterior coverage on the finite
evaluation grid, not between its points or outside its range, nor exact
frequentist coverage. Increase `n_vals` to evaluate a denser grid. Bands
are not clipped to the response-difference range (e.g. `[-1, 1]` for
probabilities). They need not contain the equal-tailed pointwise
simulation intervals.

`exclude` sets the named term contributions to zero. In particular,
excluding random effects does not integrate over their distribution.
Offsets follow
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
semantics. Supported models are `gam` and `bam` fits with a single
linear predictor and an ordinary scalar inverse-link mean (including
negative binomial, Tweedie, beta regression and scaled t families). SCAM
fits with supported single-predictor families also support link- and
response-scale differences, with delta or simulation uncertainty and
pointwise or simultaneous intervals. SCAM delta intervals use the local
covariance of transformed coefficients. Gaussian simulation draws
unrestricted fitting coefficients and applies the fitted positivity
transformation; user draws must already be in transformed coordinates
and satisfy the coefficient constraints. MH sampling is unavailable for
SCAM fits, and smoothing-parameter uncertainty is unsupported; use
`unconditional = FALSE`.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Examples

``` r
load_mgcv()
# Simulate group-specific curves with Gaussian noise and fit factor-by smooths
df <- data_sim("eg4", n = 300, seed = 2)
m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")

# Compare fitted responses over x2, including the parametric group effects
cd <- conditional_differences(m, by = "fac", condition = "x2")
cd
#> # A tibble: 300 x 9
#>    .contrast .level_1 .level_2 .diff   .se .lower_ci .upper_ci      x2    x0
#>        <int> <fct>    <fct>    <dbl> <dbl>     <dbl>     <dbl>   <dbl> <dbl>
#>  1         1 1        2         3.13 0.868      1.42      4.83 0.00325 0.468
#>  2         1 1        2         3.17 0.830      1.54      4.80 0.0133  0.468
#>  3         1 1        2         3.22 0.794      1.66      4.77 0.0234  0.468
#>  4         1 1        2         3.26 0.760      1.77      4.75 0.0334  0.468
#>  5         1 1        2         3.31 0.729      1.88      4.74 0.0435  0.468
#>  6         1 1        2         3.35 0.700      1.98      4.73 0.0536  0.468
#>  7         1 1        2         3.40 0.675      2.07      4.72 0.0636  0.468
#>  8         1 1        2         3.44 0.652      2.16      4.72 0.0737  0.468
#>  9         1 1        2         3.48 0.631      2.24      4.72 0.0837  0.468
#> 10         1 1        2         3.52 0.613      2.32      4.72 0.0938  0.468
#> # i 290 more rows
draw(cd)
```
