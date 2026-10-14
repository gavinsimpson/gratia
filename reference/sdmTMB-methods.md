# Formula effects for sdmTMB models

Evaluate and plot formula smooths and parametric effects from a fitted
`sdmTMB` model using the same plotting methods as mgcv smooths.
Penalized and unpenalized coefficients and their joint covariance are
transformed back to the constrained mgcv basis.

## Usage

``` r
# S3 method for class 'sdmTMB'
smooths(object, ...)

# S3 method for class 'sdmTMB'
get_smooths_by_id(object, id, ...)

# S3 method for class 'sdmTMB'
smooth_coefs(object, select, ...)

# S3 method for class 'sdmTMB'
smooth_estimates(
  object,
  ...,
  unconditional = FALSE,
  frequentist = FALSE,
  envir = NULL
)

# S3 method for class 'sdmTMB'
parametric_terms(model, ...)

# S3 method for class 'sdmTMB'
parametric_effects(
  object,
  terms = NULL,
  ...,
  unconditional = FALSE,
  frequentist = FALSE,
  envir = NULL
)

# S3 method for class 'sdmTMB'
assemble(
  object,
  ...,
  parametric = NULL,
  residuals = FALSE,
  unconditional = FALSE,
  frequentist = FALSE,
  envir = NULL
)

# S3 method for class 'sdmTMB'
draw(
  object,
  ...,
  parametric = NULL,
  residuals = FALSE,
  unconditional = FALSE,
  frequentist = FALSE,
  envir = NULL
)
```

## Arguments

- object, model:

  A fitted `sdmTMB` model.

- ...:

  Arguments passed to the corresponding gratia methods, including grid,
  interval, transformation, surface, styling and layout controls.

- id:

  Integer positions of smooths across model components.

- select:

  Smooth selection across both predictors of delta models. Numeric and
  logical indices follow stored smooth order, first component then
  second. Character selections use
  [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md)
  labels, such as `s(x)` and `s.1(x)` for the second component.

- unconditional, frequentist:

  Must be `FALSE`. Intervals use the joint TMB or RTMB covariance;
  mgcv-specific alternatives are unavailable.

- envir:

  Optional environment for evaluating model covariates.

- terms:

  Parametric terms to evaluate or draw, as returned by
  [`parametric_terms()`](https://gavinsimpson.github.io/gratia/reference/parametric_terms.md).
  Second-component selectors have a `.1` suffix.

- parametric:

  Include parametric effects? `NULL` includes them unless smooths are
  selected explicitly, as for
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md).

- residuals:

  Must be `FALSE`; partial residuals are not supported.

## Value

Smooth labels, smooth objects, coefficients, effect estimates, or plots,
as for the corresponding GAM methods.
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
returns a patchwork, or named panels with `wrap = FALSE`;
[`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
returns an `assembled_plots` list. Models without smooths can draw
parametric effects.

## Details

Estimates are partial contributions on each component's link scale.
Spatial, spatiotemporal, IID random effects, offsets, and varying
coefficients are not added to the effects. Their influence on
coefficient uncertainty is retained through the joint covariance. Smooth
intervals include uncertainty in their component's overall mean by
default; parametric intervals include the component's intercept. Use
`overall_uncertainty = FALSE` for term-only intervals. `fun` transforms
displayed partial effects, not full predictions.

Delta components have separate smooth and parametric coefficients and
are drawn separately, with component subtitles. Upstream sdmTMB
restrictions on supported bases and delta formulas still apply. Tensor
smooths with multiple penalties are supported. Fits require stored
smooth metadata; mapping or profiling required coefficients and
non-positive-definite Hessians give errors.

Support is for sdmTMB \>= 1.1.0, including development fits using RTMB.
Diagnostics use complete fitted predictors, including spatial,
spatiotemporal and independent group effects and offsets. Supported
single-response and standard delta families provide PIT/quantile
residuals, ordinary and scalable diagnostic plots, and joint approximate
posterior predictive checks. Delta diagnostics can select the complete
response, occurrence or positive component. Raw posterior response draws
are available through
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md).
By default, fields are drawn together once from their approximate
posterior and held fixed (`mle-mvn`), following sdmTMB's recommended
residual calculation. Checks can also hold estimates fixed, average over
fitted-field uncertainty, or simulate new fields. Marginal simulations
preserve dependence and supply residual ranks, including for nonlinear
and coupled delta responses. See
[diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md)
for treatment of coefficients, fields and covariance, and what each
check assesses.

Smooth extraction and effect plots do not support dispersion formulas,
other multi-predictor families, or threshold and nonlocal effects.
Random-effect plots are not provided.

## Examples

``` r
if (requireNamespace("sdmTMB", quietly = TRUE)) {
  library("mgcv")
  set.seed(42)
  d <- data.frame(x = runif(150), z = runif(150))
  d$y <- 1 + sin(6 * d$x) + 0.5 * d$z + rnorm(150, sd = 0.3)
  m <- sdmTMB::sdmTMB(y ~ z + s(x, k = 6), data = d, spatial = "off")
  draw(m)
  smooth_estimates(m, select = "s(x)")
  assemble(m)

  d$y <- ifelse(runif(150) < 0.3, 0, exp(d$y))
  delta <- sdmTMB::sdmTMB(y ~ z + s(x, k = 6),
    data = d,
    spatial = "off", family = sdmTMB::delta_gamma()
  )
  draw(delta)
  draw(delta, select = "s.1(x)", parametric = TRUE, terms = "z.1")
}
```
