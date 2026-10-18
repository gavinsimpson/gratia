# Estimated values for parametric model terms

Estimated values for parametric model terms

## Usage

``` r
# S3 method for class 'gamm4'
parametric_effects(object, ...)

parametric_effects(object, ...)

# S3 method for class 'gam'
parametric_effects(
  object,
  terms = NULL,
  data = NULL,
  unconditional = FALSE,
  unnest = TRUE,
  ci_level = 0.95,
  envir = NULL,
  transform = lifecycle::deprecated(),
  n = 100,
  n_2d = 50,
  n_3d = 16,
  n_4d = 4,
  dist = 0.1,
  overall_uncertainty = TRUE,
  back_transform = FALSE,
  ...
)

# S3 method for class 'scam'
parametric_effects(object, ..., unconditional = FALSE)
```

## Arguments

- object:

  a fitted model object.

- ...:

  arguments passed to other methods.

- terms:

  character; which model parametric terms should be drawn? The Default
  of `NULL` will plot all parametric terms that can be drawn.

- data:

  an optional data frame containing raw model covariates at which to
  evaluate parametric effects. By default, stored raw columns are used;
  missing raw inputs are recovered from the fitting data when possible.

- unconditional:

  logical; should confidence intervals include the uncertainty due to
  smoothness selection? If `TRUE`, the corrected Bayesian covariance
  matrix will be used.

- unnest:

  logical; unnest the parametric effect objects?

- ci_level:

  numeric; the coverage required for the confidence interval. Currently
  ignored.

- envir:

  an optional environment for local functions and constants, and for
  recovering fitting data when required raw columns are not stored in
  the model. Defaults to the model's evaluation environment.

- transform:

  **\[deprecated\]** Use `back_transform = !transform` instead.

- n, n_2d, n_3d, n_4d:

  Grid resolutions for multivariate parametric terms. Curves use
  `n = 100`; the first two numeric surface axes use `n_2d = 50`. The
  third dimension of a three-variable term uses `n_3d = 16`; dimensions
  beyond the second of higher-dimensional terms use `n_4d = 4`. `NULL`
  uses `n` instead. Factors retain their levels. Ignored when `data` is
  supplied. Single-variable terms retain their observed evaluation
  values.

- dist:

  Non-negative distance for masking surface plots far from observed
  covariates, as in
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md).
  Applied to the first two numeric axes when drawing; returned estimates
  are not masked. Use zero to disable masking.

- overall_uncertainty:

  logical; include uncertainty in the intercept of the term's linear
  predictor, including its covariance with the term? Defaults to `TRUE`.
  Partial effect estimates are unchanged. With no intercept, this has no
  effect. Set to `FALSE` for term-only uncertainty.

- back_transform:

  logical; plot covariates on their original scale? Defaults to `FALSE`,
  using model coordinates such as `log(x)` for a term containing
  `log(x)`. With `TRUE`, recover the raw covariates, generate a grid on
  their original scale and evaluate the model expressions forwards.
  Applies to axes, rugs, partial-residual locations and conditioning
  facets, including tensor products and parametric interactions. Each
  smooth axis must depend on one distinct original model covariate.
  Functions such as `foo(x)` are supported when their raw inputs and
  functions can be recovered; no inverse function is needed. Missing raw
  data or unavailable functions produce an error; supply `data` or
  `envir` as appropriate. With supplied prediction `data`, unavailable
  raw fitting observations omit rugs and partial residuals; prediction
  rows are never used as observations. For precomputed smooth estimates,
  prepare raw grids with
  `smooth_estimates(model, back_transform = TRUE)` before drawing with
  `back_transform = TRUE`. Raw inputs supplied through `data` also
  retain their coordinates. Transformed-only estimates cannot be
  relabelled on the raw scale, including grids carrying typical values
  from `include_all = TRUE`. Transformed soap-film boundaries lack
  retained raw coordinates and must be drawn with
  `back_transform = FALSE`. Parametric estimates retain both scales;
  multi-column basis expansions retain raw axes on either setting. Does
  not transform estimated effects or their intervals; use `fun` for
  that.

## Value

A tibble of class `parametric_effects`, with `.term`, `.type`,
`.partial` and `.se`. Single-variable terms retain `.value` or `.level`;
multivariate terms use columns named for their raw covariates,
containing coordinates on the selected scale. Names conflicting with
reserved columns are repaired with numeric suffixes. The `term_info`
attribute records multivariate column mappings, plotting order, factor
levels and observation data. With `unnest = FALSE`, estimates and
covariates are nested in a `data` list column. The `axis_info` attribute
identifies retained raw and model coordinate columns and their labels so
the drawing method can select either scale. The `back_transform`
attribute records the scale of the returned coordinate columns.
Auxiliary `.gratia_raw_*` and `.gratia_model_*` columns retain both
scales for transformed axes.

## Details

Each estimate is the contribution of one formula term to its linear
predictor. Intercepts, main effects and other terms are not added to an
interaction. Components follow the model's contrast coding: with
treatment contrasts, a numeric-by-factor interaction represents a
departure from the reference-level slope, rather than a complete slope
for each level.

A multi-column term such as `poly(x, 3)` is one component, whereas `x`
and `I(x^2)` remain separate components. Scalar numeric transformations
use model coordinates by default, including each axis of an interaction.
Basis expansions such as `poly(x, 3)`, terms combining several inputs in
one expression, and interactions containing several transformations of
the same input retain raw axes because they have no single model
coordinate per input. Other covariates in generated prediction data are
held at typical values solely to evaluate the model matrix; their
contributions are not included. Rugs show fitting observations,
including when prediction `data` are supplied. If fitting observations
cannot be recovered, these rugs are omitted.

The drawing method uses the first two numeric covariates for surface
axes, grouping curves by the first factor when there is only one numeric
covariate. Factor-only terms use grouped points and intervals. Remaining
covariates define facets: one wraps, two or more use the first as rows
and the rest as columns. Formula order is retained within numeric and
discrete covariates. Logical covariates are discrete. Surface plots show
estimates; uncertainty is retained in the returned data rather than
drawn as additional surfaces.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Examples

``` r
load_mgcv()

# Simulate numeric and factor effects
set.seed(42)
d <- data.frame(
  x = runif(100),
  group = factor(rep(c("A", "B"), each = 50))
)
d$y <- with(d, 2 * x + (group == "B") + rnorm(100, sd = 0.25))
m <- gam(y ~ x + group, data = d, method = "REML")

# Estimate and draw the contribution of each parametric term
pe <- parametric_effects(m)
pe
#> # A tibble: 102 x 6
#>    .term .type   .value .level .partial    .se
#>    <chr> <chr>    <dbl> <chr>     <dbl>  <dbl>
#>  1 x     numeric  0.915 NA        1.91  0.0412
#>  2 x     numeric  0.937 NA        1.96  0.0423
#>  3 x     numeric  0.286 NA        0.598 0.0411
#>  4 x     numeric  0.830 NA        1.73  0.0375
#>  5 x     numeric  0.642 NA        1.34  0.0329
#>  6 x     numeric  0.519 NA        1.08  0.0333
#>  7 x     numeric  0.737 NA        1.54  0.0345
#>  8 x     numeric  0.135 NA        0.281 0.0493
#>  9 x     numeric  0.657 NA        1.37  0.0330
#> 10 x     numeric  0.705 NA        1.47  0.0338
#> # i 92 more rows
draw(pe)

```
