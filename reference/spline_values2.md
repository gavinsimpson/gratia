# Evaluate a spline at provided covariate values

**\[deprecated\]**

The function `spline_values2()` has been renamed to
[`spline_values()`](https://gavinsimpson.github.io/gratia/reference/spline_values.md)
as of version 0.9.0. This was allowed following the removal of
[`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md),
which was the only function using
[`spline_values()`](https://gavinsimpson.github.io/gratia/reference/spline_values.md).
So `spline_values2()` has been renamed to
[`spline_values()`](https://gavinsimpson.github.io/gratia/reference/spline_values.md).

## Usage

``` r
spline_values2(
  smooth,
  data,
  model,
  unconditional,
  overall_uncertainty = TRUE,
  frequentist = FALSE
)
```

## Arguments

- smooth:

  currently an object that inherits from class `mgcv.smooth`.

- data:

  an optional data frame of values to evaluate `smooth` at.

- model:

  a fitted model; currently only
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) models are
  supported.

- unconditional:

  logical; should confidence intervals include the uncertainty due to
  smoothness selection? If `TRUE`, the corrected Bayesian covariance
  matrix will be used.

- overall_uncertainty:

  logical; should the uncertainty in the model constant term be included
  in the standard error of the evaluate values of the smooth?

- frequentist:

  logical; use the frequentist covariance matrix?
