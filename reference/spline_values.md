# Evaluate a spline at provided covariate values

Evaluate a spline at provided covariate values

## Usage

``` r
spline_values(
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

  a data frame of values to evaluate `smooth` at.

- model:

  a fitted model; currently only
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) models are
  supported.

- unconditional:

  logical; if `TRUE` (and only if `frequentist == FALSE`) then the
  bayesian smoothing parameter uncertainty-corrected covariance matrix
  is returned, if available. Whether it is available depends on which
  smoothness selection method was used to fit the model.

- overall_uncertainty:

  logical; should the uncertainty in the model constant term be included
  in the standard error of the evaluate values of the smooth?

- frequentist:

  logical; if `FALSE`, the default, the bayesian covariance matrix is
  returned, otherwise the frequentist covariance matrix.
