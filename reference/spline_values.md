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

  logical; should confidence intervals include the uncertainty due to
  smoothness selection? If `TRUE`, the corrected Bayesian covariance
  matrix will be used.

- overall_uncertainty:

  logical; should the uncertainty in the model constant term be included
  in the standard error of the evaluate values of the smooth?

- frequentist:

  logical; use the frequentist covariance matrix?
