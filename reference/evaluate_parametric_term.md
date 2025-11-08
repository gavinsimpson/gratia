# Evaluate parametric model terms

**\[deprecated\]** Returns values of parametric model terms at values of
factor terms and over a grid of covariate values for linear parametric
terms. This function is now deprecated in favour of
[`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md).

## Usage

``` r
evaluate_parametric_term(object, ...)

# S3 method for class 'gam'
evaluate_parametric_term(object, term, unconditional = FALSE, ...)
```

## Arguments

- object:

  an object of class `"gam"` or `"gamm"`.

- ...:

  arguments passed to other methods.

- term:

  character; which parametric term whose effects are evaluated

- unconditional:

  logical; should confidence intervals include the uncertainty due to
  smoothness selection? If `TRUE`, the corrected Bayesian covariance
  matrix will be used.
