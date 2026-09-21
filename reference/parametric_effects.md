# Estimated values for parametric model terms

Estimated values for parametric model terms

## Usage

``` r
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
  transform = FALSE,
  ...
)
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

  logical; if `TRUE`, the parametric effect will be plotted on its
  transformed scale which will result in the effect being a straight
  line. If FALSE, the effect will be plotted against the raw data (i.e.
  for `log10(x)`, or `poly(z)`, the x-axis of the plot will be `x` or
  `z` respectively.)
