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
  envir = environment(formula(object)),
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

  a optional data frame that may or may not be used? FIXME!

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

  an environment to look up the data within.

- transform:

  logical; if `TRUE`, the parametric effect will be plotted on its
  transformed scale which will result in the effect being a straight
  line. If FALSE, the effect will be plotted against the raw data (i.e.
  for `log10(x)`, or `poly(z)`, the x-axis of the plot will be `x` or
  `z` respectively.)
