# Add a confidence interval to an existing object

Add a confidence interval to an existing object

## Usage

``` r
add_confint(object, coverage = 0.95, ...)

# S3 method for class 'smooth_estimates'
add_confint(object, coverage = 0.95, ...)

# S3 method for class 'parametric_effects'
add_confint(object, coverage = 0.95, ...)

# Default S3 method
add_confint(object, coverage = 0.95, ...)
```

## Arguments

- object:

  a R object.

- coverage:

  numeric; the coverage for the interval. Must be in the range 0 \<
  `coverage` \< 1.

- ...:

  arguments passed to other methods.

## Details

For smooth estimates, this function computes Gaussian pointwise
intervals from the saved estimates and standard errors. It replaces any
existing limits and updates their interval metadata. To retain stored
bootstrap, shape-enforced or simultaneous limits, draw the estimates
directly with
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
without calling `add_confint()`.
