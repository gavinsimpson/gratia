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
