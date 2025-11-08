# Randomised residuals

Randomised residuals

## Usage

``` r
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# S3 method for class 'gam'
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# S3 method for class 'glm'
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)
```

## Arguments

- model:

  a fitted model object.

- type:

  character; which type of randomised residual to return

- seed:

  integer; the random seed to use when generating randomised residuals.
  Can be missing, in which case the current state residuals are computed
  using the current state of the random number generator.

- ...:

  arguments passed to other methods.
