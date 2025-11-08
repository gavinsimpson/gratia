# List the variables involved in a model fitted with a formula

List the variables involved in a model fitted with a formula

## Usage

``` r
model_vars(model, ...)

# S3 method for class 'gam'
model_vars(model, ...)

# Default S3 method
model_vars(model, ...)

# S3 method for class 'bam'
model_vars(model, ...)

# S3 method for class 'gamm'
model_vars(model, ...)

# S3 method for class 'gamm4'
model_vars(model, ...)

# S3 method for class 'list'
model_vars(model, ...)
```

## Arguments

- model:

  a fitted model object with a `$pred.formula`, `$terms` component or a
  `"terms"` attribute

- ...:

  Arguments passed to other methods. Currently ignored.

## Examples

``` r
load_mgcv()

# simulate some Gaussian data
df <- data_sim("eg1", n = 50, seed = 2)

# fit a GAM with 1 smooth and 1 linear term
m1 <- gam(y ~ s(x2, k = 7) + x1, data = df, method = "REML")
model_vars(m1)
#> [1] "x1" "x2"

# fit a lm with two linear terms
m2 <- lm(y ~ x2 + x1, data = df)
model_vars(m2)
#> [1] "x2" "x1"
```
