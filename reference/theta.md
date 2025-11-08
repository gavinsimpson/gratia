# General extractor for additional parameters in mgcv models

General extractor for additional parameters in mgcv models

## Usage

``` r
theta(object, ...)

# S3 method for class 'gam'
theta(object, transform = TRUE, ...)

# S3 method for class 'family'
theta(object, transform = TRUE, ...)
```

## Arguments

- object:

  a fitted model

- ...:

  arguments passed to other methods.

- transform:

  logical; transform to the natural scale of the parameter

## Value

Returns a numeric vector of additional parameters

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", dist = "poisson", seed = 42, scale = 1 / 5)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df, method = "REML",
  family = nb()
)
p <- theta(m)
```
