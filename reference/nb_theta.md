# Negative binomial parameter theta

Negative binomial parameter theta

## Usage

``` r
nb_theta(model)

# S3 method for class 'gam'
nb_theta(model)
```

## Arguments

- model:

  a fitted model.

## Value

A numeric vector of length 1 containing the estimated value of theta.

## Methods (by class)

- `nb_theta(gam)`: Method for class `"gam"`

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 500, dist = "poisson", scale = 0.1, seed = 6)

m <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr") +
  s(x3, bs = "cr"), family = nb, data = df, method = "REML")
## IGNORE_RDIFF_BEGIN
nb_theta(m)
#> [1] 239386
## IGNORE_RDIFF_END
```
