# Are additional parameters available for a GAM?

Are additional parameters available for a GAM?

## Usage

``` r
has_theta(object)
```

## Arguments

- object:

  an R object, either a
  [`family()`](https://rdrr.io/r/stats/family.html) object or an object
  whose class has a [`family()`](https://rdrr.io/r/stats/family.html)
  method.

## Value

A logical; `TRUE` if additional parameters available, `FALSE` otherwise.

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", dist = "poisson", seed = 42, scale = 1 / 5)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df, method = "REML",
  family = nb()
)
has_theta(m)
#> [1] TRUE
p <- theta(m)
```
