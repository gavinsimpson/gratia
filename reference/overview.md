# Provides an overview of a model and the terms in that model

Provides an overview of a model and the terms in that model

## Usage

``` r
overview(model, ...)

# S3 method for class 'gam'
overview(
  model,
  parametric = TRUE,
  random_effects = TRUE,
  dispersion = NULL,
  frequentist = FALSE,
  accuracy = 0.001,
  digits = 3,
  stars = FALSE,
  ...
)
```

## Arguments

- model:

  a fitted model object to overview.

- ...:

  arguments passed to other methods.

- parametric:

  logical; include the model parametric terms in the overview?

- random_effects:

  tests of fully penalized smooth terms (those with a zero-dimensional
  null space, e.g. random effects) are computationally expensive and for
  large data sets producing these p values can take a very long time. If
  `random_effects = FALSE`, the tests of the expensive terms will be
  skipped.

- dispersion:

  numeric; a known value for the dispersion parameter. The default
  `NULL` implies that the estimated value or the default value (1 for
  the Poisson distribution for example) where this is specified is used
  instead.

- frequentist:

  logical; by default the Bayesian estimated covariance matrix of the
  parameter estimates is used to calculate p values for parametric
  terms. If `frequentist = FALSE`, the frequentist covariance matrix of
  the parameter estimates is used.

- accuracy:

  numeric; accuracy with which to report p values, with p values below
  this value displayed as `"< accuracy"`.

- digits:

  numeric; the number of significant digits to be used.

- stars:

  logical; should significance stars be added to the output?

## Examples

``` r
load_mgcv()
df <- data_sim(n = 400, seed = 2)
m <- gam(y ~ x3 + s(x0) + s(x1, bs = "bs") + s(x2, bs = "ts"),
  data = df, method = "REML"
)
overview(m)
#> 
#> Generalized Additive Model with 5 terms
#> 
#>   term      type              k   edf ref.edf statistic p.value
#>   <chr>     <chr>         <dbl> <dbl>   <dbl>     <dbl> <chr>  
#> 1 Intercept parametric       NA  1       1        41.2  <0.001 
#> 2 x3        parametric       NA  1       1        -2.07 0.0393 
#> 3 s(x0)     TPRS              9  3.02    3.76      6.25 <0.001 
#> 4 s(x1)     B spline          9  2.81    3.47     71.0  <0.001 
#> 5 s(x2)     TPRS (shrink)     9  7.91    9        83.8  <0.001 
```
