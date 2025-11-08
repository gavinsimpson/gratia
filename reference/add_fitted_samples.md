# Add posterior draws from a model to a data object

Adds draws from the posterior distribution of `model` to the data
`object` using one of
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
[`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md),
or
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md).

## Usage

``` r
add_fitted_samples(object, model, n = 1, seed = NULL, ...)

add_predicted_samples(object, model, n = 1, seed = NULL, ...)

add_posterior_samples(object, model, n = 1, seed = NULL, ...)

add_smooth_samples(object, model, n = 1, seed = NULL, select = NULL, ...)
```

## Arguments

- object:

  a data frame or tibble to which the posterior draws will be added.

- model:

  a fitted GAM (or GAM-like) object for which a posterior draw method
  exists.

- n:

  integer; the number of posterior draws to add.

- seed:

  numeric; a value to seed the random number generator.

- ...:

  arguments are passed to the posterior draw function, currently one of
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md),
  or
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md).
  `n` and `seed` are already specified here as arguments and are also
  passed on to the posterior sampling function.

- select:

  character; select which smooth's posterior to draw from. The default,
  `NULL`, means the posteriors of all smooths in model will be sampled
  from individually. If supplied, a character vector of requested smooth
  terms.

## Examples

``` r
load_mgcv()

df <- data_sim("eg1", n = 400, seed = 42)

m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

# add fitted samples (posterior draws of the expected value of the response)
# note that there are 800 rows in the output: 400 data by `n = 2` samples.
df |>
  add_fitted_samples(m, n = 2, seed = 84)
#> # A tibble: 800 × 14
#>        y    x0     x1    x2    x3     f    f0    f1     f2    f3  .row .draw
#>    <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <int> <int>
#>  1  2.99 0.915 0.0227 0.909 0.402  1.62 0.529  1.05 0.0397     0     1     1
#>  2  2.99 0.915 0.0227 0.909 0.402  1.62 0.529  1.05 0.0397     0     1     2
#>  3  4.70 0.937 0.513  0.900 0.432  3.25 0.393  2.79 0.0630     0     2     1
#>  4  4.70 0.937 0.513  0.900 0.432  3.25 0.393  2.79 0.0630     0     2     2
#>  5 13.9  0.286 0.631  0.192 0.664 13.5  1.57   3.53 8.41       0     3     1
#>  6 13.9  0.286 0.631  0.192 0.664 13.5  1.57   3.53 8.41       0     3     2
#>  7  5.71 0.830 0.419  0.532 0.182  6.12 1.02   2.31 2.79       0     4     1
#>  8  5.71 0.830 0.419  0.532 0.182  6.12 1.02   2.31 2.79       0     4     2
#>  9  7.63 0.642 0.879  0.522 0.838 10.4  1.80   5.80 2.76       0     5     1
#> 10  7.63 0.642 0.879  0.522 0.838 10.4  1.80   5.80 2.76       0     5     2
#> # ℹ 790 more rows
#> # ℹ 2 more variables: .parameter <chr>, .fitted <dbl>

# add posterior draws from smooth s(x2)
df |>
  add_smooth_samples(m, n = 2, seed = 2, select= "s(x2)")
#> # A tibble: 800 × 15
#>        y    x0     x1    x2    x3     f    f0    f1     f2    f3  .row .smooth
#>    <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <int> <chr>  
#>  1  2.99 0.915 0.0227 0.909 0.402  1.62 0.529  1.05 0.0397     0     1 s(x2)  
#>  2  2.99 0.915 0.0227 0.909 0.402  1.62 0.529  1.05 0.0397     0     1 s(x2)  
#>  3  4.70 0.937 0.513  0.900 0.432  3.25 0.393  2.79 0.0630     0     2 s(x2)  
#>  4  4.70 0.937 0.513  0.900 0.432  3.25 0.393  2.79 0.0630     0     2 s(x2)  
#>  5 13.9  0.286 0.631  0.192 0.664 13.5  1.57   3.53 8.41       0     3 s(x2)  
#>  6 13.9  0.286 0.631  0.192 0.664 13.5  1.57   3.53 8.41       0     3 s(x2)  
#>  7  5.71 0.830 0.419  0.532 0.182  6.12 1.02   2.31 2.79       0     4 s(x2)  
#>  8  5.71 0.830 0.419  0.532 0.182  6.12 1.02   2.31 2.79       0     4 s(x2)  
#>  9  7.63 0.642 0.879  0.522 0.838 10.4  1.80   5.80 2.76       0     5 s(x2)  
#> 10  7.63 0.642 0.879  0.522 0.838 10.4  1.80   5.80 2.76       0     5 s(x2)  
#> # ℹ 790 more rows
#> # ℹ 3 more variables: .term <chr>, .draw <int>, .value <dbl>
```
