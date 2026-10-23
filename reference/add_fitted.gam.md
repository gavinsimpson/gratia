# Add fitted values from a GAM to a data frame

Add fitted values from a GAM to a data frame

## Usage

``` r
# S3 method for class 'gam'
add_fitted(
  data,
  model,
  value = ".fitted",
  type = "response",
  ...,
  envir = NULL,
  marginalise = NULL,
  integration = c("quadrature", "monte_carlo"),
  n_quad = 40L,
  n_integrate = 5000L,
  seed = NULL
)
```

## Arguments

- data:

  a data frame containing values for the variables used to fit the
  model. Passed to
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) as
  `newdata`.

- model:

  a fitted model for which a
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) method is
  available. S3 method dispatch is performed on the `model` argument.

- value:

  character; the name of the variable in which model predictions will be
  stored.

- type:

  character; the type of predictions to return. See
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
  for options.

- ...:

  additional arguments passed to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).

- envir:

  environment or `NULL`; functions and constants needed to evaluate
  model expressions in prediction data. With `NULL`, use the model's
  formula environment. Supply raw covariates in `data`.

- marginalise:

  `NULL` retains the usual predictions. `TRUE` selects all supported
  `re` and `fs` terms; character vectors select exact smooth labels.
  Fitted predictions integrate response means over new zero-mean
  Gaussian effects. Response samplers draw new effects before sampling
  responses. Smoothing parameters and dispersion remain fixed. Only
  ordinary `gam()` and `bam()` fits with a scalar response mean are
  supported.

- integration:

  Integration method when `marginalise` is supplied: `"quadrature"` (the
  default) or `"monte_carlo"`. Both integrate over the Gaussian
  distribution of the selected effects at each row.

- n_quad:

  Initial Gauss-Hermite point count, between 2 and 1000. Successive
  doubled orders, up to four times this count, and an interlaced rule
  with one fewer point must agree within `1e-8 + 1e-6 * abs(value)` for
  means and derivatives. Where the random-effect variance exceeds one,
  an independent adaptive rule also checks agreement. Failure to agree
  reports an error. Identity, log and probit links use their closed-form
  Gaussian integrals; their metadata records zero quadrature points and
  zero integration error.

- n_integrate:

  Number of Monte Carlo integration draws, at least two. These are
  shared across rows and posterior coefficient draws. This count is
  separate from `n_draws`. Ignored for quadrature.

- seed:

  Optional integer seed. An explicit seed preserves the caller's
  random-number state; `NULL` advances the current state.

## Value

A data frame (tibble) formed from `data` and predictions from `model`.

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", seed = 1)
df <- df[, c("y", "x0", "x1", "x2", "x3")]
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

# add fitted values to our data
add_fitted(df, m)
#> # A tibble: 400 x 6
#>          y     x0     x1     x2    x3 .fitted
#>      <dbl>  <dbl>  <dbl>  <dbl> <dbl>   <dbl>
#>  1  3.34   0.266  0.659  0.859  0.367    5.90
#>  2 -0.0758 0.372  0.185  0.0344 0.741    3.15
#>  3 10.7    0.573  0.954  0.971  0.934    8.28
#>  4  8.73   0.908  0.898  0.745  0.673    8.65
#>  5 15.0    0.202  0.944  0.273  0.701   15.7 
#>  6  7.67   0.898  0.724  0.677  0.848    8.38
#>  7  7.58   0.945  0.370  0.348  0.706    7.84
#>  8  8.51   0.661  0.781  0.947  0.859    6.74
#>  9 10.6    0.629  0.0111 0.339  0.446    9.14
#> 10  3.72   0.0618 0.940  0.0317 0.677    7.04
#> # i 390 more rows

# with type = "terms" or "iterms"
add_fitted(df, m, type = "terms")
#> # A tibble: 400 x 10
#>          y     x0     x1     x2    x3 .constant `s(x0)` `s(x1)` `s(x2)` `s(x3)`
#>      <dbl>  <dbl>  <dbl>  <dbl> <dbl>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1  3.34   0.266  0.659  0.859  0.367      7.94  0.175    0.559  -2.81   0.0351
#>  2 -0.0758 0.372  0.185  0.0344 0.741      7.94  0.435   -1.92   -3.23  -0.0687
#>  3 10.7    0.573  0.954  0.971  0.934      7.94  0.593    3.35   -3.47  -0.122 
#>  4  8.73   0.908  0.898  0.745  0.673      7.94 -0.812    2.77   -1.19  -0.0498
#>  5 15.0    0.202  0.944  0.273  0.701      7.94 -0.0589   3.23    4.63  -0.0576
#>  6  7.67   0.898  0.724  0.677  0.848      7.94 -0.745    1.15    0.146 -0.0981
#>  7  7.58   0.945  0.370  0.348  0.706      7.94 -1.07    -1.31    2.34  -0.0589
#>  8  8.51   0.661  0.781  0.947  0.859      7.94  0.434    1.67   -3.20  -0.101 
#>  9 10.6    0.629  0.0111 0.339  0.446      7.94  0.512   -1.95    2.63   0.0132
#> 10  3.72   0.0618 0.940  0.0317 0.677      7.94 -0.695    3.20   -3.35  -0.0508
#> # i 390 more rows
```
