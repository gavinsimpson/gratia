# Draw new response values from the conditional distribution of the response

Predicted values of the response (new response data) are drawn from the
fitted model, created via
[`simulate()`](https://rdrr.io/r/stats/simulate.html) (e.g.
[`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md))
and returned in a tidy, long, format. These predicted values do not
include the uncertainty in the estimated model; they are simply draws
from the conditional distribution of the response.

## Usage

``` r
predicted_samples(model, ...)

# Default S3 method
predicted_samples(model, ...)

# S3 method for class 'gam'
predicted_samples(
  model,
  n = 1,
  data = newdata,
  seed = NULL,
  weights = NULL,
  ...,
  newdata = NULL
)

# S3 method for class 'scam'
predicted_samples(model, n = 1, data = NULL, seed = NULL, weights = NULL, ...)
```

## Arguments

- model:

  a fitted model of the supported types

- ...:

  arguments passed to other methods. For
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  these are passed on to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).
  For
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  these are passed on to
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md).
  For `predicted_samples()` these are passed on to the relevant
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) method.

- n:

  numeric; the number of posterior samples to return.

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `data`, if available in `model`.

- seed:

  numeric; a random seed for the simulations.

- weights:

  numeric; a vector of prior weights. If `data` is null then defaults to
  `object[["prior.weights"]]`, otherwise a vector of ones.

- newdata:

  Deprecated: use `data` instead.

## Value

A tibble (data frame) with 3 columns containing the posterior predicted
values in long format. The columns are

- `row` (integer) the row of `data` that each posterior draw relates to,

- `draw` (integer) an index, in range `1:n`, indicating which draw each
  row relates to,

- `response` (numeric) the predicted response for the indicated row of
  `data`.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

predicted_samples(m, n = 5, seed = 42)
#> # A tibble: 5,000 x 3
#>     .row .draw .response
#>    <int> <int>     <dbl>
#>  1     1     1      8.93
#>  2     2     1      4.23
#>  3     3     1      7.71
#>  4     4     1      8.51
#>  5     5     1     10.1 
#>  6     6     1      8.20
#>  7     7     1      8.95
#>  8     8     1      7.20
#>  9     9     1     18.1 
#> 10    10     1     12.7 
#> # i 4,990 more rows

## Can pass arguments to predict.gam()
newd <- data.frame(
  x0 = runif(10), x1 = runif(10), x2 = runif(10),
  x3 = runif(10)
)

## Exclude s(x2)
predicted_samples(m, n = 5, newd, exclude = "s(x2)", seed = 25)
#> # A tibble: 50 x 3
#>     .row .draw .response
#>    <int> <int>     <dbl>
#>  1     1     1      9.42
#>  2     2     1      6.97
#>  3     3     1      8.10
#>  4     4     1      9.95
#>  5     5     1      6.75
#>  6     6     1     10.3 
#>  7     7     1     10.8 
#>  8     8     1     10.5 
#>  9     9     1      8.43
#> 10    10     1     12.2 
#> # i 40 more rows

## Exclude s(x1)
predicted_samples(m, n = 5, newd, exclude = "s(x1)", seed = 25)
#> # A tibble: 50 x 3
#>     .row .draw .response
#>    <int> <int>     <dbl>
#>  1     1     1      6.05
#>  2     2     1      5.28
#>  3     3     1      5.96
#>  4     4     1     13.7 
#>  5     5     1      4.36
#>  6     6     1      5.11
#>  7     7     1     12.5 
#>  8     8     1      5.66
#>  9     9     1     12.6 
#> 10    10     1      8.38
#> # i 40 more rows

## Select which terms --- result should be the same as previous
## but note that we have to include any parametric terms, including the
## constant term
predicted_samples(m,
  n = 5, newd, seed = 25,
  terms = c("Intercept", "s(x0)", "s(x2)", "s(x3)")
)
#> # A tibble: 50 x 3
#>     .row .draw .response
#>    <int> <int>     <dbl>
#>  1     1     1    -1.94 
#>  2     2     1    -2.71 
#>  3     3     1    -2.03 
#>  4     4     1     5.73 
#>  5     5     1    -3.63 
#>  6     6     1    -2.87 
#>  7     7     1     4.48 
#>  8     8     1    -2.33 
#>  9     9     1     4.65 
#> 10    10     1     0.395
#> # i 40 more rows
```
