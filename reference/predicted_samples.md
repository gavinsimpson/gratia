# Draw new response values from the conditional distribution of the response

Predicted values of the response (new response data) are drawn from the
fitted model, created via
[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) (e.g.
[`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md))
and returned in a tidy, long, format. These predicted values do not
include the uncertainty in the estimated model; they are simply draws
from the conditional distribution of the response.

## Usage

``` r
# S3 method for class 'gamm4'
predicted_samples(model, ...)

predicted_samples(model, ...)

# Default S3 method
predicted_samples(model, ...)

# S3 method for class 'gam'
predicted_samples(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  seed = NULL,
  weights = NULL,
  marginalise = NULL
)

# S3 method for class 'scam'
predicted_samples(
  model,
  ...,
  data = NULL,
  n_draws = NULL,
  seed = NULL,
  weights = NULL
)
```

## Arguments

- model:

  a fitted model of the supported types; see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

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
  [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) method.

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `data`, if available in `model`.

- n_draws:

  positive integer; number of response replicates to generate at the
  fitted parameters. Required; coefficient chains are not used.

- seed:

  numeric; a random seed for the simulations.

- weights:

  numeric; response sampling weights for GAM methods. An explicit vector
  overrides the fitting weights; a single value applies to every
  observation. With `data = NULL`, the default is the fitted prior
  weights, except factor-valued binomial responses retain one Bernoulli
  trial per observation. Native
  [`mgcv::gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) ignores
  fitting weights in its likelihood, so its default response weights are
  also ones; explicitly supplied sampling weights still override this
  default. With prediction data, weights default to a vector of ones.
  For ordinary Gaussian and Gamma responses, larger weights reduce
  response variance (precision weights). For binomial responses, weights
  give integer trial counts. They do not alter the fitted coefficient
  covariance. Include positions restored by
  [`stats::na.exclude()`](https://rdrr.io/r/stats/na.fail.html) when
  supplying weights for fitting rows.

- marginalise:

  `NULL` retains the usual predictions. `TRUE` selects all supported
  `re` and `fs` terms; character vectors select exact smooth labels.
  Fitted predictions integrate response means over new zero-mean
  Gaussian effects. Response samplers draw new effects before sampling
  responses. Smoothing parameters and dispersion remain fixed. Only
  ordinary [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) fits with a
  scalar response mean are supported.

## Value

A tibble (data frame) with 3 columns containing the posterior predicted
values in long format. The columns are

- `.row` (integer) the row of `data` that each posterior draw relates
  to,

- `.draw` (integer) an index, in range `1:n_draws`, indicating which
  draw each row relates to,

- `.response` (numeric) the predicted response for the indicated row of
  `data`.

## Details

LSS response simulation uses all fitted distribution parameters at the
requested data, with the same parameter transformations and offset rules
as
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md).
It holds coefficients fixed, so it has no coefficient chains or attached
coefficient diagnostics.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 120, seed = 2)
m <- gam(y ~ s(x0, k = 6) + s(x2, k = 6), data = dat, method = "REML")

# Simulate responses at new predictor values using the fitted model
newd <- data.frame(x0 = c(0.2, 0.5, 0.8), x2 = 0.5)
predicted_samples(m, n_draws = 5, data = newd, seed = 42)
#> # A tibble: 15 x 3
#>     .row .draw .response
#>    <int> <int>     <dbl>
#>  1     1     1     11.7 
#>  2     2     1      7.89
#>  3     3     1      9.49
#>  4     1     2      9.66
#>  5     2     2     10.6 
#>  6     3     2      8.19
#>  7     1     3     12.1 
#>  8     2     3      9.19
#>  9     3     3     14.1 
#> 10     1     4      7.74
#> 11     2     4     13.1 
#> 12     3     4     14.8 
#> 13     1     5      4.08
#> 14     2     5      8.68
#> 15     3     5      8.12
```
