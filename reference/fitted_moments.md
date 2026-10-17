# Estimate moments of the conditional response distribution

Calculate population moments at observed or supplied covariate values,
with uncertainty propagated jointly through the model's distribution
parameters.

## Usage

``` r
fitted_moments(object, ...)

# S3 method for class 'gam'
fitted_moments(
  object,
  data = NULL,
  moments = c("mean", "variance", "cv"),
  uncertainty = c("delta", "simulation", "none"),
  ci_level = 0.95,
  unconditional = FALSE,
  nuisance = c("estimated", "fixed"),
  n_draws = 10000L,
  seed = NULL,
  scores = NULL,
  method = c("gaussian", "mh", "inla", "user"),
  draws = NULL,
  envir = NULL,
  family_index = NULL,
  ...
)
```

## Arguments

- object:

  A fitted [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) model.

- ...:

  Prediction arguments passed to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html),
  including `exclude` and `na.action`. `type`, `newdata`, `se.fit`, and
  `terms` are controlled internally and cannot be supplied.

- data:

  Optional prediction data. With `NULL`, evaluate fitting rows,
  retaining fitting offsets and the model's missing-value convention.

- moments:

  Character vector selecting `"mean"`, `"variance"`, `"sd"`, `"cv"`,
  `"skewness"`, `"kurtosis"`, or `"excess_kurtosis"`. `"all"` requests
  all seven. CV is SD divided by a strictly positive mean; kurtosis is
  the fourth central moment divided by squared variance.

- uncertainty:

  One of `"delta"`, `"simulation"`, or `"none"`.

- ci_level:

  Coverage of pointwise uncertainty intervals, between 0 and 1.

- unconditional:

  Include smoothing parameter uncertainty when available.

- nuisance:

  One of `"estimated"` or `"fixed"`. The default propagates uncertainty
  in estimated family parameters outside the coefficient vector, such as
  Gamma dispersion, negative-binomial shape, or Tweedie power. `"fixed"`
  explicitly conditions on their fitted values.

- n_draws:

  Number of joint parameter draws for simulation, at least two.
  User-supplied coefficient draws determine their own count when
  omitted.

- seed:

  Optional integer random seed. An explicit seed preserves the caller's
  random-number state; `NULL` uses and advances it when generating
  random draws. Supplied draws do not consume random numbers.

- scores:

  Optional numeric category scores, in category order, for ordinal or
  multinomial models. Otherwise return moments of each category's
  zero/one membership indicator. Binomial moments describe a Bernoulli
  response, independently of fitting trial counts or likelihood weights.

- method:

  Coefficient sampler for simulation; see
  [`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md).
  Joint sampling of estimated global family parameters uses
  `"gaussian"`.

- draws:

  Optional coefficient matrix for `method = "user"`, in fitted
  coefficient order. Requires `nuisance = "fixed"` if global parameters
  were estimated outside the coefficient vector.

- envir:

  Environment used to recover model covariates.

- family_index:

  Optional integer component-family positions for grouped predictions,
  one per supplied row. Membership is otherwise recovered as in
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md).
  It must agree with membership recoverable from `data`.

## Value

A tibble of class `"fitted_moments"`, containing `.row`, covariates,
`.parameter`, `.fitted`, `.se`, `.lower_ci`, `.upper_ci` and `.status`.
Category indicators include `.category`; multivariate responses include
`.y`; grouped responses include `.family`. Simulation also includes
`.p_defined`. All estimates, standard errors and interval bounds are on
the requested quantity's scale.

## Details

Estimates describe one response at each covariate combination, with unit
observation weight. Fitting weights are not transferred to new
responses. For ML/REML fits with an estimated dispersion, all
uncertainty modes use the likelihood estimate stored in `reml.scale`.
The conditional coefficient covariance is rescaled to this value. This
can differ from mgcv's separate Pearson/Fletcher estimate in `sig2`; the
saved outer Hessian describes the likelihood estimate, so combining it
with `sig2` would mix estimators. Response variance and uncertainty in
its estimate are different quantities: `.fitted` contains the variance
when `.parameter` is `"variance"`, and `.se` describes uncertainty in
that estimate. These are not moments of a posterior predictive mixture
or prediction intervals for future observations.

Standard Gaussian, Poisson, binomial, Gamma and inverse-Gaussian
families, negative binomial, beta, scaled t, Tweedie, all seven
location/scale/shape families, ordinal, multinomial and
multivariate-normal models are supported. Censored normal, logistic and
Poisson families return moments of the latent uncensored response. Beta
moments describe the beta likelihood, without the epsilon clipping used
by some response generators.
[`bcg()`](https://rdrr.io/pkg/mgcv/man/bcg.html) supports the original
lognormal response only when its Box-Cox parameter is fixed at zero:
other powers require a distribution outside the inverse transformation's
domain that the fitted family does not specify. Quasi and Cox families
are unsupported. Grouped (`gfam`) predictions use each row's component
family and the full joint model covariance. Grouped ordinal components
require explicit category scores; grouped category-indicator output is
unsupported.

The delta method differentiates the complete parameter-to-moment
transformation and uses its full joint covariance. Gaussian, Poisson,
Gamma, inverse-Gaussian and Tweedie derivatives (including their models
with several predictors) are analytic; other families use central
numerical differences. Positive variance, SD, CV and kurtosis intervals
are calculated on a log scale; `.se` always remains on the reported
quantity's scale. Other delta intervals use the identity scale.
Simulation uses joint parameter draws and equal-tailed quantiles. Both
methods retain the moment evaluated at fitted parameters as `.fitted`.

With estimated global family parameters, a first-order joint Gaussian
approximation combines the conditional Bayesian coefficient covariance
with second derivatives of the ML/REML fitting criterion and derivatives
of the fitted coefficients with respect to the family and smoothing
parameters. Without `unconditional`, outer curvature is conditioned on
smoothing parameters. With `unconditional`, the full saved outer Hessian
is inverted, including its smoothing uncertainty. The curvature and
coefficient sensitivities are evaluated at the fitted parameters, with
mgcv's mapping for linked or fixed smoothing parameters. A
positive-definite Hessian is required. This does not also add the
corrected coefficient covariance, which would count that uncertainty
twice. The fitted object must store a matching Hessian and
sensitivities; unsupported fitting methods, including fast REML BAM
fits, give an error. This is a local approximation about the fitted
parameters, not an exact posterior sampler for dispersion or family
shape. With fixed global parameters, the existing coefficient covariance
or
[`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
machinery is used. Delta intervals and Gaussian simulation with
`unconditional = TRUE` and fixed estimated global parameters use the
first-order smoothing correction conditional on those fixed parameters:
native corrected covariance can also include their uncertainty. This
requires the same saved outer Hessian and sensitivities when smoothing
parameters are present. Legacy
[`negbin()`](https://rdrr.io/pkg/mgcv/man/negbin.html) fits outside
ML/REML require explicit `nuisance = "fixed"`; use
[`nb()`](https://rdrr.io/pkg/mgcv/man/negbin.html) to estimate and
propagate shape uncertainty. The covariance calculation uses a
first-order expansion of the coefficient means and does not include
mgcv's additional correction for changes in the conditional coefficient
covariance. The `moment_info` attribute records which approximation was
used.

Nonexistent moments are retained as `NA` with a status and one warning
per call. In simulation, `.p_defined` records the fraction of draws with
a finite value. Intervals and standard errors are withheld if any draw
is undefined; draws are never silently dropped. CV requires a positive
mean, and skewness and kurtosis require positive variance. GEV moments
of order r require shape \< 1/r; t moments require degrees of freedom
greater than their order. Status values distinguish
`"moment_does_not_exist"`, `"nonpositive_mean"`, `"zero_variance"`,
`"undefined_in_draws"`, and `"missing_predictors"`.
`"numerical_failure"` identifies a value that could not be calculated
with finite precision or invalid distribution parameters;
`"derivative_unavailable"` identifies a finite estimate whose local
derivative could not be calculated.

## See also

[`conditional_moments()`](https://gavinsimpson.github.io/gratia/reference/conditional_moments.md),
[`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md),
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)

## Examples

``` r
library("mgcv")
m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
fitted_moments(m, data = mtcars[1:3, ], moments = c("mean", "variance"))
#> # A tibble: 6 x 18
#>    .row .parameter .fitted   .se .lower_ci .upper_ci .status   mpg   cyl  disp
#>   <int> <chr>        <dbl> <dbl>     <dbl>     <dbl> <chr>   <dbl> <dbl> <dbl>
#> 1     1 mean         23.0  0.707     21.6       24.4 ok       21       6   160
#> 2     1 variance      7.12 1.84       4.29      11.8 ok       21       6   160
#> 3     2 mean         21.2  0.662     19.9       22.5 ok       21       6   160
#> 4     2 variance      7.12 1.84       4.29      11.8 ok       21       6   160
#> 5     3 mean         25.3  0.766     23.8       26.8 ok       22.8     4   108
#> 6     3 variance      7.12 1.84       4.29      11.8 ok       22.8     4   108
#> # i 8 more variables: hp <dbl>, drat <dbl>, wt <dbl>, qsec <dbl>, vs <dbl>,
#> #   am <dbl>, gear <dbl>, carb <dbl>
fitted_moments(m, data = mtcars[1:3, ], moments = "variance",
  uncertainty = "simulation", n_draws = 500, seed = 42)
#> # A tibble: 3 x 19
#>    .row .parameter .fitted   .se .lower_ci .upper_ci .status .p_defined   mpg
#>   <int> <chr>        <dbl> <dbl>     <dbl>     <dbl> <chr>        <dbl> <dbl>
#> 1     1 variance      7.12  1.91      4.14      11.1 ok               1  21  
#> 2     2 variance      7.12  1.91      4.14      11.1 ok               1  21  
#> 3     3 variance      7.12  1.91      4.14      11.1 ok               1  22.8
#> # i 10 more variables: cyl <dbl>, disp <dbl>, hp <dbl>, drat <dbl>, wt <dbl>,
#> #   qsec <dbl>, vs <dbl>, am <dbl>, gear <dbl>, carb <dbl>
```
