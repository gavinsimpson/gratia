# Generate fitted values from a estimated GAM

Generate fitted values from a estimated GAM

## Usage

``` r
fitted_values(object, ...)

# S3 method for class 'gam'
fitted_values(
  object,
  data = NULL,
  scale = c("response", "link", "linear predictor"),
  ci_level = 0.95,
  envir = NULL,
  ...,
  interval = c("confidence", "simultaneous"),
  n_sim = 10000,
  n_cores = 1,
  seed = NULL,
  unconditional = FALSE
)

# S3 method for class 'gamm'
fitted_values(object, ...)

# S3 method for class 'scam'
fitted_values(object, ...)
```

## Arguments

- object:

  a fitted model. Currently only models fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) are supported.

- ...:

  arguments passed to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).
  Note that `type`, `newdata`, and `se.fit` are already used and passed
  on to
  [`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html).

- data:

  optional data frame of covariate values for which fitted values are to
  be returned.

- scale:

  character; what scale should the fitted values be returned on?
  `"linear predictor"` is a synonym for `"link"` if you prefer that
  terminology.

- ci_level:

  numeric; a value between 0 and 1 indicating the coverage of the
  credible interval.

- envir:

  an optional environment supplying functions and constants used in
  model expressions. The available model formula environment is used
  when `NULL`. Covariate observations should be supplied in `data`.

- interval:

  character; `"confidence"` (the default) gives pointwise intervals.
  `"simultaneous"` gives simultaneous intervals for predictions at the
  supplied covariate combinations.

- n_sim:

  positive integer; number of coefficient draws used for simultaneous
  intervals. Ignored for pointwise intervals.

- n_cores:

  positive integer; number of cores used by
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html) for
  simultaneous intervals. Parallel execution requires OpenMP support.

- seed:

  integer or `NULL`; optional random seed for simultaneous intervals. An
  explicit seed preserves the caller's random number state. With `NULL`,
  the current random number state is used and advanced.

- unconditional:

  logical; include smoothing parameter uncertainty in the Bayesian
  covariance matrix, if available. Otherwise a warning is issued and the
  uncorrected covariance is used.

## Value

A tibble (data frame) whose first *m* columns contain either the data
used to fit the model (if `data` was `NULL`), or the variables supplied
to `data`. Four further columns are added:

- `.fitted`: the fitted values on the specified scale,

- `.se`: the standard error of the fitted values (always on the *link*
  scale),

- `.lower_ci`, `.upper_ci`: the limits of the credible interval on the
  fitted values, on the specified scale.

Models fitted with certain families will include additional variables

- [`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) models: when
  `scale = "response"`, the returned object will contain a `row` column
  and a `category` column, which indicate to which row of the `data`
  each row of the returned object belongs. Additionally, there will be
  `nrow(data) * n_categories` rows in the returned object; each row is
  the predicted probability for a single category of the response.

## Details

With `data = NULL`, results follow the model's `na.action`: `na.exclude`
restores excluded observations as `NA` fitted values, standard errors
and interval bounds; `na.omit` returns only retained observations.
Covariates unavailable in the stored model frame are restored as typed
`NA` values. `.row` indexes the fitting data after any `subset` was
applied.

Supplying `data` requests predictions at those rows, independently of
training exclusions. Missing responses do not prevent prediction.
Missing required predictors produce `NA` results by default. An explicit
`na.action` passed through `...` is honoured; with `na.omit`, `.row`
retains the positions in the supplied data, rather than numbering the
remaining rows consecutively.

Simultaneous intervals jointly cover the model's underlying expected
responses, or linear predictors on the link scale, at all valid
covariate combinations evaluated in this call, with approximate
posterior probability `ci_level`. The fitted values are estimates of
these quantities. The combinations may include factor levels and need
not form an ordered sequence, curve, or grid. This interpretation also
applies to two predictions. With `data = NULL`, the simultaneous set
comprises the retained fitting covariate combinations. Missing
predictions do not enter this set. Coverage is not asserted outside this
set or between its points. Separate calls define separate simultaneous
sets. These are uncertainty intervals for model predictions, not
prediction intervals for future observations.

Simultaneous intervals use joint Gaussian coefficient draws and a
critical value based on the maximum absolute standardized deviation over
the supplied rows. Their limits are computed on the link scale and
transformed if `scale = "response"`; `.se` remains on the link scale.
The simulation covariance must be positive definite. Supported families
are Gaussian, Poisson, binomial, Gamma, inverse Gaussian, quasi
families, negative binomial, Tweedie, beta regression, and scaled t,
fitted by `gam()` or `bam()` (including the `gam` component of a
`gamm()` fit). Other families and `scam` models currently support
pointwise intervals only.

Response-scale simultaneous intervals support the identity, log, logit,
probit, cloglog, cauchit, square-root, inverse, and inverse-squared
links. Intervals crossing an inverse-link domain boundary are rejected;
use `scale = "link"` for these intervals or for other links.

`terms` and `exclude` in `...` retain
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
semantics and apply to both prediction and interval calculation. If
terms are selected or excluded, intervals concern that selected
quantity, not necessarily the full expected response. Use the link scale
for sums of smooth contributions, and consider the intercept and offset
treatment when selecting terms.

## Note

For most families, regardless of the scale on which the fitted values
are returned, the `se` component of the returned object is on the *link*
(*linear predictor*) scale, not the response scale. An exception is the
[`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) family, for
which the `se` is on the response scale if `scale = "response"`.

## Examples

``` r
load_mgcv()
sim_df <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = sim_df, method = "REML")
fv <- fitted_values(m)
fv
#> # A tibble: 400 x 9
#>     .row       x0        x1         x2       x3  .fitted      .se .lower_ci
#>    <int>    <dbl>     <dbl>      <dbl>    <dbl>    <dbl>    <dbl>     <dbl>
#>  1     1 0.184882 0.617142  0.415244   0.132410  8.73875 0.354677   8.04360
#>  2     2 0.702374 0.569064  0.531439   0.365331  7.62581 0.337779   6.96378
#>  3     3 0.573326 0.153970  0.00324621 0.454532  3.12106 0.591862   1.96103
#>  4     4 0.168052 0.0348332 0.252100   0.537114 11.1124  0.402378  10.3237 
#>  5     5 0.943839 0.997953  0.155229   0.185495 14.0533  0.452947  13.1655 
#>  6     6 0.943475 0.835574  0.878840   0.449276  6.13080 0.364521   5.41635
#>  7     7 0.129159 0.586562  0.203511   0.256527 12.4838  0.355808  11.7864 
#>  8     8 0.833449 0.339117  0.583528   0.618458  6.25215 0.344700   5.57655
#>  9     9 0.468019 0.166883  0.804473   0.880744  4.21463 0.372003   3.48552
#> 10    10 0.549984 0.807410  0.264717   0.317747 15.5283  0.369999  14.8031 
#> # i 390 more rows
#> # i 1 more variable: .upper_ci <dbl>

# Simultaneous intervals for two arbitrary covariate combinations
fitted_values(m, data = sim_df[c(1, 20), ], interval = "simultaneous",
  n_sim = 1000, seed = 42)
#> # A tibble: 2 x 15
#>    .row       y        x0       x1       x2       x3       f       f0      f1
#>   <int>   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>   <dbl>    <dbl>   <dbl>
#> 1     1 7.12834 0.184882  0.617142 0.415244 0.132410 8.38643 1.09743  3.43592
#> 2     2 2.93507 0.0749794 0.797900 0.874521 0.203225 5.57766 0.466765 4.93227
#> # i 6 more variables: f2 <dbl>, f3 <dbl>, .fitted <dbl>, .se <dbl>,
#> #   .lower_ci <dbl>, .upper_ci <dbl>

# A selected sum of smooth contributions on the link scale
fitted_values(m, data = sim_df[c(1, 20), ], scale = "link",
  terms = c("s(x2)", "s(x3)"), interval = "simultaneous",
  n_sim = 1000, seed = 42)
#> # A tibble: 2 x 15
#>    .row       y        x0       x1       x2       x3       f       f0      f1
#>   <int>   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>   <dbl>    <dbl>   <dbl>
#> 1     1 7.12834 0.184882  0.617142 0.415244 0.132410 8.38643 1.09743  3.43592
#> 2     2 2.93507 0.0749794 0.797900 0.874521 0.203225 5.57766 0.466765 4.93227
#> # i 6 more variables: f2 <dbl>, f3 <dbl>, .fitted <dbl>, .se <dbl>,
#> #   .lower_ci <dbl>, .upper_ci <dbl>
```
