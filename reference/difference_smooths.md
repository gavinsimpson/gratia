# Differences of factor smooth interactions

Estimates pairwise differences (comparisons) between factor smooth
interactions (smooths with a factor `by` argument) for pairs of groups
defined by the factor. The group means can be optionally included in the
difference.

## Usage

``` r
difference_smooths(model, ...)

# S3 method for class 'gam'
difference_smooths(
  model,
  select = NULL,
  smooth = deprecated(),
  n = 100,
  n_2d = 50,
  ci_level = 0.95,
  data = NULL,
  group_means = FALSE,
  partial_match = TRUE,
  unconditional = FALSE,
  frequentist = FALSE,
  envir = NULL,
  ...,
  interval = c("confidence", "simultaneous"),
  n_sim = 10000,
  n_cores = 1,
  seed = NULL,
  variables = NULL,
  n_3d = 16,
  n_4d = 4
)

# S3 method for class 'scam'
difference_smooths(model, ..., unconditional = FALSE)
```

## Arguments

- model:

  A fitted model.

- ...:

  arguments passed to other methods. Not currently used.

- select:

  character, logical, or numeric; which smooths to compare. If `NULL`,
  the default, then all model smooths are factor-smooth interactions are
  compared. Numeric `select` indexes the smooths in the order they are
  specified in the formula and stored in `object`. Character `select`
  matches the labels for smooths as shown for example in the output from
  `summary(object)`. Logical `select` operates as per numeric `select`
  in the order that smooths are stored. Careful selection is needed
  because it is not allowed to compare smooths of different covariates
  or of different factor-by variables.

  For character `select`, specific named smooths cane be provided, in
  which case, the exact names of the smooths (as given by
  [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md),
  for example, can be specified, and `partial_match` must be set to
  `FALSE`.

- smooth:

  **\[deprecated\]** Use `select` instead.

- n:

  numeric; the number of points at which to evaluate the difference
  between pairs of smooths.

- n_2d:

  numeric; the number of points along each of the first two axes of a
  smooth surface, including surface panels of higher-dimensional
  smooths. The default is 50 in plotting and plot-preparation functions.
  If `NULL`, use `n` instead. Ignored when evaluation `data` are
  supplied. Factor levels are retained, and curves with only one
  continuous covariate use `n`.

- ci_level:

  numeric between 0 and 1; the coverage of credible interval.

- data:

  data frame of locations at which to evaluate the difference between
  smooths.

- group_means:

  logical; should the group means be included in the difference?

- partial_match:

  logical; should `smooth` match partially against `smooths`? If
  `partial_match = TRUE`, `smooth` must only be a single string, a
  character vector of length 1. Unlike similar functions, the default
  here is `TRUE` because the intention is that users will be matching
  against factor-by smooth labels.

- unconditional:

  logical; account for smoothness selection in the model?

- frequentist:

  logical; use the frequentist covariance matrix?

- envir:

  an optional environment supplying functions and constants used in
  model expressions. The available model formula environment is used
  when `NULL`. Covariate observations should be supplied in `data`.

- interval:

  character; `"confidence"` (the default) gives pointwise intervals.
  `"simultaneous"` gives simultaneous intervals for differences of
  smooths at the supplied covariate combinations, separately for each
  pair of factor levels.

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

- variables:

  Character vector giving every covariate of the selected smooths
  exactly once, in plotting order: x, y, then the conditioning covariate
  for 3D smooths, or facet rows and columns for 4D smooths. When
  supplied, selected smooths must use the same covariates. Use fitted
  coordinate names, such as `"log(x)"` for a transformed covariate. The
  default, `NULL`, favours a two-variable tensor margin for x and y,
  otherwise retaining fitted order. Grid resolution follows this order.
  Supplied `data` are evaluated unchanged. Drawing saved estimates can
  change the order but uses only points already evaluated; evaluate
  again to change grid resolution or conditioning values. Continuous
  covariates must precede factors. Spherical smooths retain their
  latitude/longitude convention and do not support reordering.

- n_3d, n_4d:

  numeric; the number of conditioning values for smooths of three or
  four covariates. The defaults are 16 and 4, respectively. If `NULL`,
  use `n`. Ignored when evaluation `data` are supplied.

## Details

Simultaneous intervals jointly cover the underlying smooth differences
at the evaluated covariate combinations for each pair of factor levels,
with approximate posterior probability `ci_level` when using the
Bayesian covariance. They do not provide joint coverage across all
pairs, outside the evaluation set, or between its points. With
`data = NULL`, the evaluation set is generated using `n`, `n_2d`,
`n_3d`, and `n_4d`. Differences and interval limits are on the linear
predictor scale, including group means when requested.

`variables` determines the order used to generate and display the
evaluation grid. The first two covariates are the x and y axes. The
third covariate defines facets for three-dimensional smooths, or facet
rows for four-dimensional smooths; the fourth defines facet columns. Use
`data` to choose particular conditioning values. The resolved order is
saved with the estimates for use by
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md).

The intervals use the joint coefficient covariance selected by
`unconditional` and `frequentist`, retaining covariance between the two
smooths. The simulation covariance must be positive definite. With
`frequentist = TRUE`, simulation instead uses the frequentist covariance
of the coefficient estimators. `n_sim`, `n_cores`, and `seed` are
ignored for pointwise intervals. An explicit seed scopes the entire
call: each pair uses a new batch of coefficient draws, and the caller's
random number state is restored on exit. With `seed = NULL`, the current
random number state is used and advanced.

## Parallel calculation

With mirai and carrier installed, configure workers with
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html) to
distribute calculations across selected smooths. `difference_smooths()`
distributes factor-level comparisons instead. Without configured
workers, or with only one calculation, evaluation is serial. These
functions leave the caller's worker pool running.

Model selection, evaluation of model expressions and random draws are
prepared in the caller. Workers receive the coordinates and coefficient
columns needed for their calculation. Custom prediction methods are
evaluated in the caller, including custom tensor margins and
factor-smooth bases; workers receive their prediction matrices.
Simultaneous intervals use successive draws in selection order, so
[`set.seed()`](https://rdrr.io/r/base/Random.html) gives the same
results with or without workers, independently of their number or
completion order.
[`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
uses [`set.seed()`](https://rdrr.io/r/base/Random.html); the other
functions also accept `seed`, which restores the caller's random number
state on exit.

[`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
draws coefficients jointly once, then reuses those draws for every
smooth, preserving dependence and chain identities. Sampling finishes
before smooth calculations start, so MH chains and smooth workers are
not nested. Sampling thread controls keep their existing meanings.
Workers use their configured BLAS thread settings; when using a threaded
BLAS, limit its threads before starting daemons to avoid competing
threads.

Parallel work is most useful for expensive bases or interval
calculations. Small grids can be slower because of transfer and
scheduling costs. Large grids and many draws also require memory for
worker inputs and returned results, in addition to the final output.

## Examples

``` r

load_mgcv()
df <- data_sim("eg4", seed = 42)
m <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = df, method = "REML")

sm_dif <- difference_smooths(m, select = "s(x2)")
sm_dif
#> # A tibble: 300 x 9
#>    .smooth .by   .level_1 .level_2 .diff   .se .lower_ci .upper_ci      x2
#>    <chr>   <chr> <chr>    <chr>    <dbl> <dbl>     <dbl>     <dbl>   <dbl>
#>  1 s(x2)   fac   1        2        0.386 0.618  -0.824        1.60 0.00359
#>  2 s(x2)   fac   1        2        0.479 0.574  -0.646        1.60 0.0136 
#>  3 s(x2)   fac   1        2        0.572 0.534  -0.474        1.62 0.0237 
#>  4 s(x2)   fac   1        2        0.665 0.497  -0.308        1.64 0.0338 
#>  5 s(x2)   fac   1        2        0.758 0.464  -0.151        1.67 0.0438 
#>  6 s(x2)   fac   1        2        0.850 0.435  -0.00342      1.70 0.0539 
#>  7 s(x2)   fac   1        2        0.941 0.412   0.134        1.75 0.0639 
#>  8 s(x2)   fac   1        2        1.03  0.393   0.262        1.80 0.0740 
#>  9 s(x2)   fac   1        2        1.12  0.378   0.380        1.86 0.0841 
#> 10 s(x2)   fac   1        2        1.21  0.367   0.489        1.93 0.0941 
#> # i 290 more rows

draw(sm_dif)


# include the groups means for `fac` in the difference
sm_dif2 <- difference_smooths(m, select = "s(x2)", group_means = TRUE)
draw(sm_dif2)


# simultaneous intervals, separately for each pair of factor levels
sm_sim <- difference_smooths(m, select = "s(x2)",
  interval = "simultaneous", n_sim = 1000, seed = 42)
draw(sm_sim)


# compare specific smooths
sm_dif3 <- difference_smooths(m,
  select = c("s(x2):fac1", "s(x2):fac2"), partial_match = FALSE
)

# Choose two axes and condition on the third covariate
m3 <- gam(y ~ fac + te(x0, x1, x2, by = fac, k = c(3, 3, 3)),
  data = df, method = "REML")
sm_dif_3d <- difference_smooths(m3, select = "te(x0,x1,x2)",
  variables = c("x1", "x2", "x0"), n_2d = 20, n_3d = 3)
draw(sm_dif_3d)

# Redraw the evaluated points with the axes exchanged
draw(sm_dif_3d, variables = c("x2", "x1", "x0"))


# Supply particular conditioning values using a prediction grid
grid <- data_slice(m3, x1 = evenly(x1, n = 20),
  x2 = evenly(x2, n = 20), x0 = c(0.2, 0.8), fac = levels(fac))
sm_dif_at <- difference_smooths(m3, select = "te(x0,x1,x2)",
  data = grid, variables = c("x1", "x2", "x0"))
draw(sm_dif_at)
```
