# Differences of factor smooth interactions

Estimates pairwise differences (comparisons) between factor smooth
interactions (smooths with a factor `by` argument) for pairs of groups
defined by the factor. The group means can be optionally included in the
difference. First and second derivatives of these differences, including
partial derivatives across smooth surfaces, can also be estimated.

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
  n_4d = 4,
  simultaneous_method = c("simulation", "tube"),
  simultaneous_domain = NULL,
  tube_control = list(),
  order = 0L,
  focal = NULL,
  type = c("central", "forward", "backward"),
  eps = NULL,
  wrt = c("smooth", "covariate")
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

  logical; should the group means be included in the difference? Must be
  `FALSE` when `order > 0`.

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

- simultaneous_method:

  character; calibration for simultaneous bands. The default,
  `"simulation"`, uses coefficient draws on the evaluation grid.
  `"tube"` uses a conservative Gaussian threshold-crossing bound over a
  continuous interval for supported one-dimensional cubic splines. See
  [simultaneous_bands](https://gavinsimpson.github.io/gratia/reference/simultaneous_bands.md).

- simultaneous_domain:

  numeric pair of increasing finite endpoints, or a named list of such
  pairs keyed by spline coordinate. For tube bands, `NULL` uses the
  range of returned evaluation coordinates, including gaps. All returned
  points must lie inside the interval and the interval must lie inside
  the fitted spline support. Requires tube calibration.

- tube_control:

  named list controlling numerical tube calibration: `rel_tol` (default
  `1e-6`), `abs_tol` (`1e-8`), `subdivisions` (`1000L`), and `root_tol`
  (`1e-8`). Requires tube calibration. Simulation controls `n_sim`,
  `n_cores` (or `ncores`), and `seed` are ignored by tube calibration.

- order:

  numeric; `0` (the default) estimates smooth differences; `1` and `2`
  estimate their first and second derivatives.

- focal:

  character; one differentiation coordinate shared by all compared
  smooths. Inferred for univariate smooths; required for multivariate
  smooths when `order > 0`.

- type:

  character; finite-difference scheme for derivatives. One of
  `"central"` (the default), `"forward"`, or `"backward"`.

- eps:

  a positive finite number giving the absolute finite-difference step,
  or `NULL` (the default) to choose it automatically. The automatic step
  is the fitted range of the differentiation coordinate multiplied by
  `.Machine$double.eps^(1 / (order + p))`, where `p = 2` for central
  differences and `p = 1` for forward or backward differences. A
  constant coordinate uses its absolute value (or one if zero) instead
  of its range. The step is rounded upwards and bounded below so that
  adding it to the coordinates produces a representable change. Stored
  transformed values or raw covariate summaries supply the range;
  supplied `data` are used if neither is available. This is a
  scale-aware heuristic, not an error bound; unusual function scales or
  domain boundaries may require explicit `eps`. For first central
  differences the two points are separated by `eps`; for second central
  differences they are each `eps` from the target.

- wrt:

  differentiation coordinate: `"smooth"` (the default) uses the smooth
  expression, such as `log(x)`; `"covariate"` uses a raw covariate, such
  as `x`, and reevaluates expressions at each finite-difference point.

## Value

A tibble of class `"difference_smooth"`. Columns `.smooth`, `.by`,
`.level_1`, and `.level_2` identify each comparison; `.diff` contains
the estimate for level 1 minus level 2, with `.se`, `.lower_ci`,
`.upper_ci`, and evaluation covariates. For `order > 0`, `.diff`
contains the derivative difference and `.focal` names its coordinate.
The `derivative_info` attribute records `order`, `focal`, `wrt`, `type`,
and the resolved step `eps`. The `plot_variables` attribute records the
plotting axes.

## Details

With the default simulation calibration, simultaneous intervals cover
smooth differences at the evaluated covariate combinations for each pair
of factor levels, with approximate posterior probability `ci_level` when
using the Bayesian covariance. They do not provide joint coverage across
all pairs, outside the evaluation set, or between its points. With
`data = NULL`, the evaluation set is generated using `n`, `n_2d`,
`n_3d`, and `n_4d`. Differences and interval limits are on the linear
predictor scale, including group means when requested. For supported
univariate cubic splines, `simultaneous_method = "tube"` instead covers
a continuous interval, including between returned points; see
[simultaneous_bands](https://gavinsimpson.github.io/gratia/reference/simultaneous_bands.md).
Coverage remains separate for each pair.

`variables` determines the order used to generate and display the
evaluation grid. The first two covariates are the x and y axes. The
third covariate defines facets for three-dimensional smooths, or facet
rows for four-dimensional smooths; the fourth defines facet columns. Use
`data` to choose particular conditioning values. The resolved order is
saved with the estimates for use by
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md).

The intervals use the joint coefficient covariance selected by
`unconditional` and `frequentist`, retaining covariance between the two
smooths. The covariance must be positive semidefinite. With
`frequentist = TRUE`, simulation instead uses the frequentist covariance
of the coefficient estimators. `n_sim`, `n_cores`, and `seed` are
ignored for pointwise intervals. An explicit seed scopes the entire
call: each pair uses a new batch of coefficient draws, and the caller's
random number state is restored on exit. With `seed = NULL`, the current
random number state is used and advanced.

With `order > 0`, the estimate is the difference between the derivatives
of the selected smooths, on the linear predictor scale. Equivalently, it
is the derivative of their difference. The estimate and its uncertainty
are computed from the difference of the derivative matrices, retaining
covariance between the smooths. Group means are excluded, and tube
intervals are unavailable for derivatives; use simulation for
simultaneous intervals.

For a multivariate smooth, `focal` identifies the coordinate to
differentiate. All other independent coordinates are fixed at their
value in each evaluation row. They can vary between rows, so `data` may
describe a full surface or several slices. `order = 2` differentiates
twice with respect to `focal`; mixed partial derivatives are not
supported. Generated grids adjust the focal range for the
finite-difference scheme; supplied evaluation locations are preserved.

With `wrt = "covariate"`, model expressions are reevaluated after
changing the raw focal covariate. Generated raw grids use
[`smooth_data()`](https://gavinsimpson.github.io/gratia/reference/smooth_data.md)
and require each axis to depend on one distinct original covariate.
Supply raw `data` for other transformations. With `wrt = "smooth"`, only
the named smooth coordinate is changed. Derivative controls are ignored
when `order = 0`. Raw-coordinate comparisons match the same original
covariates, excluding the comparison factor. Evaluated smooth
coordinates may differ between factor levels when a transformation
involves that factor. The finite-difference step must fit within the
fitted focal range for generated grids, including grids with only one
focal value.

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

## See also

[`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md),
[`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)

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

# Compare slopes of the factor-by smooths, retaining their joint uncertainty
slope_dif <- difference_smooths(m, select = "s(x2)", order = 1)
slope_dif
#> # A tibble: 300 x 10
#>    .smooth .focal .by   .level_1 .level_2 .diff   .se .lower_ci .upper_ci
#>    <chr>   <chr>  <chr> <chr>    <chr>    <dbl> <dbl>     <dbl>     <dbl>
#>  1 s(x2)   x2     fac   1        2         9.24  5.74   -2.01        20.5
#>  2 s(x2)   x2     fac   1        2         9.24  5.73   -1.99        20.5
#>  3 s(x2)   x2     fac   1        2         9.23  5.69   -1.93        20.4
#>  4 s(x2)   x2     fac   1        2         9.22  5.60   -1.77        20.2
#>  5 s(x2)   x2     fac   1        2         9.18  5.46   -1.51        19.9
#>  6 s(x2)   x2     fac   1        2         9.13  5.26   -1.18        19.4
#>  7 s(x2)   x2     fac   1        2         9.06  5.02   -0.785       18.9
#>  8 s(x2)   x2     fac   1        2         8.95  4.76   -0.383       18.3
#>  9 s(x2)   x2     fac   1        2         8.81  4.50   -0.0138      17.6
#> 10 s(x2)   x2     fac   1        2         8.64  4.26    0.290       17.0
#> # i 290 more rows
#> # i 1 more variable: x2 <dbl>
draw(slope_dif)


# Fit a surface for each factor level to compare rates of change in x2
m2 <- gam(y ~ fac + te(x2, x0, by = fac, k = c(4, 4)),
  data = df, method = "REML")

# Evaluate partial derivatives of the differences across both covariates
partial_dif <- difference_smooths(m2, select = "te(x2,x0)",
  order = 1, focal = "x2", n_2d = 20)
partial_dif
#> # A tibble: 1,200 x 11
#>    .smooth   .focal .by   .level_1 .level_2 .diff   .se .lower_ci .upper_ci
#>    <chr>     <chr>  <chr> <chr>    <chr>    <dbl> <dbl>     <dbl>     <dbl>
#>  1 te(x2,x0) x2     fac   1        2         9.24  7.55    -5.57       24.0
#>  2 te(x2,x0) x2     fac   1        2         9.19  6.85    -4.24       22.6
#>  3 te(x2,x0) x2     fac   1        2         9.13  6.22    -3.07       21.3
#>  4 te(x2,x0) x2     fac   1        2         9.08  5.69    -2.08       20.2
#>  5 te(x2,x0) x2     fac   1        2         9.02  5.25    -1.27       19.3
#>  6 te(x2,x0) x2     fac   1        2         8.97  4.90    -0.640      18.6
#>  7 te(x2,x0) x2     fac   1        2         8.92  4.62    -0.131      18.0
#>  8 te(x2,x0) x2     fac   1        2         8.86  4.37     0.296      17.4
#>  9 te(x2,x0) x2     fac   1        2         8.80  4.16     0.638      17.0
#> 10 te(x2,x0) x2     fac   1        2         8.72  4.01     0.858      16.6
#> # i 1,190 more rows
#> # i 2 more variables: x2 <dbl>, x0 <dbl>
draw(partial_dif)


# Choose a surface grid and calibrate simultaneous intervals for each pair
derivative_grid <- data_slice(m2, x2 = evenly(x2, n = 15),
  x0 = evenly(x0, n = 10), fac = levels(fac))
partial_sim <- difference_smooths(m2, select = "te(x2,x0)",
  data = derivative_grid, order = 1, focal = "x2",
  interval = "simultaneous", n_sim = 1000, seed = 42)
partial_sim
#> # A tibble: 450 x 11
#>    .smooth   .focal .by   .level_1 .level_2 .diff   .se .lower_ci .upper_ci
#>    <chr>     <chr>  <chr> <chr>    <chr>    <dbl> <dbl>     <dbl>     <dbl>
#>  1 te(x2,x0) x2     fac   1        2         9.24  7.55    -14.2       32.7
#>  2 te(x2,x0) x2     fac   1        2         9.13  6.16     -9.98      28.2
#>  3 te(x2,x0) x2     fac   1        2         9.01  5.17     -7.02      25.0
#>  4 te(x2,x0) x2     fac   1        2         8.90  4.53     -5.15      22.9
#>  5 te(x2,x0) x2     fac   1        2         8.77  4.09     -3.92      21.5
#>  6 te(x2,x0) x2     fac   1        2         8.56  3.92     -3.58      20.7
#>  7 te(x2,x0) x2     fac   1        2         8.21  4.04     -4.31      20.7
#>  8 te(x2,x0) x2     fac   1        2         7.66  4.43     -6.10      21.4
#>  9 te(x2,x0) x2     fac   1        2         6.97  5.28     -9.41      23.3
#> 10 te(x2,x0) x2     fac   1        2         6.21  6.60    -14.3       26.7
#> # i 440 more rows
#> # i 2 more variables: x2 <dbl>, x0 <dbl>

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
