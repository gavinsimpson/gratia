# Partial derivatives of estimated multivariate smooths via finite differences

Partial derivatives of estimated multivariate smooths via finite
differences

## Usage

``` r
partial_derivatives(object, ...)

# Default S3 method
partial_derivatives(object, ...)

# S3 method for class 'gamm'
partial_derivatives(object, ...)

# S3 method for class 'gam'
partial_derivatives(
  object,
  select = NULL,
  term = deprecated(),
  focal = NULL,
  data = newdata,
  order = 1L,
  type = c("forward", "backward", "central"),
  n = 100,
  eps = NULL,
  interval = c("confidence", "simultaneous"),
  n_sim = 10000,
  level = 0.95,
  unconditional = FALSE,
  frequentist = FALSE,
  offset = NULL,
  ncores = 1,
  partial_match = FALSE,
  seed = NULL,
  ...,
  newdata = NULL,
  envir = NULL,
  wrt = c("smooth", "covariate"),
  interval_method = c("auto", "gaussian", "bootstrap"),
  simultaneous_method = c("simulation", "tube"),
  simultaneous_domain = NULL,
  tube_control = list()
)

# S3 method for class 'gamm4'
partial_derivatives(object, ...)

# S3 method for class 'scam'
partial_derivatives(object, ..., unconditional = FALSE)
```

## Arguments

- object:

  an R object to compute derivatives for.

- ...:

  arguments passed to other methods.

- select:

  character; vector of one or more smooth terms for which derivatives
  are required. If missing, derivatives for all smooth terms will be
  returned. Can be a partial match to a smooth term; see argument
  `partial_match` below.

- term:

  **\[deprecated\]** Use `select` instead.

- focal:

  character; name of the focal variable. The partial derivative of the
  estimated smooth with respect to this variable will be returned. All
  other variables involved in the smooth will be held at constant. This
  can be missing if supplying `data`, in which case, the focal variable
  will be identified as the one variable that is not constant.

- data:

  a data frame containing the values of the model covariates at which to
  evaluate the first derivatives of the smooths. If supplied, all but
  one variable must be held at a constant value.

- order:

  numeric; the order of derivative.

- type:

  character; the type of finite difference used. One of `"forward"`,
  `"backward"`, or `"central"`.

- n:

  numeric; the number of points to evaluate the derivative at.

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

- interval:

  character; the type of interval to compute. One of `"confidence"` for
  point-wise intervals, or `"simultaneous"` for simultaneous intervals.

- n_sim:

  integer; the number of simulations used in computing the simultaneous
  intervals.

- level:

  numeric; `0 < level < 1`; the confidence level of the point-wise or
  simultaneous interval. The default is `0.95` for a 95% interval.

- unconditional:

  logical; use smoothness selection-corrected Bayesian covariance
  matrix?

- frequentist:

  logical; use the frequentist covariance matrix?

- offset:

  numeric; a value to use for any offset term

- ncores:

  number of cores for generating random variables from a multivariate
  normal distribution. Passed to
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html).
  Parallelization will take place only if OpenMP is supported (but
  appears to work on Windows with current `R`).

- partial_match:

  logical; should smooths be selected by partial matches with `term`? If
  `TRUE`, `term` can only be a single string to match against.

- seed:

  numeric; RNG seed to use.

- newdata:

  Deprecated: use `data` instead.

- envir:

  optional environment for functions and constants in model expressions;
  see
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md).

- wrt:

  differentiation coordinate: `"smooth"` (the default) uses the smooth
  expression, such as `log(x)`; `"covariate"` uses a raw covariate, such
  as `x`, and reevaluates expressions at each finite-difference point.

- interval_method:

  character; use stored scasm bootstrap draws for pointwise intervals
  with `"auto"` (the default), or explicitly select `"gaussian"` or
  `"bootstrap"`. Bootstrap simultaneous bands are unavailable.

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

## Value

A tibble, currently with the following variables:

- `.smooth`: the smooth each row refers to,

- `.partial_deriv`: the estimated partial derivative,

- `.se`: the standard error of the estimated partial derivative,

- `.crit`: the critical value such that `derivative` ± `(crit * se)`
  gives the upper and lower bounds of the requested confidence or
  simultaneous interval (given `level`); `NA` for bootstrap limits,

- `.lower_ci`: the lower bound of the confidence or simultaneous
  interval,

- `.upper_ci`: the upper bound of the confidence or simultaneous
  interval.

## Note

`partial_derivatives()` will ignore any random effect smooths it
encounters in `object`. For soap-film smooths with known boundary
values, derivatives include the fixed boundary contribution. This shifts
the estimates and interval limits without adding uncertainty from fitted
coefficients.

## Parallel calculation

With mirai and carrier installed, configure workers with
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html) to
distribute calculations across selected smooths.
[`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
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

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson

## Examples

``` r

library("ggplot2")
library("patchwork")
load_mgcv()
df <- data_sim("eg2", n = 2000, dist = "normal", scale = 0.5, seed = 42)

# fit the GAM (note: for execution time reasons, k is set articifially low)
m <- gam(y ~ te(x, z, k = c(5, 5)), data = df, method = "REML")

# data slice through te(x,z) holding z == 0.4
ds <- data_slice(m, x = evenly(x, n = 100), z = 0.4)

# evaluate te(x,z) at values of x & z
sm <- smooth_estimates(m, select = "te(x,z)", data = ds) |>
  add_confint()

# partial derivatives
pd_x <- partial_derivatives(m, data = ds, type = "central", focal = "x")

# draw te(x,z)
p1 <- draw(m, rug = FALSE) &
  geom_hline(yintercept = 0.4, linewidth = 1)
p1


# draw te(x,z) along slice
cap <- expression(z == 0.4)
p2 <- sm |>
  ggplot(aes(x = x, y = .estimate)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  geom_line() +
  labs(
    x = "x", y = "Partial effect", title = "te(x,z)",
    caption = cap
  )
p2


# draw partial derivs
p3 <- pd_x |>
  draw() +
  labs(caption = cap)
p3


# draw all three panels
p1 + p2 + p3 + plot_layout(ncol = 3)
```
