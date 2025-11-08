# Point-wise and simultaneous confidence intervals for smooths

Calculates point-wise confidence or simultaneous intervals for the
smooth terms of a fitted GAM.

## Usage

``` r
# S3 method for class 'gam'
confint(
  object,
  parm,
  level = 0.95,
  data = newdata,
  n = 100,
  type = c("confidence", "simultaneous"),
  nsim = 10000,
  shift = FALSE,
  transform = FALSE,
  unconditional = FALSE,
  ncores = 1,
  partial_match = FALSE,
  ...,
  newdata = NULL
)

# S3 method for class 'gamm'
confint(object, ...)

# S3 method for class 'list'
confint(object, ...)
```

## Arguments

- object:

  an object of class `"gam"` or `"gamm"`.

- parm:

  which parameters (smooth terms) are to be given intervals as a vector
  of terms. If missing, all parameters are considered, although this is
  not currently implemented.

- level:

  numeric, `0 < level < 1`; the confidence level of the point-wise or
  simultaneous interval. The default is `0.95` for a 95% interval.

- data:

  data frame; new values of the covariates used in the model fit. The
  selected smooth(s) will be evaluated at the supplied values.

- n:

  numeric; the number of points to evaluate smooths at.

- type:

  character; the type of interval to compute. One of `"confidence"` for
  point-wise intervals, or `"simultaneous"` for simultaneous intervals.

- nsim:

  integer; the number of simulations used in computing the simultaneous
  intervals.

- shift:

  logical; should the constant term be add to the smooth?

- transform:

  logical; should the smooth be evaluated on a transformed scale? For
  generalised models, this involves applying the inverse of the link
  function used to fit the model. Alternatively, the name of, or an
  actual, function can be supplied to transform the smooth and it's
  confidence interval.

- unconditional:

  logical; if `TRUE` (and `freq == FALSE`) then the Bayesian smoothing
  parameter uncertainty corrected covariance matrix is returned, if
  available.

- ncores:

  number of cores for generating random variables from a multivariate
  normal distribution. Passed to
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html).
  Parallelization will take place only if OpenMP is supported (but
  appears to work on Windows with current `R`).

- partial_match:

  logical; should matching `parm` use a partial match or an exact match?
  Can only be used if `length(parm)` is `1`.

- ...:

  additional arguments for methods

- newdata:

  DEPRECATED! data frame; containing new values of the covariates used
  in the model fit. The selected smooth(s) will be evaluated at the
  supplied values.

## Value

a tibble with components:

1.  `.smooth`; character indicating to which term each row relates,

2.  `.type`; the type of smooth,

3.  `.by` the name of the by variable if a by smooth, `NA` otherwise,

4.  one or more vectors of values at which the smooth was evaluated,
    named as per the variables in the smooth,

5.  zero or more variables containing values of the by variable,

6.  `.estimate`; estimated value of the smooth,

7.  `.se`; standard error of the estimated value of the smooth,

8.  `.crit`; critical value for the `100 * level`% confidence interval.

9.  `.lower_ci`; lower limit of the confidence or simultaneous interval,

10. `.upper_ci`; upper limit of the confidence or simultaneous interval,

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

# new data to evaluate the smooths at, say over the middle 50% of range
# of each covariate
middle <- function(x, n = 50, coverage = 0.5) {
  v <- (1 - coverage) / 2
  q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
  seq(q[1], q[2], length = n)
}
new_data <- sapply(dat[c("x0", "x1", "x2", "x3")], middle)
new_data <- data.frame(new_data)

## point-wise interval for smooth of x2
ci <- confint(mod, parm = "s(x2)", type = "confidence", data = new_data)
ci
#> # A tibble: 50 x 9
#>    .smooth .type .by      x2 .estimate   .se .crit .lower_ci .upper_ci
#>    <chr>   <chr> <chr> <dbl>     <dbl> <dbl> <dbl>     <dbl>     <dbl>
#>  1 s(x2)   TPRS  NA     0.26       5.3  0.18   2.0       5.0       5.7
#>  2 s(x2)   TPRS  NA     0.27       5.1  0.18   2.0       4.8       5.5
#>  3 s(x2)   TPRS  NA     0.28       4.9  0.18   2.0       4.6       5.3
#>  4 s(x2)   TPRS  NA     0.29       4.6  0.18   2.0       4.3       5.0
#>  5 s(x2)   TPRS  NA     0.30       4.3  0.19   2.0       3.9       4.7
#>  6 s(x2)   TPRS  NA     0.32       4.0  0.19   2.0       3.6       4.3
#>  7 s(x2)   TPRS  NA     0.33       3.6  0.20   2.0       3.2       4.0
#>  8 s(x2)   TPRS  NA     0.34       3.2  0.20   2.0       2.9       3.6
#>  9 s(x2)   TPRS  NA     0.35       2.9  0.20   2.0       2.5       3.3
#> 10 s(x2)   TPRS  NA     0.36       2.5  0.19   2.0       2.1       2.9
#> # i 40 more rows
```
