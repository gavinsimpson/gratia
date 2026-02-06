# Derivatives of estimated smooths via finite differences

Derivatives of estimated smooths via finite differences

## Usage

``` r
derivatives(object, ...)

# Default S3 method
derivatives(object, ...)

# S3 method for class 'gamm'
derivatives(object, ...)

# S3 method for class 'gam'
derivatives(
  object,
  select = NULL,
  term = deprecated(),
  data = newdata,
  order = 1L,
  type = c("forward", "backward", "central"),
  n = 100,
  eps = 1e-07,
  interval = c("confidence", "simultaneous"),
  n_sim = 10000,
  level = 0.95,
  unconditional = FALSE,
  frequentist = FALSE,
  offset = NULL,
  ncores = 1,
  partial_match = FALSE,
  ...,
  newdata = NULL
)
```

## Arguments

- object:

  an R object to compute derivatives for.

- ...:

  arguments passed to other methods.

- select:

  character; select which smooth's posterior to draw from. The default
  (`NULL`) means the posteriors of all smooths in `model` will be
  sampled from. If supplied, a character vector of requested terms. Can
  be a partial match to a smooth term; see argument `partial_match`
  below.

- term:

  **\[deprecated\]** Use `select` instead.

- data:

  a data frame containing the values of the model covariates at which to
  evaluate the first derivatives of the smooths.

- order:

  numeric; the order of derivative.

- type:

  character; the type of finite difference used. One of `"forward"`,
  `"backward"`, or `"central"`.

- n:

  numeric; the number of points to evaluate the derivative at.

- eps:

  numeric; the finite difference.

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

- newdata:

  Deprecated: use `data` instead.

## Value

A tibble, currently with the following variables:

- `.smooth`: the smooth each row refers to,

- `.by`: the name of any factor by variable involved in the smooth,

- `.fs`: the name of any random factor variable involved in the smooth,

- `.derivative`: the estimated derivative,

- `.se`: the standard error of the estimated derivative,

- `.crit`: the critical value such that `derivative` ± `(crit * se)`
  gives the upper and lower bounds of the requested confidence or
  simultaneous interval (given `level`),

- `.lower_ci`: the lower bound of the confidence or simultaneous
  interval,

- `.upper_ci`: the upper bound of the confidence or simultaneous
  interval.

- plus one or more columns of data containing the values of covariates
  at which the derivative was evaluated.

## Note

`derivatives()` will ignore any random effect smooths it encounters in
`object`.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 42)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

## first derivatives of all smooths using central finite differences
derivatives(mod, type = "central")
#> # A tibble: 400 x 12
#>    .smooth .by   .fs   .derivative   .se .crit .lower_ci .upper_ci      x0    x1
#>    <chr>   <chr> <chr>       <dbl> <dbl> <dbl>     <dbl>     <dbl>   <dbl> <dbl>
#>  1 s(x0)   NA    NA           7.41  3.33  1.96     0.874      13.9 2.39e-4    NA
#>  2 s(x0)   NA    NA           7.40  3.33  1.96     0.884      13.9 1.03e-2    NA
#>  3 s(x0)   NA    NA           7.39  3.30  1.96     0.929      13.8 2.04e-2    NA
#>  4 s(x0)   NA    NA           7.36  3.24  1.96     1.01       13.7 3.04e-2    NA
#>  5 s(x0)   NA    NA           7.32  3.15  1.96     1.14       13.5 4.05e-2    NA
#>  6 s(x0)   NA    NA           7.26  3.04  1.96     1.30       13.2 5.06e-2    NA
#>  7 s(x0)   NA    NA           7.18  2.90  1.96     1.49       12.9 6.06e-2    NA
#>  8 s(x0)   NA    NA           7.09  2.76  1.96     1.69       12.5 7.07e-2    NA
#>  9 s(x0)   NA    NA           6.99  2.61  1.96     1.87       12.1 8.07e-2    NA
#> 10 s(x0)   NA    NA           6.87  2.47  1.96     2.03       11.7 9.08e-2    NA
#> # i 390 more rows
#> # i 2 more variables: x2 <dbl>, x3 <dbl>

## derivatives for a selected smooth
derivatives(mod, type = "central", select = "s(x1)")
#> # A tibble: 100 x 9
#>    .smooth .by   .fs   .derivative   .se .crit .lower_ci .upper_ci       x1
#>    <chr>   <chr> <chr>       <dbl> <dbl> <dbl>     <dbl>     <dbl>    <dbl>
#>  1 s(x1)   NA    NA         -0.907  3.12  1.96     -7.02      5.20 0.000405
#>  2 s(x1)   NA    NA         -0.906  3.11  1.96     -7.01      5.20 0.0105  
#>  3 s(x1)   NA    NA         -0.898  3.10  1.96     -6.97      5.17 0.0205  
#>  4 s(x1)   NA    NA         -0.880  3.06  1.96     -6.88      5.12 0.0306  
#>  5 s(x1)   NA    NA         -0.849  3.00  1.96     -6.73      5.03 0.0406  
#>  6 s(x1)   NA    NA         -0.803  2.92  1.96     -6.52      4.92 0.0507  
#>  7 s(x1)   NA    NA         -0.740  2.81  1.96     -6.25      4.77 0.0607  
#>  8 s(x1)   NA    NA         -0.659  2.69  1.96     -5.93      4.61 0.0708  
#>  9 s(x1)   NA    NA         -0.557  2.56  1.96     -5.57      4.46 0.0809  
#> 10 s(x1)   NA    NA         -0.436  2.42  1.96     -5.19      4.32 0.0909  
#> # i 90 more rows
## or via a partial match
derivatives(mod, type = "central", select = "x1", partial_match = TRUE)
#> # A tibble: 100 x 9
#>    .smooth .by   .fs   .derivative   .se .crit .lower_ci .upper_ci       x1
#>    <chr>   <chr> <chr>       <dbl> <dbl> <dbl>     <dbl>     <dbl>    <dbl>
#>  1 s(x1)   NA    NA         -0.907  3.12  1.96     -7.02      5.20 0.000405
#>  2 s(x1)   NA    NA         -0.906  3.11  1.96     -7.01      5.20 0.0105  
#>  3 s(x1)   NA    NA         -0.898  3.10  1.96     -6.97      5.17 0.0205  
#>  4 s(x1)   NA    NA         -0.880  3.06  1.96     -6.88      5.12 0.0306  
#>  5 s(x1)   NA    NA         -0.849  3.00  1.96     -6.73      5.03 0.0406  
#>  6 s(x1)   NA    NA         -0.803  2.92  1.96     -6.52      4.92 0.0507  
#>  7 s(x1)   NA    NA         -0.740  2.81  1.96     -6.25      4.77 0.0607  
#>  8 s(x1)   NA    NA         -0.659  2.69  1.96     -5.93      4.61 0.0708  
#>  9 s(x1)   NA    NA         -0.557  2.56  1.96     -5.57      4.46 0.0809  
#> 10 s(x1)   NA    NA         -0.436  2.42  1.96     -5.19      4.32 0.0909  
#> # i 90 more rows
```
