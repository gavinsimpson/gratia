# Point-wise and simultaneous confidence intervals for derivatives of smooths

Calculates point-wise confidence or simultaneous intervals for the first
derivatives of smooth terms in a fitted GAM.

## Usage

``` r
# S3 method for class 'fderiv'
confint(
  object,
  parm,
  level = 0.95,
  type = c("confidence", "simultaneous"),
  nsim = 10000,
  ncores = 1L,
  ...
)
```

## Arguments

- object:

  an object of class `"fderiv"` containing the estimated derivatives.

- parm:

  which parameters (smooth terms) are to be given intervals as a vector
  of terms. If missing, all parameters are considered.

- level:

  numeric, `0 < level < 1`; the confidence level of the point-wise or
  simultaneous interval. The default is `0.95` for a 95% interval.

- type:

  character; the type of interval to compute. One of `"confidence"` for
  point-wise intervals, or `"simultaneous"` for simultaneous intervals.

- nsim:

  integer; the number of simulations used in computing the simultaneous
  intervals.

- ncores:

  number of cores for generating random variables from a multivariate
  normal distribution. Passed to
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html).
  Parallelization will take place only if OpenMP is supported (but
  appears to work on Windows with current `R`).

- ...:

  additional arguments for methods

## Value

a data frame with components:

1.  `term`; factor indicating to which term each row relates,

2.  `lower`; lower limit of the confidence or simultaneous interval,

3.  `est`; estimated derivative

4.  `upper`; upper limit of the confidence or simultaneous interval.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 1000, dist = "normal", scale = 2, seed = 2)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

# new data to evaluate the derivatives at, say over the middle 50% of range
# of each covariate
middle <- function(x, n = 25, coverage = 0.5) {
  v <- (1 - coverage) / 2
  q <- quantile(x, prob = c(0 + v, 1 - v), type = 8)
  seq(q[1], q[2], length = n)
}
new_data <- sapply(dat[c("x0", "x1", "x2", "x3")], middle)
new_data <- data.frame(new_data)
## first derivatives of all smooths...
fd <- fderiv(mod, newdata = new_data)
#> Warning: `fderiv()` was deprecated in gratia 0.7.0.
#> i Please use `derivatives()` instead.

## point-wise interval
ci <- confint(fd, type = "confidence")
ci
#> # A tibble: 100 x 4
#>    term    lower   est upper
#>    <chr>   <dbl> <dbl> <dbl>
#>  1 s(x0)  1.7     4.1    6.6
#>  2 s(x0)  1.3     3.8    6.3
#>  3 s(x0)  0.99    3.5    6.0
#>  4 s(x0)  0.68    3.1    5.6
#>  5 s(x0)  0.37    2.8    5.2
#>  6 s(x0)  0.0049  2.4    4.8
#>  7 s(x0) -0.40    2.0    4.5
#>  8 s(x0) -0.79    1.7    4.2
#>  9 s(x0) -1.1     1.3    3.8
#> 10 s(x0) -1.4     0.99   3.4
#> # i 90 more rows

## simultaneous interval for smooth term of x2
x2_sint <- confint(fd,
  parm = "x2", type = "simultaneous",
  nsim = 10000, ncores = 2
)
# \donttest{
x2_sint
#> # A tibble: 25 x 4
#>    term  lower   est upper
#>    <chr> <dbl> <dbl> <dbl>
#>  1 s(x2)  -24.  -15.  -5.6
#>  2 s(x2)  -35.  -26. -16. 
#>  3 s(x2)  -41.  -33. -24. 
#>  4 s(x2)  -44.  -36. -29. 
#>  5 s(x2)  -44.  -36. -28. 
#>  6 s(x2)  -42.  -34. -25. 
#>  7 s(x2)  -38.  -30. -21. 
#>  8 s(x2)  -33.  -24. -16. 
#>  9 s(x2)  -27.  -19. -11. 
#> 10 s(x2)  -22.  -14.  -5.8
#> # i 15 more rows
# }
```
