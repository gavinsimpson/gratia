# Evaluate smooths at covariate values

Evaluate a smooth at a grid of evenly spaced value over the range of the
covariate associated with the smooth. Alternatively, a set of points at
which the smooth should be evaluated can be supplied.
`smooth_estimates()` is a new implementation of
[`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md),
and replaces that function, which has been removed from the package.

## Usage

``` r
smooth_estimates(object, ...)

# S3 method for class 'gam'
smooth_estimates(
  object,
  select = NULL,
  smooth = deprecated(),
  n = 100,
  n_3d = 16,
  n_4d = 4,
  data = NULL,
  unconditional = FALSE,
  overall_uncertainty = TRUE,
  dist = NULL,
  unnest = TRUE,
  partial_match = FALSE,
  clip = FALSE,
  ...
)
```

## Arguments

- object:

  an object of class `"gam"` or `"gamm"`.

- ...:

  arguments passed to other methods.

- select:

  character; select which smooth's posterior to draw from. The default
  (`NULL`) means the posteriors of all smooths in `model` will be
  sampled from. If supplied, a character vector of requested terms.

- smooth:

  **\[deprecated\]** Use `select` instead.

- n:

  numeric; the number of points over the range of the covariate at which
  to evaluate the smooth.

- n_3d, n_4d:

  numeric; the number of points over the range of last covariate in a 3D
  or 4D smooth. The default is `NULL` which achieves the standard
  behaviour of using `n` points over the range of all covariate,
  resulting in `n^d` evaluation points, where `d` is the dimension of
  the smooth. For `d > 2` this can result in very many evaluation points
  and slow performance. For smooths of `d > 4`, the value of `n_4d` will
  be used for all dimensions `> 4`, unless this is `NULL`, in which case
  the default behaviour (using `n` for all dimensions) will be observed.

- data:

  a data frame of covariate values at which to evaluate the smooth.

- unconditional:

  logical; should confidence intervals include the uncertainty due to
  smoothness selection? If `TRUE`, the corrected Bayesian covariance
  matrix will be used.

- overall_uncertainty:

  logical; should the uncertainty in the model constant term be included
  in the standard error of the evaluate values of the smooth?

- dist:

  numeric; if greater than 0, this is used to determine when a location
  is too far from data to be plotted when plotting 2-D smooths. The data
  are scaled into the unit square before deciding what to exclude, and
  `dist` is a distance within the unit square. See
  [`mgcv::exclude.too.far()`](https://rdrr.io/pkg/mgcv/man/exclude.too.far.html)
  for further details.

- unnest:

  logical; unnest the smooth objects?

- partial_match:

  logical; in the case of character `select`, should `select` match
  partially against `smooths`? If `partial_match = TRUE`, `select` must
  only be a single string, a character vector of length 1.

- clip:

  logical; should evaluation points be clipped to the boundary of a soap
  film smooth? The default is `FALSE`, which will return `NA` for any
  point that is deemed to lie outside the boundary of the soap film.

## Value

A data frame (tibble), which is of class `"smooth_estimates"`.

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

## evaluate all smooths
smooth_estimates(m1)
#> # A tibble: 400 x 9
#>    .smooth .type .by   .estimate      .se         x0    x1    x2    x3
#>    <chr>   <chr> <chr>     <dbl>    <dbl>      <dbl> <dbl> <dbl> <dbl>
#>  1 s(x0)   TPRS  NA    -0.966542 0.316118 0.00710904    NA    NA    NA
#>  2 s(x0)   TPRS  NA    -0.925391 0.297170 0.0171157     NA    NA    NA
#>  3 s(x0)   TPRS  NA    -0.884233 0.279256 0.0271224     NA    NA    NA
#>  4 s(x0)   TPRS  NA    -0.843050 0.262594 0.0371291     NA    NA    NA
#>  5 s(x0)   TPRS  NA    -0.801824 0.247376 0.0471358     NA    NA    NA
#>  6 s(x0)   TPRS  NA    -0.760536 0.233728 0.0571425     NA    NA    NA
#>  7 s(x0)   TPRS  NA    -0.719175 0.221701 0.0671492     NA    NA    NA
#>  8 s(x0)   TPRS  NA    -0.677736 0.211261 0.0771559     NA    NA    NA
#>  9 s(x0)   TPRS  NA    -0.636220 0.202303 0.0871626     NA    NA    NA
#> 10 s(x0)   TPRS  NA    -0.594641 0.194685 0.0971693     NA    NA    NA
#> # i 390 more rows

## or selected smooths
smooth_estimates(m1, select = c("s(x0)", "s(x1)"))
#> # A tibble: 200 x 7
#>    .smooth .type .by   .estimate      .se         x0    x1
#>    <chr>   <chr> <chr>     <dbl>    <dbl>      <dbl> <dbl>
#>  1 s(x0)   TPRS  NA    -0.966542 0.316118 0.00710904    NA
#>  2 s(x0)   TPRS  NA    -0.925391 0.297170 0.0171157     NA
#>  3 s(x0)   TPRS  NA    -0.884233 0.279256 0.0271224     NA
#>  4 s(x0)   TPRS  NA    -0.843050 0.262594 0.0371291     NA
#>  5 s(x0)   TPRS  NA    -0.801824 0.247376 0.0471358     NA
#>  6 s(x0)   TPRS  NA    -0.760536 0.233728 0.0571425     NA
#>  7 s(x0)   TPRS  NA    -0.719175 0.221701 0.0671492     NA
#>  8 s(x0)   TPRS  NA    -0.677736 0.211261 0.0771559     NA
#>  9 s(x0)   TPRS  NA    -0.636220 0.202303 0.0871626     NA
#> 10 s(x0)   TPRS  NA    -0.594641 0.194685 0.0971693     NA
#> # i 190 more rows

# parallel processing of smooths
if (requireNamespace("mirai") && requireNamespace("carrier")) {
  library("mirai")
  daemons(2)                          # only low for CRAN requirements
  smooth_estimates(m1)
}
#> Loading required namespace: carrier
#> # A tibble: 400 x 9
#>    .smooth .type .by   .estimate      .se         x0    x1    x2    x3
#>    <chr>   <chr> <chr>     <dbl>    <dbl>      <dbl> <dbl> <dbl> <dbl>
#>  1 s(x0)   TPRS  NA    -0.966542 0.316118 0.00710904    NA    NA    NA
#>  2 s(x0)   TPRS  NA    -0.925391 0.297170 0.0171157     NA    NA    NA
#>  3 s(x0)   TPRS  NA    -0.884233 0.279256 0.0271224     NA    NA    NA
#>  4 s(x0)   TPRS  NA    -0.843050 0.262594 0.0371291     NA    NA    NA
#>  5 s(x0)   TPRS  NA    -0.801824 0.247376 0.0471358     NA    NA    NA
#>  6 s(x0)   TPRS  NA    -0.760536 0.233728 0.0571425     NA    NA    NA
#>  7 s(x0)   TPRS  NA    -0.719175 0.221701 0.0671492     NA    NA    NA
#>  8 s(x0)   TPRS  NA    -0.677736 0.211261 0.0771559     NA    NA    NA
#>  9 s(x0)   TPRS  NA    -0.636220 0.202303 0.0871626     NA    NA    NA
#> 10 s(x0)   TPRS  NA    -0.594641 0.194685 0.0971693     NA    NA    NA
#> # i 390 more rows
```
