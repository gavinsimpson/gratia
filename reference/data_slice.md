# Prepare a data slice through model covariates

Prepare a data slice through model covariates

## Usage

``` r
data_slice(object, ...)

# Default S3 method
data_slice(object, ...)

# S3 method for class 'data.frame'
data_slice(object, ..., .observed_only = FALSE)

# S3 method for class 'gam'
data_slice(object, ..., data = NULL, envir = NULL, .observed_only = FALSE)

# S3 method for class 'gamm'
data_slice(object, ...)

# S3 method for class 'list'
data_slice(object, ...)

# S3 method for class 'scam'
data_slice(object, ...)
```

## Arguments

- object:

  an R model object.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  User supplied variables defining the data slice. Arguments passed via
  `...` need to be *named*.

- .observed_only:

  logical or character; should the data slice be trimmed to those
  combinations of the variables specified that are observed in `object`.
  If `TRUE`, the observed combinations of variables mentioned in `...`
  are matched against those in `object` and filtered to return only
  those combinations. If `FALSE`, no filtering is done. If
  `.observed_only` is a character vector, on those variables named in
  the vector are used to in the comparison with the combinations in
  `object`.

- data:

  an alternative data frame of values containing all the variables
  needed to fit the model. If `NULL`, the default, the data used to fit
  the model will be recovered using `model.frame`. User-supplied
  expressions passed in `...` will be evaluated in `data`.

- envir:

  the environment within which to recreate the data used to fit
  `object`.

## Details

A data slice is the data set that results where one (or more covariates)
is varied systematically over some or all of its (their) range or at a
specified subset of values of interest, while any remaining covariates
in the model are held at fixed, representative values. This is known as
a *reference grid* in package **emmeans** and a *data grid* in the
**marginaleffects** package.

For GAMs, any covariates not specified via `...` will take
representative values determined from the data used to fit the model as
follows:

- for numeric covariates, the value in the fitting data that is closest
  to the median value is used,

- for factor covariates, the modal (most frequently observed) level is
  used, or the first level (sorted as per the vector returned by
  [`base::levels()`](https://rdrr.io/r/base/levels.html) if several
  levels are observed the same number of times.

These values are already computed when calling `gam()` or `bam()` for
example and can be found in the `var.summary` component of the fitted
model. Function
[`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
will extract these values for you if you are interested.

Convenience functions
[`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md),
[`ref_level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md),
and
[`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
are provided to help users specify data slices.
[`ref_level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md),
and
[`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
also ensure that factor covariates have the correct levels, as needed by
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
for example.

For an extended discussion of `data_slice()` and further examples, see
`vignette("data-slices", package = "gratia")`.

## See also

The convenience functions
[`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md),
[`ref_level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md),
and
[`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md).
[`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
for extracting the representative values used for covariates in the
model but not named in the slice.

## Examples

``` r
load_mgcv()

# simulate some Gaussian data
df <- data_sim("eg1", n = 50, seed = 2)

# fit a GAM with 1 smooth and 1 linear term
m <- gam(y ~ s(x2, k = 7) + x1, data = df, method = "REML")

# Want to predict over f(x2) while holding `x1` at some value.
# Default will use the observation closest to the median for unspecified
# variables.
ds <- data_slice(m, x2 = evenly(x2, n = 50))
ds
#> # A tibble: 50 x 2
#>        x2    x1
#>     <dbl> <dbl>
#>  1 0.0228 0.403
#>  2 0.0424 0.403
#>  3 0.0619 0.403
#>  4 0.0815 0.403
#>  5 0.101  0.403
#>  6 0.121  0.403
#>  7 0.140  0.403
#>  8 0.160  0.403
#>  9 0.179  0.403
#> 10 0.199  0.403
#> # i 40 more rows

# for full control, specify the values you want
ds <- data_slice(m, x2 = evenly(x2, n = 50), x1 = 0.3)

# or provide an expression (function call) which will be evaluated in the
# data frame passed to `data` or `model.frame(object)`
ds <- data_slice(m, x2 = evenly(x2, n = 50), x1 = mean(x1))
```
