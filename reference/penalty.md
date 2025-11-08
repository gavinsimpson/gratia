# Extract and tidy penalty matrices

Extract and tidy penalty matrices

## Usage

``` r
penalty(object, ...)

# Default S3 method
penalty(
  object,
  rescale = FALSE,
  data,
  knots = NULL,
  constraints = FALSE,
  diagonalize = FALSE,
  ...
)

# S3 method for class 'gam'
penalty(
  object,
  select = NULL,
  smooth = deprecated(),
  rescale = FALSE,
  partial_match = FALSE,
  ...
)

# S3 method for class 'mgcv.smooth'
penalty(object, rescale = FALSE, ...)

# S3 method for class 'tensor.smooth'
penalty(object, margins = FALSE, ...)

# S3 method for class 't2.smooth'
penalty(object, margins = FALSE, ...)

# S3 method for class 're.smooth.spec'
penalty(object, data, ...)
```

## Arguments

- object:

  a fitted GAM or a smooth.

- ...:

  additional arguments passed to methods.

- rescale:

  logical; by default, *mgcv* will scale the penalty matrix for better
  performance in
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html). If `rescale`
  is `TRUE`, this scaling will be undone to put the penalty matrix back
  on the original scale.

- data:

  data frame; a data frame of values for terms mentioned in the smooth
  specification.

- knots:

  a list or data frame with named components containing knots locations.
  Names must match the covariates for which the basis is required. See
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html).

- constraints:

  logical; should identifiability constraints be applied to the smooth
  basis. See argument `absorb.cons` in
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html).

- diagonalize:

  logical; if `TRUE`, reparameterises the smooth such that the
  associated penalty is an identity matrix. This has the effect of
  turning the last diagonal elements of the penalty to zero, which
  highlights the penalty null space.

- select:

  character, logical, or numeric; which smooths to extract penalties
  for. If `NULL`, the default, then penalties for all model smooths are
  drawn. Numeric `select` indexes the smooths in the order they are
  specified in the formula and stored in `object`. Character `select`
  matches the labels for smooths as shown for example in the output from
  `summary(object)`. Logical `select` operates as per numeric `select`
  in the order that smooths are stored.

- smooth:

  **\[deprecated\]** Use `select` instead.

- partial_match:

  logical; should smooths be selected by partial matches with `select`?
  If `TRUE`, `select` can only be a single string to match against.

- margins:

  logical; extract the penalty matrices for the tensor product or the
  marginal smooths of the tensor product?

## Value

A 'tibble' (data frame) of class `penalty_df` inheriting from `tbl_df`,
with the following components:

- `.smooth` - character; the label *mgcv* uses to refer to the smooth,

- `.type` - character; the type of smooth,

- `.penalty` - character; the label for the specific penalty. Some
  smooths have multiple penalty matrices, so the `penalty` component
  identifies the particular penalty matrix and uses the labelling that
  *mgcv* uses internally,

- `.row` - character; a label of the form `fn` where `n` is an integer
  for the `n`th basis function, referencing the columns of the penalty
  matrix,

- `.col` - character; a label of the form `fn` where `n` is an integer
  for the `n`th basis function, referencing the columns of the penalty
  matrix,

- `.value` - double; the value of the penalty matrix for the combination
  of `row` and `col`,

## Note

The [`print()`](https://rdrr.io/r/base/print.html) method uses
[`base::zapsmall()`](https://rdrr.io/r/base/zapsmall.html) to turn very
small numbers into 0s for display purposes only; the underlying values
of the penalty matrix or matrices are not changed.

For smooths that are subject to an eigendecomposition (e.g. the default
thin plate regression splines, `bs = "tp"`), the signs of the
eigenvectors are not defined and as such you can expect differences
across systems in the penalties for such smooths that are system-, OS-,
and CPU architecture- specific.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg4", n = 400, seed = 42)
m <- gam(
  y ~ s(x0, bs = "cr") + s(x1, bs = "cr") +
    s(x2, by = fac, bs = "cr"),
  data = dat, method = "REML"
)

# penalties for all smooths
penalty(m)
#> # A tibble: 405 x 6
#>    .smooth .type .penalty .row  .col   .value
#>    <chr>   <chr> <chr>    <chr> <chr>   <dbl>
#>  1 s(x0)   CRS   s(x0)    F1    F1     0.783 
#>  2 s(x0)   CRS   s(x0)    F1    F2    -0.635 
#>  3 s(x0)   CRS   s(x0)    F1    F3     0.265 
#>  4 s(x0)   CRS   s(x0)    F1    F4    -0.0203
#>  5 s(x0)   CRS   s(x0)    F1    F5     0.0441
#>  6 s(x0)   CRS   s(x0)    F1    F6     0.0378
#>  7 s(x0)   CRS   s(x0)    F1    F7     0.0482
#>  8 s(x0)   CRS   s(x0)    F1    F8     0.0216
#>  9 s(x0)   CRS   s(x0)    F1    F9     0.0247
#> 10 s(x0)   CRS   s(x0)    F2    F1    -0.635 
#> # i 395 more rows

# for a specific smooth
penalty(m, select = "s(x2):fac1")
#> # A tibble: 81 x 6
#>    .smooth    .type .penalty   .row  .col   .value
#>    <chr>      <chr> <chr>      <chr> <chr>   <dbl>
#>  1 s(x2):fac1 CRS   s(x2):fac1 F1    F1     1.66  
#>  2 s(x2):fac1 CRS   s(x2):fac1 F1    F2    -0.755 
#>  3 s(x2):fac1 CRS   s(x2):fac1 F1    F3     0.430 
#>  4 s(x2):fac1 CRS   s(x2):fac1 F1    F4     0.0846
#>  5 s(x2):fac1 CRS   s(x2):fac1 F1    F5     0.192 
#>  6 s(x2):fac1 CRS   s(x2):fac1 F1    F6     0.152 
#>  7 s(x2):fac1 CRS   s(x2):fac1 F1    F7     0.188 
#>  8 s(x2):fac1 CRS   s(x2):fac1 F1    F8     0.164 
#>  9 s(x2):fac1 CRS   s(x2):fac1 F1    F9     0.0597
#> 10 s(x2):fac1 CRS   s(x2):fac1 F2    F1    -0.755 
#> # i 71 more rows
```
