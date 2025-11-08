# First derivatives of fitted GAM functions

**\[deprecated\]**

This function was deprecated because it was limited to first order
forward finite differences for derivatives only, but couldn't be
improved to offer the needed functionality without breaking backwards
compatibility with papers and blog posts that already used `fderiv()`. A
replacement,
[`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md),
is now available and recommended for new analyses.

## Usage

``` r
fderiv(model, ...)

# S3 method for class 'gam'
fderiv(
  model,
  newdata,
  term,
  n = 200,
  eps = 1e-07,
  unconditional = FALSE,
  offset = NULL,
  ...
)

# S3 method for class 'gamm'
fderiv(model, ...)
```

## Arguments

- model:

  A fitted GAM. Currently only models fitted by
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) are
  supported.

- ...:

  Arguments that are passed to other methods.

- newdata:

  a data frame containing the values of the model covariates at which to
  evaluate the first derivatives of the smooths.

- term:

  character; vector of one or more terms for which derivatives are
  required. If missing, derivatives for all smooth terms will be
  returned.

- n:

  integer; if `newdata` is missing the original data can be
  reconstructed from `model` and then `n` controls the number of values
  over the range of each covariate with which to populate `newdata`.

- eps:

  numeric; the value of the finite difference used to approximate the
  first derivative.

- unconditional:

  logical; if `TRUE`, the smoothing parameter uncertainty corrected
  covariance matrix is used, *if available*, otherwise the uncorrected
  Bayesian posterior covariance matrix is used.

- offset:

  numeric; value of offset to use in generating predictions.

## Value

An object of class `"fderiv"` is returned.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", seed = 2)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

## first derivatives of all smooths...
fd <- fderiv(mod)
## now use -->
fd <- derivatives(mod)

## ...and a selected smooth
fd2 <- fderiv(mod, term = "x1")
## now use -->
fd2 <- derivatives(mod, select = "s(x1)")

## Models with factors
dat <- data_sim("eg4", n = 400, dist = "normal", scale = 2, seed = 2)
mod <- gam(y ~ s(x0) + s(x1) + fac, data = dat, method = "REML")

## first derivatives of all smooths...
fd <- fderiv(mod)
## now use -->
fd <- derivatives(mod)

## ...and a selected smooth
fd2 <- fderiv(mod, term = "x1")
## now use -->
fd2 <- derivatives(mod, select = "s(x1)")
```
