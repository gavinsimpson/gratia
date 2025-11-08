# Basis expansions for smooths

Basis expansions from a definition of a smoother using the syntax of
*mgcv*'s smooths via
[`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html).,
[`mgcv::te()`](https://rdrr.io/pkg/mgcv/man/te.html),
[`mgcv::ti()`](https://rdrr.io/pkg/mgcv/man/te.html), and
[`mgcv::t2()`](https://rdrr.io/pkg/mgcv/man/t2.html), or directly from a
fitted GAM(M).

## Usage

``` r
basis(object, ...)

# S3 method for class 'gam'
basis(
  object,
  select = NULL,
  term = deprecated(),
  data = NULL,
  n = 100,
  n_2d = 50,
  n_3d = 16,
  n_4d = 4,
  partial_match = FALSE,
  ...
)

# S3 method for class 'scam'
basis(
  object,
  select = NULL,
  term = deprecated(),
  data = NULL,
  n = 100,
  n_2d = 50,
  n_3d = 16,
  n_4d = 4,
  partial_match = FALSE,
  ...
)

# S3 method for class 'gamm'
basis(
  object,
  select = NULL,
  term = deprecated(),
  data = NULL,
  n = 100,
  n_2d = 50,
  n_3d = 16,
  n_4d = 4,
  partial_match = FALSE,
  ...
)

# S3 method for class 'gamm4'
basis(
  object,
  select = NULL,
  term = deprecated(),
  data = NULL,
  n = 100,
  n_2d = 50,
  n_3d = 16,
  n_4d = 4,
  partial_match = FALSE,
  ...
)

# Default S3 method
basis(
  object,
  data,
  knots = NULL,
  constraints = FALSE,
  at = NULL,
  diagonalize = FALSE,
  coefficients = NULL,
  ...
)
```

## Arguments

- object:

  a smooth specification, the result of a call to one of
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html).,
  [`mgcv::te()`](https://rdrr.io/pkg/mgcv/man/te.html),
  [`mgcv::ti()`](https://rdrr.io/pkg/mgcv/man/te.html), or
  [`mgcv::t2()`](https://rdrr.io/pkg/mgcv/man/t2.html), or a fitted
  GAM(M) model.

- ...:

  other arguments passed to
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html).

- select:

  character; select smooths in a fitted model

- term:

  **\[deprecated\]** This argument has been renamed `select`

- data:

  a data frame containing the variables used in `smooth`.

- n:

  numeric; the number of points over the range of the covariate at which
  to evaluate the smooth.

- n_2d:

  numeric; the number of new observations for each dimension of a
  bivariate smooth. Not currently used; `n` is used for both dimensions.

- n_3d:

  numeric; the number of new observations to generate for the third
  dimension of a 3D smooth.

- n_4d:

  numeric; the number of new observations to generate for the dimensions
  higher than 2 (!) of a *k*D smooth (*k* \>= 4). For example, if the
  smooth is a 4D smooth, each of dimensions 3 and 4 will get `n_4d` new
  observations.

- partial_match:

  logical; in the case of character `select`, should `select` match
  partially against `smooths`? If `partial_match = TRUE`, `select` must
  only be a single string, a character vector of length 1.

- knots:

  a list or data frame with named components containing knots locations.
  Names must match the covariates for which the basis is required. See
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html).

- constraints:

  logical; should identifiability constraints be applied to the smooth
  basis. See argument `absorb.cons` in
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html).

- at:

  a data frame containing values of the smooth covariate(s) at which the
  basis should be evaluated.

- diagonalize:

  logical; if `TRUE`, reparameterises the smooth such that the
  associated penalty is an identity matrix. This has the effect of
  turning the last diagonal elements of the penalty to zero, which
  highlights the penalty null space.

- coefficients:

  numeric; vector of values for the coefficients of the basis functions.

## Value

A tibble.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg4", n = 400, seed = 42)

bf <- basis(s(x0), data = df)
bf <- basis(s(x2, by = fac, bs = "bs"), data = df, constraints = TRUE)
```
