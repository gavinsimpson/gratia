# A tidy basis representation of a smooth object

Takes an object of class `mgcv.smooth` and returns a tidy representation
of the basis.

## Usage

``` r
tidy_basis(smooth, data = NULL, at = NULL, coefs = NULL, p_ident = NULL)
```

## Arguments

- smooth:

  a smooth object of or inheriting from class `"mgcv.smooth"`.
  Typically, such objects are returned as part of a fitted GAM or GAMM
  in the `$smooth` component of the model object or the `$gam$smooth`
  component if the model was fitted by
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) or
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html).

- data:

  a data frame containing the variables used in `smooth`.

- at:

  a data frame containing values of the smooth covariate(s) at which the
  basis should be evaluated.

- coefs:

  numeric; an optional vector of coefficients for the smooth

- p_ident:

  logical vector; only used for handling
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html) smooths.

## Value

A tibble.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 400, seed = 42)

# fit model
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

# tidy representaition of a basis for a smooth definition
# extract the smooth
sm <- get_smooth(m, "s(x2)")
# get the tidy basis - need to pass where we want it to be evaluated
bf <- tidy_basis(sm, at = df)

# can weight the basis by the model coefficients for this smooth
bf <- tidy_basis(sm, at = df, coefs = smooth_coefs(sm, model = m))
```
