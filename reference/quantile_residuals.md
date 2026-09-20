# Randomised residuals

Randomised residuals

## Usage

``` r
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# S3 method for class 'gam'
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)

# S3 method for class 'glm'
quantile_residuals(model, type = c("pit", "quantile"), seed = NULL, ...)
```

## Arguments

- model:

  a fitted model object.

- type:

  character; which type of randomised residual to return

- seed:

  integer; the random seed to use when generating randomised residuals.
  Can be missing, in which case the current state residuals are computed
  using the current state of the random number generator.

- ...:

  arguments passed to other methods.

## Details

With `na.exclude`, excluded observations are restored as `NA` residuals;
with `na.omit`, only model-used observations are returned.

For [`mgcv::cnorm()`](https://rdrr.io/pkg/mgcv/man/cnorm.html),
[`mgcv::clog()`](https://rdrr.io/pkg/mgcv/man/clog.html), and
[`mgcv::cpois()`](https://rdrr.io/pkg/mgcv/man/cpois.html) models,
censored observations have PIT residuals sampled uniformly between
`F(l)` and `F(u)`, where `l` and `u` bound the censoring interval and
`F` is the fitted latent response CDF. Left and right censoring use
probabilities zero and one, respectively, for the unbounded end.
Quantile residuals apply
[`qnorm()`](https://rdrr.io/r/stats/Normal.html) to these PIT values.
Uncensored continuous observations use `F(y)`; uncensored Poisson
observations are randomized between `F(y - 1)` and `F(y)`. Use
non-integer censoring limits for `cpois()`, as recommended by mgcv.

For CDF helpers with native log-tail support, quantile residuals are
computed directly from log probabilities in the smaller tail. This
avoids infinite residuals caused by rounding a probability to zero or
one. No probability clipping is applied: genuine zero-probability tails
still give infinite residuals. PIT residuals are returned as ordinary
probabilities and may still round to zero or one in extreme tails, so
applying [`qnorm()`](https://rdrr.io/r/stats/Normal.html) to the
returned PIT values can be less accurate than requesting quantile
residuals directly. Native log tails are available for Poisson, negative
binomial, binomial, Gaussian, Gamma, `cnorm()`, `clog()`, `gaulss()`,
`gammals()`, `scat()`, and `betar()` families. Other CDF helpers retain
ordinary-probability evaluation.
