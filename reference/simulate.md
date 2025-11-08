# Simulate from the posterior distribution of a GAM

Simulations from the posterior distribution of a fitted GAM model
involve computing predicted values for the observation data for which
simulated data are required, then generating random draws from the
probability distribution used when fitting the model.

## Usage

``` r
# S3 method for class 'gam'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL
)

# S3 method for class 'gamm'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL
)

# S3 method for class 'scam'
simulate(
  object,
  nsim = 1,
  seed = NULL,
  data = newdata,
  weights = NULL,
  ...,
  newdata = NULL
)
```

## Arguments

- object:

  a fitted GAM, typically the result of a call to
  [mgcv::gam](https://rdrr.io/pkg/mgcv/man/gam.html)\` or
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html).

- nsim:

  numeric; the number of posterior simulations to return.

- seed:

  numeric; a random seed for the simulations.

- data:

  data frame; new observations at which the posterior draws from the
  model should be evaluated. If not supplied, the data used to fit the
  model will be used for `newdata`, if available in `object`.

- weights:

  numeric; a vector of prior weights. If `newdata` is null then defaults
  to `object[["prior.weights"]]`, otherwise a vector of ones.

- ...:

  arguments passed to methods. `simulate.gam()` and `simulate.scam()`
  pass `...` on to `predict.gam()`. As such you can pass additional
  arguments such as `terms`, `exclude`, to select which model terms are
  included in the predictions. This may be useful, for example, for
  excluding the effects of random effect terms.

- newdata:

  Deprecated. Use `data` instead.

## Value

(Currently) A data frame with `nsim` columns.

## Details

For `simulate.gam()` to function, the `family` component of the fitted
model must contain, or be updatable to contain, the required random
number generator. See
[`mgcv::fix.family.rd()`](https://rdrr.io/pkg/mgcv/man/fix.family.link.html).

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 2)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

sims <- simulate(m1, nsim = 5, seed = 42)
head(sims)
#>       sim_1     sim_2     sim_3     sim_4     sim_5
#> 1 11.445470 11.374304 10.098681  7.264881  8.796630
#> 2  6.510912  5.909584  9.057362  7.698084 11.444781
#> 3  3.837995  3.230610  3.550240  3.759380  4.774581
#> 4 12.361830 11.209226 10.714215 11.861957 10.746417
#> 5 14.851461 12.911440 11.356984 15.783913 15.106270
#> 6  5.921276  4.158963  5.520856  7.973614  9.654888
```
