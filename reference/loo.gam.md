# Leave-one-out validation and WAIC for GAMs

These methods evaluate the likelihood of each fitting observation at
posterior coefficient draws and return the usual objects from loo.
Neither method changes
[`stats::logLik()`](https://rdrr.io/r/stats/logLik.html) for fitted
GAMs.

## Usage

``` r
loo.gam(
  x,
  n = 4000,
  seed = NULL,
  sampling_args = list(),
  r_eff = NULL,
  chain_id = NULL,
  save_psis = FALSE,
  cores = 1,
  ...
)

loo.bam(
  x,
  n = 4000,
  seed = NULL,
  sampling_args = list(),
  r_eff = NULL,
  chain_id = NULL,
  save_psis = FALSE,
  cores = 1,
  ...
)

waic.gam(x, n = 4000, seed = NULL, sampling_args = list(), ...)

waic.bam(x, n = 4000, seed = NULL, sampling_args = list(), ...)
```

## Arguments

- x:

  a GAM or BAM fitted by mgcv.

- n:

  number of posterior coefficient draws.

- seed:

  random seed for posterior sampling.

- sampling_args:

  named list of arguments passed to the coefficient sampler; see
  [`log_lik()`](https://gavinsimpson.github.io/gratia/reference/log_lik.md).

- r_eff:

  optional relative effective sample sizes, either one value or one per
  observation. Independent Gaussian draws use `1` by default. For
  Metropolis–Hastings draws this is estimated from the likelihoods.
  Supplied draws require `r_eff` or `chain_id`.

- chain_id:

  integer chain identifier for each coefficient draw, in draw order.
  Identifiers must be consecutive starting at one, and chains must
  contain equal numbers of draws. MH sampling defaults to one chain.

- save_psis:

  retain the PSIS object in the returned LOO result.

- cores:

  number of cores used by loo.

- ...:

  additional arguments passed to
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) or
  [`loo::waic()`](https://mc-stan.org/loo/reference/waic.html).

## Value

A native `loo` or `waic` object, with attributes `gratia` describing
posterior sampling assumptions and `observation` identifying fitting
rows.

## Details

The Gaussian posterior approximation and fitted values of additional
family parameters are described in
[`log_lik()`](https://gavinsimpson.github.io/gratia/reference/log_lik.md).
PSIS diagnostics assess the importance weights; they do not measure the
accuracy of that approximation. With random-effect smooths, leaving out
individual observations predicts further observations in the fitted
groups, rather than new groups.

Likelihoods are evaluated one observation at a time using the function
interface in loo. Saving PSIS weights still requires storage
proportional to the number of observations times the number of draws.

## Examples

``` r
if (requireNamespace("loo", quietly = TRUE)) {
  library("mgcv")
  m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
  loo::loo(m, n = 500, seed = 42)
}
#> 
#> Computed from 500 by 32 log-likelihood matrix.
#> 
#>          Estimate  SE
#> elpd_loo    -78.5 3.8
#> p_loo         2.9 0.8
#> looic       156.9 7.5
#> ------
#> MCSE of elpd_loo is 0.1.
#> MCSE and ESS estimates assume independent draws (r_eff=1).
#> 
#> All Pareto k estimates are good (k < 0.63).
#> See help('pareto-k-diagnostic') for details.
```
