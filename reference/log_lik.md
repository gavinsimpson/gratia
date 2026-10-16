# Posterior draws of pointwise log likelihoods

Evaluate each fitting observation at draws from the coefficient
posterior. This leaves the scalar
[`stats::logLik()`](https://rdrr.io/r/stats/logLik.html) method supplied
by mgcv unchanged.

## Usage

``` r
log_lik(model, ...)

# S3 method for class 'gam'
log_lik(
  model,
  ...,
  n_draws = NULL,
  chains = NULL,
  diagnostics = FALSE,
  seed = NULL,
  method = c("gaussian", "mh", "user"),
  draws = NULL,
  unconditional = FALSE
)

# S3 method for class 'bam'
log_lik(
  model,
  ...,
  n_draws = NULL,
  chains = NULL,
  diagnostics = FALSE,
  seed = NULL,
  method = c("gaussian", "mh", "user"),
  draws = NULL,
  unconditional = FALSE
)
```

## Arguments

- model:

  A fitted independent-response `gam` or `bam` model.

- ...:

  Additional arguments to
  [`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md),
  such as `burnin`, `thin` or `mvn_method`. Prediction at new
  observations and frequentist covariance matrices are not supported.

- n_draws:

  Positive integer; total coefficient draws across chains. Defaults to
  4000 when generating draws. Supplied draws determine their own count;
  an explicit count must agree.

- chains:

  positive integer or `NULL`; number of chains. `NULL` uses one chain,
  or the chain mapping in supplied draws. `n_draws` must be divisible by
  `chains`. Burn-in is applied separately to each MH chain.

- diagnostics:

  logical; calculate coefficient R-hat, effective sample sizes, and
  Monte Carlo standard errors using the optional posterior package.

- seed:

  Optional random seed. The caller's random-number state is retained.

- method:

  One of `"gaussian"`, `"mh"` or `"user"`; see
  [`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md).

- draws:

  A numeric matrix with one row per draw and one column per coefficient,
  in fitted coefficient order. Used with `method = "user"`.

- unconditional:

  Include the smoothing-parameter covariance correction when available,
  using
  [`mgcv::vcov.gam()`](https://rdrr.io/pkg/mgcv/man/vcov.gam.html).
  Defaults to `FALSE`.

## Value

A numeric matrix with draws in rows and retained fitting observations in
columns. Attribute `observation` gives their original fitting-row
indices after any fitting subset; `gratia` describes the sampling
assumptions. The `sampling` attribute retains chain identities and
requested coefficient diagnostics;
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
converts the likelihood matrix by chain. Impossible observations may
have log likelihood `-Inf`; invalid numerical calculations produce an
error. LOO and WAIC require finite log likelihoods.

## Details

The result contains log densities or log probability masses, including
their normalising constants and fitting weights. Smoothing penalties and
priors are excluded. Likelihood weights need not have the same
interpretation as the weights used by
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
to generate responses.

Ordinary and extended response families, distributional GAMs,
multinomial, multivariate-normal and grouped families are supported. A
multivariate-normal observation contributes one joint log density; its
precision coefficients vary with each draw. Grouped observations use
their fitted component family. Custom families can supply the `log_lik`
hook in
[family_diagnostics](https://gavinsimpson.github.io/gratia/reference/family_diagnostics.md).
Quasi, censored and Cox families, residual-correlated BAMs and full
mixed models are not supported. Only retained fitting observations are
evaluated.

Gaussian draws approximate the coefficient posterior. Parameters
represented by coefficients vary jointly, including distributional-model
scale and shape predictors. Other scale, shape and smoothing parameters
remain at fitted values. Even with `unconditional = TRUE`, those
additional parameters are not jointly sampled. Ordinary PSIS-LOO uses
these draws without correcting their Gaussian approximation. Pareto
diagnostics assess reweighting, not the accuracy of the Gaussian
approximation. See
[loo.gam](https://gavinsimpson.github.io/gratia/reference/loo.gam.md)
for optional loo methods.

## See also

[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
[family_diagnostics](https://gavinsimpson.github.io/gratia/reference/family_diagnostics.md)

## Examples

``` r
library("mgcv")
m <- gam(mpg ~ s(wt, k = 5), data = mtcars, method = "REML")
ll <- log_lik(m, n_draws = 500, seed = 42)
dim(ll)
#> [1] 500  32
if (requireNamespace("loo", quietly = TRUE)) {
  loo::loo(ll, r_eff = 1)
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
