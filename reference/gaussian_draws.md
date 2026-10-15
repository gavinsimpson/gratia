# Posterior samples using a Gaussian approximation to the posterior distribution

Posterior samples using a Gaussian approximation to the posterior
distribution

## Usage

``` r
# S3 method for class 'gamm4'
gaussian_draws(model, ...)

gaussian_draws(model, ...)

# S3 method for class 'gam'
gaussian_draws(
  model,
  n,
  n_cores = 1L,
  index = NULL,
  frequentist = FALSE,
  unconditional = FALSE,
  mvn_method = "mvnfast",
  ...
)

# S3 method for class 'scam'
gaussian_draws(
  model,
  n,
  n_cores = 1L,
  index = NULL,
  frequentist = FALSE,
  parametrized = TRUE,
  mvn_method = "mvnfast",
  ...,
  unconditional = FALSE
)
```

## Arguments

- model:

  a fitted model from
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html), or
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html), or a
  model inheriting from these classes. See
  [gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
  for sampling from gamm4 fits.

- ...:

  arguments passed to methods.

- n:

  numeric; the number of posterior draws to take.

- n_cores:

  integer; number of CPU cores to use when generating multivariate
  normal distributed random values. Only used if
  `mvn_method = "mvnfast"` **and** `method = "gaussian"`.

- index:

  numeric; vector of indices of coefficients to use. Can be used to
  subset the mean vector and covariance matrix extracted from `model`.

- frequentist:

  logical; if `TRUE`, the frequentist covariance matrix of the parameter
  estimates is used. If `FALSE`, the Bayesian posterior covariance
  matrix of the parameters is used. See
  [`mgcv::vcov.gam()`](https://rdrr.io/pkg/mgcv/man/vcov.gam.html).

- unconditional:

  logical; if `TRUE` the Bayesian smoothing parameter uncertainty
  corrected covariance matrix is used, *if available* for `model`. See
  [`mgcv::vcov.gam()`](https://rdrr.io/pkg/mgcv/man/vcov.gam.html). SCAM
  models do not support this correction; use `unconditional = FALSE`.

- mvn_method:

  character; one of `"mvnfast"` or `"mgcv"`. The default is uses
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html), which
  can be considerably faster at generate large numbers of MVN random
  values than [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html),
  but which might not work for some marginal fits, such as those where
  the covariance matrix is close to singular.

- parametrized:

  logical; use parametrized coefficients and covariance matrix, which
  respect the linear inequality constraints of the model. Only for
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html) model fits.

## Details

GAM-class fits with linear inequality constraints, including
[`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) fits, store
these as `Ain %*% coefficients >= bin`. Gaussian sampling uses the
fitted covariance matrix without enforcing these inequalities and issues
a warning. Sampled smooths can therefore violate the fitted shape
constraints. Pointwise intervals computed from the same Gaussian
covariance approximation also need not preserve those shapes.
Metropolis-Hastings sampling is unavailable for these fits because
[`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html) does not
enforce their inequalities.

If [`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) was fitted
with `bs > 0`, its `bs` component stores coefficient draws from the
native constrained bootstrap in columns. These can be supplied
explicitly as `draws = t(model$bs)` with `method = "user"`. The result
then describes bootstrap variation, rather than Gaussian posterior
sampling. Gratia does not automatically select these draws. The caller
is responsible for the interpretation and constraint feasibility of
user-supplied draws.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
