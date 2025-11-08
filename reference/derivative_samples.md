# Posterior expectations of derivatives from an estimated model

Posterior expectations of derivatives from an estimated model

## Usage

``` r
derivative_samples(object, ...)

# Default S3 method
derivative_samples(object, ...)

# S3 method for class 'gamm'
derivative_samples(object, ...)

# S3 method for class 'gam'
derivative_samples(
  object,
  focal = NULL,
  data = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n = 100,
  eps = 1e-07,
  n_sim = 10000,
  level = lifecycle::deprecated(),
  seed = NULL,
  envir = environment(formula(object)),
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  ...
)

# S3 method for class 'scam'
derivative_samples(
  object,
  focal = NULL,
  data = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n = 100,
  eps = 1e-07,
  n_sim = 10000,
  seed = NULL,
  envir = environment(formula(object)),
  draws = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  ...
)
```

## Arguments

- object:

  an R object to compute derivatives for

- ...:

  arguments passed to other methods and on to
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)

- focal:

  character; name of the focal variable. The response derivative of the
  response with respect to this variable will be returned. All other
  variables involved in the model will be held at constant values. This
  can be missing if supplying `data`, in which case, the focal variable
  will be identified as the one variable that is not constant.

- data:

  a data frame containing the values of the model covariates at which to
  evaluate the first derivatives of the smooths. If supplied, all but
  one variable must be held at a constant value.

- order:

  numeric; the order of derivative.

- type:

  character; the type of finite difference used. One of `"forward"`,
  `"backward"`, or `"central"`.

- scale:

  character; should the derivative be estimated on the response or the
  linear predictor (link) scale? One of `"response"` (the default), or
  `"linear predictor"`.

- method:

  character; which method should be used to draw samples from the
  posterior distribution. `"gaussian"` uses a Gaussian (Laplace)
  approximation to the posterior. `"mh"` uses a Metropolis Hastings
  sample that alternates t proposals with proposals based on a shrunken
  version of the posterior covariance matrix. `"inla"` uses a variant of
  Integrated Nested Laplace Approximation due to Wood (2019), (currently
  not implemented). `"user"` allows for user-supplied posterior draws
  (currently not implemented).

- n:

  numeric; the number of points to evaluate the derivative at (if `data`
  is not supplied).

- eps:

  numeric; the finite difference.

- n_sim:

  integer; the number of simulations used in computing the simultaneous
  intervals.

- level:

  **\[deprecated\]**

- seed:

  numeric; a random seed for the simulations.

- envir:

  the environment within which to recreate the data used to fit
  `object`.

- draws:

  matrix; user supplied posterior draws to be used when
  `method = "user"`.

- mvn_method:

  character; one of `"mvnfast"` or `"mgcv"`. The default is uses
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html), which
  can be considerably faster at generate large numbers of MVN random
  values than [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html),
  but which might not work for some marginal fits, such as those where
  the covariance matrix is close to singular.

## Value

A tibble, currently with the following variables:

- `.derivative`: the estimated partial derivative,

- additional columns containing the covariate values at which the
  derivative was evaluated.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", dist = "negbin", scale = 0.25, seed = 42)

# fit the GAM (note: for execution time reasons using bam())
m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df, family = nb(), method = "fREML")

# data slice through data along x2 - all other covariates will be set to
# typical values (value closest to median)
ds <- data_slice(m, x2 = evenly(x2, n = 200))

# samples from posterior of derivatives
fd_samp <- derivative_samples(m,
  data = ds, type = "central",
  focal = "x2", eps = 0.01, seed = 21, n_sim = 100
)

# plot the first 20 posterior draws
if (requireNamespace("ggplot2") && requireNamespace("dplyr")) {
  library("ggplot2")
  fd_samp |>
    dplyr::filter(.draw <= 20) |>
    ggplot(aes(x = x2, y = .derivative, group = .draw)) +
    geom_line(alpha = 0.5)
}
```
