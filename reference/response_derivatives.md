# Derivatives on the response scale from an estimated GAM

Derivatives on the response scale from an estimated GAM

## Usage

``` r
response_derivatives(object, ...)

# Default S3 method
response_derivatives(object, ...)

# S3 method for class 'gamm'
response_derivatives(object, ...)

# S3 method for class 'gam'
response_derivatives(
  object,
  focal = NULL,
  data = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n = 100,
  eps = NULL,
  n_sim = 10000,
  level = 0.95,
  seed = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  ...,
  uncertainty = c("simulation", "delta"),
  unconditional = FALSE
)

# S3 method for class 'scam'
response_derivatives(
  object,
  focal = NULL,
  data = NULL,
  order = 1L,
  type = c("forward", "backward", "central"),
  scale = c("response", "linear_predictor"),
  method = c("gaussian", "mh", "inla", "user"),
  n = 100,
  eps = NULL,
  n_sim = 10000,
  level = 0.95,
  seed = NULL,
  mvn_method = c("mvnfast", "mgcv"),
  ...,
  uncertainty = c("simulation", "delta"),
  unconditional = FALSE
)
```

## Arguments

- object:

  an R object to compute derivatives for.

- ...:

  arguments passed to posterior sampling or prediction, such as `draws`,
  `envir`, and `exclude`. `freq = TRUE` selects frequentist coefficient
  covariance for delta uncertainty or Gaussian draws. For delta
  uncertainty, `newdata`, `se.fit`, and `terms` are not supported; use
  `data` and `exclude`.

- focal:

  character; name of the focal variable. The response derivative of the
  response with respect to this variable will be returned. All other
  variables involved in the model will be held at constant values. This
  must be supplied.

- data:

  a data frame containing the values of the model covariates at which to
  evaluate derivatives of fitted values. If supplied, all but one
  variable must be held at a constant value.

- order:

  numeric; the order of derivative.

- type:

  character; the type of finite difference used. One of `"forward"`,
  `"backward"`, or `"central"`.

- scale:

  character; should the derivative be estimated on the response or the
  linear predictor (link) scale? One of `"response"` (the default), or
  `"linear_predictor"`.

- method:

  character; which method should be used to draw samples from the
  posterior distribution. `"gaussian"` uses a Gaussian (Laplace)
  approximation to the posterior. `"mh"` uses a Metropolis Hastings
  sample that alternates t proposals with proposals based on a shrunken
  version of the posterior covariance matrix. `"inla"` uses a variant of
  Integrated Nested Laplace Approximation due to Wood (2019), (currently
  not implemented). `"user"` allows for user-supplied posterior draws
  via `draws` in `...`.

- n:

  numeric; the number of points to evaluate the derivative at (if `data`
  is not supplied).

- eps:

  a positive finite number giving the absolute finite-difference step,
  or `NULL` (the default) to choose it automatically. The automatic step
  is the fitted range of the differentiation coordinate multiplied by
  `.Machine$double.eps^(1 / (order + p))`, where `p = 2` for central
  differences and `p = 1` for forward or backward differences. A
  constant coordinate uses its absolute value (or one if zero) instead
  of its range. The step is rounded upwards and bounded below so that
  adding it to the coordinates produces a representable change. Stored
  transformed values or raw covariate summaries supply the range;
  supplied `data` are used if neither is available. This is a
  scale-aware heuristic, not an error bound; unusual function scales or
  domain boundaries may require explicit `eps`. For first central
  differences the two points are separated by `eps`; for second central
  differences they are each `eps` from the target.

- n_sim:

  integer; number of posterior draws. Ignored when using user-supplied
  draws.

- level:

  numeric; `0 < level < 1`; the coverage level of the credible interval.
  The default is `0.95` for a 95% interval.

- seed:

  numeric; a random seed for the simulations.

- mvn_method:

  character; one of `"mvnfast"` or `"mgcv"`. The default is uses
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html), which
  can be considerably faster at generate large numbers of MVN random
  values than [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html),
  but which might not work for some marginal fits, such as those where
  the covariance matrix is close to singular.

- uncertainty:

  character; `"simulation"` (default) uses posterior draws and
  equal-tailed intervals; `"delta"` uses the delta method and normal
  intervals. Both methods return pointwise intervals. Sampler controls,
  including `n_sim`, `method`, and `seed`, are ignored for delta
  uncertainty.

- unconditional:

  logical; include smoothing-parameter uncertainty in the Bayesian
  covariance, if available, for delta intervals and Gaussian draws. This
  does not modify MH or user-supplied draws.

## Value

A tibble, currently with the following variables:

- `.row`: integer, indexing the row of `data` each row in the output
  represents

- `.focal`: the name of the variable for which the partial derivative
  was evaluated,

- `.derivative`: the posterior median for simulation uncertainty, or the
  finite difference of fitted predictions for delta uncertainty,

- `.se`: posterior standard deviation for simulation uncertainty (`NA`
  with only one draw), or the delta-method standard error,

- `.lower_ci`: the lower bound of the pointwise interval,

- `.upper_ci`: the upper bound of the pointwise interval,

- additional columns containing the covariate values at which the
  derivative was evaluated.

## Details

Delta uncertainty uses the same shifted prediction rows and
finite-difference formulas as simulation, for both first and second
derivatives. It applies those formulas to the coefficient gradients of
fitted means and propagates their full joint covariance. Formula offsets
are included in predictions. Intervals concern derivatives of the
conditional mean, not future observations. With Bayesian coefficient
covariance, delta intervals have an approximate posterior
interpretation. They are symmetric and may differ from simulation
intervals when posterior transformations are strongly nonlinear; the two
methods' point estimates can also differ.

Delta uncertainty supports single-linear-predictor `gam` and `bam`
models with an ordinary scalar inverse-link mean (including negative
binomial, Tweedie, beta regression, and scaled t families), and `gamm`
via its GAM component. It is not supported for `scam` or general
multi-predictor families; their existing simulation support is
unchanged. The `uncertainty` attribute records the selected method.

## Author

Gavin L. Simpson

## Examples

``` r

library("ggplot2")
library("patchwork")
load_mgcv()
df <- data_sim("eg1", dist = "negbin", scale = 0.25, seed = 42)

# fit the GAM (note: for execution time reasons using bam())
m <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
  data = df, family = nb(), method = "fREML"
)

# data slice through data along x2 - all other covariates will be set to
# typical values (value closest to median)
ds <- data_slice(m, x2 = evenly(x2, n = 100))

# fitted values along x2
fv <- fitted_values(m, data = ds)

# response derivatives - ideally n_sim = >10000
y_d <- response_derivatives(m,
  data = ds, type = "central", focal = "x2",
  eps = 0.01, seed = 21, n_sim = 1000
)

# draw fitted values along x2
p1 <- fv |>
  ggplot(aes(x = x2, y = .fitted)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, y = NULL),
    alpha = 0.2
  ) +
  geom_line() +
  labs(
    title = "Estimated count as a function of x2",
    y = "Estimated count"
  )

# pointwise normal intervals from the delta method
y_delta <- response_derivatives(m, data = ds, focal = "x2",
  type = "central", uncertainty = "delta")

# draw response derivatives
p2 <- y_d |>
  ggplot(aes(x = x2, y = .derivative)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  geom_line() +
  labs(
    title = "Estimated 1st derivative of estimated count",
    y = "First derivative"
  )

# draw both panels
p1 + p2 + plot_layout(nrow = 2)
```
