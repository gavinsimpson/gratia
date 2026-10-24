# Generating responses with mgcv family objects

[`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
accepts instantiated standard and mgcv family objects. The example
template supplies covariates and true functions; `predictors` supplies
the linear predictors; `parameters` supplies generating values that are
not themselves modelled by a predictor. The `"truth"` attribute records
all predictors, distribution parameters and complete responses.

## Scalar families

The parameter names below belong inside `parameters = list(...)`.
Parameters are scalar unless explicitly described as observation-level.
Beta draws use the constructor's `eps` endpoint adjustment, matching
mgcv's generator. This adjustment can matter when the beta law places
substantial probability very close to zero or one; `eps` is recorded in
the truth.

|  |  |
|----|----|
| Family | Generating parameters |
| [`gaussian()`](https://rdrr.io/r/stats/family.html) | `dispersion = 1`, the non-negative variance |
| [`poisson()`](https://rdrr.io/r/stats/family.html), `cpois()` | None |
| [`binomial()`](https://rdrr.io/r/stats/family.html) | `trials = 1`, positive integer trial counts |
| [`Gamma()`](https://rdrr.io/r/stats/family.html), [`inverse.gaussian()`](https://rdrr.io/r/stats/family.html) | `dispersion = 1` |
| `nb()`, `negbin()` | `theta`, positive size; `Inf` gives the Poisson limit |
| `tw()`, `Tweedie()` | `power` in (1, 2), and `dispersion = 1` |
| `betar()` | Positive `precision` |
| `scat()` | `df` above the family's `min.df`, and positive t scale `sigma` |
| `ocat()` | Finite increasing `cuts`, one fewer than the categories |
| `ziP()` | `intercept` and natural-scale `slope` above the constructor's `b` |
| `cnorm()`, `clog()` | Positive `sigma`, Gaussian SD or logistic scale |
| `bcg()` | `lambda = 0` and positive log-response SD `sigma` |

Dispersion and trials can have length one or `n`. All other
scalar-family parameters are constant across observations. Fixed
constructor parameters are used when available; conflicting explicit
parameters are errors. Otherwise the corresponding generating values are
required, including cut points for `ocat(R = ...)`. For example, both
`family = mgcv::nb(theta = 3)` and
`family = mgcv::nb(), parameters = list(theta = 3)` specify size 3.
Constructor starting values for an estimated parameter are not used as
generating truth. A size range in `negbin(c(1, 10))` also requires an
explicit generating `theta`.

For `ziP()`, `lambda = exp(eta)` and
`P(Y > 0) = 1 - exp(-exp(intercept + slope * eta))`. Positive
observations follow a Poisson distribution conditioned on being
positive. The natural `slope` includes the constructor's lower bound
`b`; the second entry of `ziP(theta = ...)` is instead a log excess
above `b`. For fixed ordinal spacings, mgcv fixes the first cut at -1;
for example `ocat(theta = c(1, 5))` supplies cuts `c(-1, 0, 5)`. For
free cuts whose first value is `a`, mgcv's fixed first cut of -1
represents the same probabilities after shifting the predictor by
`-(a + 1)`. Include the corresponding component intercept in grouped
fitting recipes.

Quasi families do not define a complete sampling distribution and are
rejected. Nonzero Box-Cox powers are currently rejected: some Gaussian
transformed responses would lie outside the inverse transformation's
domain.

## Families with several linear predictors

The columns returned by `predictors` follow mgcv's order and links,
including their constructor-specific bounds. These are link-scale
predictors, not the `mu` arguments of mgcv's random-number generators.

|  |  |
|----|----|
| Family | Predictor order and natural parameters recorded in truth |
| `gaulss()` | Mean, SD predictor; records `mu`, `sigma` |
| `gammals()` | Log mean, log-dispersion predictor; records `mu`, `dispersion` |
| `gumbls()` | Location, log-scale predictor; records `location`, `mu` (mean), `sigma` |
| `gevlss()` | Location, log scale, shape predictor; records `location`, `sigma`, `shape` |
| `shash()` | Location, scale predictor, skewness, log tail; records `location`, `sigma`, `skewness`, `tail` |
| `twlss()` | Mean predictor, power predictor, log dispersion; records `mu`, `power`, `dispersion` |
| `ziplss()` | Log Poisson intensity, complementary log-log presence predictor; records `lambda`, `presence` |
| `multinom(K)` | K logits relative to category 0; records category `probabilities` |
| `mvn(d)` | d response means; `parameters$covariance` defaults to the identity matrix |

These families need no scalar generating parameters except `mvn()`'s
optional positive definite covariance matrix. In `shash()`, the
transformed normal is multiplied by `tail * sigma`, following mgcv's
parameterisation. In `gumbls()`, the mean is location plus Euler's
constant times `sigma`; the first predictor specifies the location. For
`gevlss()`, the default shape link restricts shape to (-1, 0.5); the
identity link allows any finite shape. In `ziplss()`, positive counts
follow a zero-truncated Poisson distribution.

## Default predictors

Let `g` be the template's `f` before distribution transformations.
Scalar Gaussian, t, Gaussian-censoring, logistic-censoring and Box-Cox
families use natural location `g`. Binomial and beta means are
`plogis((g - 5) / 5)`; ordinal location is `g - mean(g)`; the `ziP()`
predictor is `g / 5`. Other scalar means are `exp(g / 5)`. Each mean is
transformed with the supplied family's link. If it is outside that
link's domain, supply custom predictors rather than relying on a
default.

For general families, default predictors are: `(g, 0)` for `gaulss()`
and `gumbls()`; `(g / 5, 0)` for `gammals()` and `ziplss()`;
`(g, 0, linkfun(0))` for `gevlss()` (zero shape under the supplied shape
link); `(g, 0, 0, 0)` for `shash()`; and `(g / 5, 0, 0)` for `twlss()`.
Multivariate normal means repeat `g`. Multinomial logit j is
`(g - mean(g)) / 5 * j / K`. The `sc_location_scale` template instead
supplies its own log mean and log dispersion for `gammals()`.

## Censoring and survival

For `cnorm()`, `clog()`, `cpois()` and `bcg()`, `observation` can
contain finite `left` and `right` detection limits and a positive
`interval_width`. Responses outside detection limits are left or right
censored. Other responses are placed in rounding intervals wholly inside
those limits; values in intervals crossing a limit are retained exactly.
Omitting `observation` returns complete responses.

Matrix responses use equal entries for exact observations,
`(limit, -Inf)` for left censoring, `(limit, Inf)` for right censoring,
and `(lower, upper)` for intervals. `bcg()` instead uses zero in the
second entry for left censoring; its limits must be positive. Poisson
detection limits must be non-integer, and interval widths must be
integer numbers of counts. Its intervals use half-integer endpoints to
avoid ambiguity at count values.

`cox.ph()` uses Weibull cumulative baseline hazard
`H0(t) = (t / baseline_scale)^baseline_shape`, with both baseline
parameters defaulting to 1. Event times have survival
`exp(-H0(t) * exp(eta))`. Independent exponential censoring has
`censor_rate = 0.2`; zero disables censoring. Default `eta` is
`(g - mean(g)) / 5`. Fit using `weights = status`. True event and
censoring times are retained in the truth parameter list.

## Grouped families

Supply `family = mgcv::gfam(list(...))`, optionally with
`parameters = list(components = list(...), index = ...)`. `components`
contains one parameter list per family-list position. `index` is one
integer position per row (a scalar selects the same family for every
row). Without `index`, every component is represented at least once and
remaining positions are sampled uniformly; `n` must be at least the
number of families. Repeated family types retain separate indices and
parameters.

The default shared predictor is `g / 5`; custom predictors return one
column. Scalar response families are supported; general families are
rejected. Censored-family components return complete, uncensored
responses. Fit using `cbind(y, index)` as the response and
`weights = trials` if a component is binomial with more than one trial.
Row-level dispersion and trial vectors in component parameter lists
align with the full generated data, or can have the component's row
count. Truth parameters record each component's rows.

## See also

[`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md),
[data_sim_shapes](https://gavinsimpson.github.io/gratia/reference/data_sim_shapes.md),
[`mgcv::family.mgcv()`](https://rdrr.io/pkg/mgcv/man/family.mgcv.html)

## Examples

``` r
# A beta response: precision is explicitly chosen, rather than estimated.
d <- data_sim(family = mgcv::betar(theta = 12), seed = 2)
attr(d, "truth")$parameters$precision
#> [1] 12

# Binomial proportions: use trials as the fitting weights.
d <- data_sim(family = binomial(), parameters = list(trials = 10), seed = 2)

# Mean and standard deviation have separate predictors.
d <- data_sim("gwf2", family = mgcv::gaulss(), seed = 2,
  predictors = function(d) cbind(d$f, log(0.2 + d$x)))

# Fit a censored normal model to the matrix response.
d <- data_sim("sc_increasing", family = mgcv::cnorm(theta = 0.2), seed = 2,
  observation = list(left = 0.1, right = 0.9))
# \donttest{
m <- mgcv::gam(y ~ s(x), family = mgcv::cnorm(), data = d, method = "REML")
# }
```
