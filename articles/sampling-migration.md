# Migrating posterior sampling calls

The multiple-chain release gives sampling arguments the same meaning
across
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
[`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md),
[`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md),
their `add_*()` helpers, and coefficient draw functions. Obsolete
arguments now produce errors with migration guidance. Controls must be
named; only the model, or the data and model in `add_*()` helpers, can
be passed by position. This prevents old positional calls from changing
meaning.

## Draw counts and evaluation points

`n_draws` always means the total retained draws across all chains. It
must be divisible by `chains`. Generating draws requires an explicit
count: the previous defaults ranged from one draw to 10,000 draws.
`chains = NULL` uses one chain, or infers the chains from supplied
coefficient draws.

| Previous argument | Replacement |
|:---|:---|
| `n` in fitted, posterior, predicted, smooth and coefficient sampling | `n_draws` |
| `n_sim` in derivative sampling | `n_draws` |
| `n` in derivative sampling | `n_grid` |
| `n_vals` in smooth sampling | `n_grid` |
| `object` in derivative sampling | `model` |
| `object` in `add_*()` sampling helpers | `data` |
| `freq` | `frequentist` |
| `newdata` | `data` |
| `ncores` | `n_cores` for MH chains; `n_threads` for Gaussian draws |
| `n_cores` for Gaussian sampling | `n_threads` |
| `term` in smooth sampling | `select` |

For example, the previous derivative call

``` r

# Previous interface; now produces an informative error.
derivative_samples(model, focal = "x", n = 100, n_sim = 1000)
```

becomes

``` r

# 1,000 retained coefficient draws, evaluated at 100 points.
derivative_samples(model, focal = "x", n_draws = 1000, n_grid = 100)
```

`n_grid = 200` is the common default for smooth and derivative sampling.
For smooths, it supplies the resolution to
[`smooth_data()`](https://gavinsimpson.github.io/gratia/reference/smooth_data.md);
multidimensional smooths can contain more rows than `n_grid`. Derivative
sampling constructs a slice along `focal`, holding other variables at
representative values. Explicit `data` determine the evaluation
locations instead of `n_grid`. Use explicit `data` when the exact grid
matters. Fitted and response sampling use fitting observations when
`data = NULL` and do not construct a grid.

## Multiple chains and parallel work

``` r

samples <- fitted_samples(
  model, n_draws = 1000, chains = 4, n_cores = 4,
  method = "mh", burnin = 1000, thin = 1,
  diagnostics = TRUE, seed = 281
)
diagnostics <- sampling_diagnostics(samples)
diagnostics
# These plots describe the coefficient chains used for the fitted samples.
draw(diagnostics, select = 1:2)
draw(diagnostics, type = "rank", select = 1:2)
```

This retains 250 draws per chain. Every chain discards its own 1,000
burn-in iterations; parallel chains repeat this warmup work. `thin = 2`
retains every second iteration after burn-in and still returns `n_draws`
draws in total. `n_cores` limits concurrent MH chains using mirai,
capped at `chains`. An existing worker pool is reused; otherwise
temporary workers are cleaned up automatically. Given the same seed and
sampler backend, retained draws agree between serial and parallel runs.

Gaussian sampling uses `n_threads` to control threads inside mvnfast. It
does not run concurrent MH chains. A control explicitly supplied to a
method that cannot use it produces an error, including MH controls for
Gaussian draws or Gaussian covariance controls for MH. `block_size` and
`dense_limit` apply only to `method = "mh", mh_backend = "gratia"`.

The default MH backend remains `"mgcv"`. The three options are:

| `mh_backend` | Evaluation | Model support |
|:---|:---|:---|
| `"mgcv"` | [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html) | Its existing `gam` support |
| `"gratia"` | Compiled likelihoods and MH loop | Gaussian identity, Poisson log, binomial logit, Gamma log, and negative binomial log `gam` and independent-observation `bam` fits |
| `"family"` | Compiled MH loop with callbacks to the fitted family | Compatible standard, extended, and general mgcv families, including independent-observation `bam` fits |

For example, a multinomial fit can use the family evaluator without a
separate compiled multinomial likelihood:

``` r

samples <- posterior_samples(
  model, n_draws = 1000, chains = 4, n_cores = 4,
  method = "mh", mh_backend = "family",
  burnin = 1000, diagnostics = TRUE, seed = 281
)
```

The family backend prepares its full design matrix, fitted smoothing
penalty, and proposal factorisation once and reuses them across chains.
It calls the fitted family’s `dev.resids()` and `linkinv()`, or `ll()`
for multiple predictors, preserving response attributes and coefficient
mappings. It retains the full design matrix even for `bam`: use the
gratia backend’s block evaluation when that matrix is too large and its
family is supported. Both evaluate bases at the original covariates for
discrete `bam` fits.

The gratia and family evaluators reject GAMM components and AR1
dependence; they do not evaluate fitted observation correlations. They
also reject `paraPen`, `H`, pre-fit `G` objects, and singular
coefficient covariances. The family backend additionally rejects quasi
families, which do not provide a likelihood, and Cox proportional
hazards models. All three MH backends condition on fitted smoothing,
dispersion, and any family parameters outside the coefficient vector.
Parameters represented by coefficients are sampled, including scale or
covariance coefficients in general families. `unconditional = TRUE`
applies to Gaussian sampling only.

## Reusing coefficient draws

``` r

betas <- post_draws(model, n_draws = 1000, chains = 4, seed = 281)
fitted <- fitted_samples(model, method = "user", draws = betas)
smooth <- smooth_samples(model, method = "user", draws = betas)
derivative <- derivative_samples(
  model, focal = "x", method = "user", draws = betas
)
```

The coefficient matrix determines the count and chain layout. Explicit
`n_draws` or `chains` must agree with it. A plain matrix without chain
metadata represents one chain; chain boundaries are never guessed from
the row count. Smooths always transform the same joint coefficient
draws. The old `rng_per_smooth` option is removed because independently
redrawing each smooth loses their posterior dependence. The previously
ignored `mu`, `sigma`, and derivative `level` arguments are also
rejected.

Multiple-chain tables retain `.chain`, `.iteration`, and globally unique
`.draw` columns. Single-chain tables keep their previous columns.
Attached diagnostics refer to coefficients before transformation or
response simulation.
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
converts coefficient matrices or sampled quantities for posterior or
coda diagnostics. Optional packages are required only when their formats
or diagnostics are requested.

`draw(sampling_diagnostics(x))` uses native gratia plotting code. Trace
plots show retained iteration within each chain; `type = "rank"`
compares histogram outlines of ranks pooled across chains, with a
reference line for equal counts per bin. Ranks use average ranks for
ties and no random jitter. Use `select` to choose coefficient names or
positions. Rank plots require at least two chains. Discarded burn-in is
unavailable, and thinning is represented by the retained iteration
count. Coefficient matrices provide their own draws; fitted, response,
smooth and derivative outputs require `diagnostics = TRUE` when sampling
to retain coefficient draws for these plots. Plotting itself needs
neither posterior nor bayesplot. Numerical summaries still use
posterior.

## Which response sampling function to use

[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
draws distribution parameters by default; use `quantity = "expectation"`
for expected responses.
[`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
draws individual smooth contributions, and
[`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
draws derivatives.
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
also simulates response variation for each coefficient draw.
[`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
simulates responses with fitted parameters held fixed. It uses `n_draws`
for the number of response replicates and rejects coefficient-chain
controls; use
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
to include coefficient uncertainty.

Model-specific capabilities still apply. For example, the native
diagnostic samplers for `lm` and mixed models support Gaussian
approximations and fitting observations only, and reject unsupported
chain controls. See
[`help("diagnostic_support")`](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md)
for model-specific support. Functions that summarise draws, such as
[`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
and
[`conditional_differences()`](https://gavinsimpson.github.io/gratia/reference/conditional_differences.md),
keep their existing grid and simulation-count arguments; their internal
sampling calls use this interface.

## Likelihoods and external diagnostic wrappers

[`log_lik()`](https://gavinsimpson.github.io/gratia/reference/log_lik.md),
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html),
[`loo::waic()`](https://mc-stan.org/loo/reference/waic.html) and
[`bayesplot::pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html)
use the same `n_draws` convention. Likelihood and LOO/WAIC wrappers
default to 4,000 draws; bayesplot uses 50 for ordinary checks and 4,000
for leave-one-out plots. Supplied coefficient matrices determine their
own counts. Pass `chains`, `diagnostics`, `method` and backend controls
directly to
[`log_lik()`](https://gavinsimpson.github.io/gratia/reference/log_lik.md),
or through `sampling_args` for LOO, WAIC and bayesplot. Likelihood
matrices and these diagnostic results retain the coefficient sampling
metadata. LOO reads stored chain iterations when estimating relative
efficiency, while keeping likelihood and response draws in the same
original order. Unlabelled supplied matrices still require explicit
`chain_id` or `r_eff` for LOO. The old `n` argument and explicit
controls for a different sampler produce errors.

## Location-scale-shape models

[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
now accepts evaluation data for
[`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html),
[`gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html),
[`gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html),
[`gevlss()`](https://rdrr.io/pkg/mgcv/man/gevlss.html),
[`shash()`](https://rdrr.io/pkg/mgcv/man/shash.html),
[`twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html) and
[`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html) fits. Supply
covariates and formula-offset variables; observed responses are
unnecessary. Every predictor uses the same joint coefficient draw. For
example:

``` r

# Evaluate the first 200 observations, retaining their joint parameter draws.
b <- post_draws(model, n_draws = 1000, chains = 4, seed = 281)
y <- posterior_samples(model, data = evaluation_data[1:200, ],
  method = "user", draws = b, seed = 282)
```

The functions return different quantities:

| Function | Quantity for an LSS model |
|:---|:---|
| `fitted_samples(scale = "response")` | All natural distribution parameters represented by predictors |
| `fitted_samples(scale = "linear_predictor")` | All parameter predictors, in family order |
| `fitted_samples(quantity = "expectation")` | Expected response, using every parameter on which it depends |
| [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md) | Responses conditional on each joint coefficient draw |
| [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md) | Responses conditional on the fixed fitted coefficients |
| `derivative_samples(scale = "response")` | Derivative of the expected response, retaining joint coefficient uncertainty |
| `derivative_samples(scale = "linear_predictor")` | Derivative of the first predictor |
| [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md) | Selected smooth contribution to its own predictor |

The `.parameter` column labels each parameter by its meaning on both
scales. Expectation output uses `.parameter = "expectation"`. Gumbel,
GEV and sinh-arcsinh means generally differ from location; the hurdle
Poisson mean uses both predictors and accounts for zero truncation. A
GEV mean does not exist at shape \>= 1; requesting that mean or its
derivative gives an error, while parameter and response draws remain
available.

[`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) represents its
second response parameter as inverse SD. Sampling uses its reciprocal
and divides SD by the square root of an explicit sampling weight. The
native [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) likelihood
ignores fitting weights; default response weights are ones, preserving
the fitted likelihood and existing diagnostic rules. The other LSS
response generators currently ignore sampling weights, as their mgcv
implementations do. Formula offsets are evaluated at supplied
covariates; stored fitting offsets are retained with `data = NULL`. The
installed mgcv version rejects offsets when fitting
[`shash()`](https://rdrr.io/pkg/mgcv/man/shash.html) and drops
call-level offsets for general-family fits; gratia cannot reconstruct
offsets never applied by the fitter. Prefer supported formula offsets.

## Grouped response families

[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
also accepts fitting-data subsets and new covariates for
[`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html) models. Every
row must identify its component family, either through the
response-index expression in the supplied data or through
`family_index`. Observed response values are not needed. For example:

``` r

posterior_samples(grouped_model, data = evaluation_data,
  family_index = component_index, n_draws = 1000, chains = 4, seed = 281)
```

The same joint coefficient draw is used for every component.
Component-specific links, fitted nuisance parameters and formula offsets
are retained. New-data sampling weights default to one; explicit weights
override them. Output retains `.parameter = "component1"`,
`"component2"`, and so on, together with `.row`, `.draw` and any chain
columns. Missing predictors, membership or sampling weights produce
missing responses. Fitting rows excluded by `na.exclude` are restored.
Smooth samples retain their predictor contributions, and response
derivatives use each row’s component mean. For derivatives, membership
must be recoverable from `data`;
[`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
does not accept `family_index`. Ordinal expectations and their
derivatives refer to category probabilities, identified by `.category`.
Scalar components have `NA` in that column. Censored grouped components
require an observation process for posterior response sampling; use
[`simulate()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
for latent uncensored draws.

MH coefficient sampling rejects BAM fits with nonzero prescribed smooth
contributions, such as a nonzero soap-film boundary. Native fitting and
prediction can use different offsets for those fits. Supplied
coefficient draws can still be evaluated with the native prediction
conventions.

## Censoring and survival responses

Censored-family generators return latent uncensored responses; posterior
predictive checks need a censoring observation process. Cox survival
models lack a response generator for this interface, and expected
survival times cannot be obtained from a location parameter alone.

## Distribution parameters and expected responses

[`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
and
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
now default to `quantity = "parameters"`. They return each parameter
represented by a predictor, including an intercept-only predictor, with
its natural meaning. Use `family_parameters(model)` to inspect the
names, definitions, predictor order and native representation. Global
parameters appear in that description but remain fixed during
coefficient sampling.

Use `quantity = "expectation"` to request the expected response.
Expectations are available only on the response scale. Use
`scale = "link"` with parameter output for the model predictors.
Gaussian LSS returns `mean` and `sd`; GEV returns `location`, `scale`
and `shape`. GEV location is generally not its expectation.
Zero-inflated Poisson returns `rate` and `presence_probability`; its
expectation includes the zero-truncation adjustment.

``` r

family_parameters(model)
fitted_samples(model, n_draws = 100, quantity = "parameters")
fitted_samples(model, n_draws = 100, quantity = "expectation")
fitted_values(model, quantity = "expectation")
```

For categorical models, expectation output contains category
probabilities. Categories and multivariate responses use `.category` and
`.y`, separately from `.parameter`. Expected responses use
`.parameter = "expectation"`.
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
draws new observations after drawing coefficients;
[`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
draws observations with fitted coefficients held fixed. Neither is a
substitute for draws of an expectation.

### Location, rate and expectation

The following models illustrate why the quantity must be explicit:

``` r

load_mgcv()
set.seed(426)
d <- data.frame(x = runif(200), y = rnorm(200))
gev_fit <- gam(list(y ~ x, ~1, ~1), data = d, family = gevlss())
family_parameters(gev_fit)
fitted_values(gev_fit) # location, scale and shape
fitted_values(gev_fit, quantity = "expectation")

d$y <- rpois(200, exp(1 + d$x))
zip_fit <- gam(list(y ~ x, ~1), data = d, family = ziplss())
fitted_samples(zip_fit, n_draws = 5) # rate and presence probability
fitted_samples(zip_fit, n_draws = 5, quantity = "expectation")

tw_fit <- gam(list(y ~ x, ~1, ~1), data = d, family = twlss())
fitted_values(tw_fit) # mean, power and dispersion
fitted_values(tw_fit, quantity = "expectation") # equals the mean parameter
```

For [`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html), writing rate
as lambda and presence probability as p, the expectation is
`p * lambda / (1 - exp(-lambda))`. The denominator accounts for the
zero-truncated Poisson distribution of positive counts. GEV location
generally differs from its expectation. Tweedie’s mean parameter is
already its expectation, but its power and dispersion are separate
parameters.
