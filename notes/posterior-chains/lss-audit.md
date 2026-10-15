# Location-scale-shape sampling audit

Audited against installed mgcv 1.9-4. Its exported LSS constructors are `gaulss`,
`gammals`, `gumbls`, `gevlss`, `shash`, `twlss` and `ziplss`. The audit inspected
the constructors' `linfo`, `predict`, `postproc`, `ll` and `rd` functions and
existing gratia adapters in `fix-family.R`, `family-utils.R`,
`response-distributions.R` and `diagnostic-core.R`.

## Distribution parameters and generators

Here eta denotes a predictor including its offset. Parameters below are the
ones passed to the response generator, rather than necessarily inverse links.
Non-default links use the fitted `linfo` functions and their fitted bounds.

| Family | Predictors and response parameters | Expected response | Generator |
|:-------|:-----------------------------------|:------------------|:----------|
| `gaulss` | Mean and inverse SD. Default inverse SD is `1 / (exp(eta2) + b)` | Mean | Native normal generator uses SD `sqrt(scale / weight) / inverse_SD` |
| `gammals` | First predictor is log mean, despite its identity link; exponentiate it. Second inverse link gives log dispersion | `exp(eta1)` | Native Gamma generator, shape `1 / exp(log_phi)`, scale `mean * exp(log_phi)` |
| `gumbls` | Location and log scale from predictors. Native `predict()` returns location, but stored fitted values and `rd()` require mean and log scale | `location + Euler_constant * exp(log_scale)` | Native Gumbel generator subtracts the Euler correction from its mean argument |
| `gevlss` | Location, log scale, shape | `location + scale * (Gamma(1 - shape) - 1) / shape`, continuous limit at zero shape | Native GEV inverse distribution generator |
| `shash` | Location, log scale, skewness, log tail parameter | Mean of `location + delta * sigma * sinh((asinh(Z) + skewness) / delta)` for standard normal Z | Native sinh-arcsinh generator |
| `twlss` | Mean, unconstrained power predictor, log dispersion | Mean | Existing gratia adapter calls `rtw()` after transforming power and dispersion |
| `ziplss` | Log Poisson intensity and complementary-log-log presence predictor; both `linfo` links are identity | Presence probability times the mean of a positive-truncated Poisson | Native hurdle Poisson generator |

For `twlss`, predictor 2 controls power and predictor 3 controls dispersion in
the installed likelihood, post-processing and existing gratia adapter. Some
manual prose reverses these; the implementation follows the fitted family.
For `ziplss`, native `predict()` returns only the mean, so response generation
must instead retain both raw parameters. For `gammals`, accepting only inverse
links would fail to exponentiate the log mean. For `gumbls`, accepting native
prediction output directly as generator input would shift the distribution.

Existing gratia mean adapters already cover GEV, sinh-arcsinh and hurdle means;
existing `fix_family_rd()` supplies the missing Tweedie LSS generator. These
are reused. Native generators are retained, including their weight semantics:
`gaulss` uses explicit sampling weights; its likelihood ignores fitting weights,
so gratia retains unit response weights by default, matching existing diagnostic
and simulation adapters. The other six native/adapted LSS generators ignore
weights. Explicit weights are passed to the generator and do not change
coefficient draws. Custom family hooks are preserved; the Gumbel correction
recognises the native mgcv prediction hook before adjusting its convention.

## Shared prediction and sampling

`sampling-lss.R` prepares one model matrix, its predictor-to-coefficient map and
all predictor offsets per call. Every row and predictor uses the same joint
coefficient draw. It retains shared coefficient memberships in the fitted
matrix's `lpi` attribute. Fitting offsets are recovered from stored predictors;
formula offsets are evaluated at supplied data. No observed response is needed.
Missing predictor rows are removed for calculation and restored at the boundary,
without consuming random numbers for those rows. Explicit weights follow the
original requested rows, including under `na.omit` and `na.exclude`.

`posterior_samples()` generates responses after drawing coefficients.
`fitted_samples(scale = "response")` returns E[Y]; the historical
`.parameter = "location"` column is retained for compatibility.
`fitted_samples(scale = "linear_predictor")` returns the first predictor.
`derivative_samples()` differentiates the corresponding quantity using the same
joint draws at all finite-difference points. It therefore includes uncertainty
in scale, shape and presence where these affect the expectation.
`smooth_samples()` returns each selected component on its own predictor scale;
it does not transform individual components into expected responses.
`predicted_samples()` uses the same distribution parameters with fixed fitted
coefficients. Its `.row` values now retain original supplied-data identities
under `na.omit`. The `add_*()` functions join these row identities and retain
chain information and coefficient diagnostics where applicable.

Coefficient drawing does not require evaluation data. LSS prediction consumes
all coefficient columns; `user_draws()` selection remains available for
coefficient analysis. Generated Gaussian marginal selections retain their
existing seeded behaviour. A selected coefficient subset is not sufficient for
response or mean prediction and is rejected by supplied-draw validation.

## Remaining restrictions and their source

* GEV means do not exist for shape >= 1. Fitted response and response-derivative
  sampling give an informative error; location-predictor and response sampling
  are still possible. The default fitted shape link stays below this limit.
  Other supported LSS means exist at finite valid parameters; numerical overflow
  in evaluating an expectation gives a separate error.
* Installed mgcv rejects offsets when fitting `shash`, and discards call-level
  `offset` arguments for general-family fits. gratia retains offsets actually
  present in stored predictors, and evaluates supported formula offsets at new
  data. It cannot recover an offset that the fitter never applied.
* Installed `mgcv::bam()` explicitly rejects `general.family` models, including
  these seven LSS families. Their prediction support applies to `gam` fits;
  existing ordinary-family `bam` sampling is unchanged.
* The grouped-family posterior response sampler still uses fitting rows. Its
  fitted-mean and fixed-coefficient prediction paths already support supplied
  data with explicit family membership; this LSS extension does not change that.
* `multinom` and `mvn` have separate existing multivariate output paths. They are
  not scalar LSS models. Those paths and their coefficient mappings are retained.
  Diagnostic multivariate-normal draws retain their existing covariance sampling.
* Cox survival families have no response generator for this interface. Survival
  probabilities at specified times and strata are not expected survival times.
  Their fitted-response and posterior-response errors state what is missing.
* Censored `cnorm`, `clog` and `cpois` generators produce latent uncensored
  responses. An observed censoring mechanism is additional information, so
  posterior predictive checks continue to reject those models without it.
* Correlated AR1 response draws still require fitting rows and an available
  joint response generator. This extension does not infer dependence for new rows.

## Independent validation

`test-sampling-lss.R` constructs independent predictor and expectation formulas
for all seven families, including numerical integration of the sinh-arcsinh
expectation. It checks fitting rows, supplied subsets and genuinely new
covariates, multiple predictor offsets where mgcv supports them, prior and
explicit weights, missing values, row identities, supplied and generated draws,
chains and diagnostics, derivatives, selected smooths and joins. Response tests
check analytic means, normal variance and hurdle zero probabilities with fixed
seeds and six Monte Carlo standard errors for mean tolerances. Separate exact
normal draws check reciprocal SD and sampling weights. Existing diagnostic
tests compare raw and streamed posterior draws.

Primary documentation: [mgcv family overview](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/family.mgcv.html),
[gaulss](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gaulss.html),
[gammals](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gammals.html),
[gumbls](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gumbls.html),
[gevlss](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gevlss.html),
[shash](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/shash.html),
[twlss](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/twlss.html),
[ziplss](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/ziplss.html).
