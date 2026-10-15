# Model diagnostic support and conventions

Diagnostics use each model's fitted response distribution, observation
order and choice of conditioning. The same calculations then produce
residuals, reference values and plots across fitting packages.

## Details

A conditional check holds fitted random effects fixed. A marginal check
averages over their fitted distribution. When responses share random
effects or residual correlation, they must be simulated together to
preserve that dependence; this is called joint response simulation
below. Whether responses are independent depends on the chosen
conditioning, not just the model class.

### Coverage

PIT means probability integral transform: the fitted cumulative
distribution function (CDF) maps each observation to a probability. At
discrete values, that probability is randomised over the corresponding
probability mass. Quantile residuals transform PIT values to
standard-normal scores. A posterior predictive check (PPC) compares
observed responses with simulated data sets that also include
approximate uncertainty in fitted parameters.

This table applies to the supported response families described below.

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Fitter | PIT/quantile | Uniform QQ | Simulated QQ | Scalable plots | PPC |
| lm/glm | Yes | Yes | Yes | Yes | Approximate coefficient draws |
| mgcv::gam | Yes | Depends on family | Yes | Yes | Coefficients drawn together |
| mgcv::bam | Marginal families | Marginal or joint | Gaussian AR1; otherwise marginal for GEE | Yes | Excludes GEE AR1 |
| gamm/nlme::lme | Conditional/marginal | Depends on independence | Gaussian; PQL limits | Yes | Gaussian; approximate PQL without residual correlation |
| gamm4/lme4::merMod | Conditional/marginal | Depends on independence | Joint, including GLMMs | Yes | Conditional/marginal |
| glmmTMB | Conditional/marginal | Depends on independence | Joint, including non-Gaussian | Yes | Conditional/marginal |
| GJRM::gamlss | Seven univariate families | Yes | Yes | Yes | Coefficients drawn together |
| sdmTMB | Depends on treatment of fields | Depends on independence | Joint, including delta | Yes | Parameters and fields drawn together |

- `lm`/`glm`: weighted Gaussian and supported response families, with
  their fitted dispersion. Quasi families do not specify a response
  distribution, so they do not support PIT/quantile residuals or PPCs.

- `gam`/`bam`: ordinary, distributional and grouped families, including
  weighted Gamma, inverse Gaussian, `twlss`, ordinal and `ziP`
  distributions. Multivariate-normal PITs check each response
  separately; simulations retain covariance between responses. Nominal
  multinomial models retain traditional residual checks and PPCs. PITs
  would require an ordering of their categories.

- SCAM models support ordinary posterior predictive plots. Scalable
  posterior predictive plots are not currently available; use
  `scalable = FALSE`. Smoothing-parameter uncertainty is not currently
  supported for SCAM models; use `unconditional = FALSE`.

- `gamm`/`gamm4` and direct `lme`/`merMod` fits use their full
  predictors. Gaussian nlme variance weights and residual covariance are
  retained. Non-Gaussian `gamm` fits use penalized quasi-likelihood
  (PQL). Without residual correlation, these support approximate
  response checks and PPCs based on their fitted working parameters. PQL
  residual correlation acts on the working response used for fitting; it
  does not specify a joint distribution for the observed responses.

- `glmmTMB`: conditional Gaussian, Poisson, binomial, NB1/NB2, beta,
  Gamma, Tweedie, lognormal and supported truncated distributions,
  including fitted zero inflation and dispersion. Marginal simulations
  also provide reference samples for other families that glmmTMB can
  simulate. Unsupported conditional family CDFs remain unavailable. The
  package's own joint simulator preserves temporal AR1, spatial
  covariance and correlated random slopes.

- Univariate [`GJRM::gamlss`](https://rdrr.io/pkg/GJRM/man/gamlss.html):
  uncensored N, LN, GA, GAi, P, NBI and NBII. Probabilities use the
  final likelihood parameters. This corrects stored NBII probabilities
  that use a Poisson CDF in some upstream versions. Other families and
  joint, censored and survival models are not yet supported.

- `sdmTMB`: supported single-response and delta families retain spatial,
  spatiotemporal, grouping, time-varying and smooth effects. Standard
  deltas and deltas with the coupled Poisson link support checks of the
  complete response, occurrence and positive component. sdmTMB's
  transformations from internal parameters to response parameters must
  be available. Student-t checks use the fitted location, scale and
  fixed or estimated degrees of freedom. Pearson residuals require
  degrees of freedom above two; PIT/quantile residuals and predictive
  checks remain available with infinite variance. Predictive plots also
  support native response families whose analytic residual distributions
  are unavailable.

### Response encodings and residual definitions

Factor-valued binomial responses are Bernoulli observations, even when
likelihood weights exceed one. Numeric proportions use the fitted trial
counts; paired success/failure counts retain their grouped-binomial
encoding. Default response simulations and CDFs follow that distinction.
Explicit GAM sampling weights can change trial counts or response
precision, including Gaussian AR1 precision, without changing fitted
coefficient uncertainty. MASS negative-binomial GLM families use their
fitted size parameter. The native gaulss likelihood ignores fitting
weights, so its default diagnostic references use the fitted inverse
standard deviation directly. Explicit sampling precision weights remain
available; see the sampling documentation for how these change the
response distribution.

Native beta-regression simulation clips draws at epsilon and one minus
epsilon. These endpoints therefore have probability mass. Diagnostics
include that mass in their CDFs and randomisation and use moments of the
same clipped distribution. This is labelled an approximation: the
fitting likelihood evaluates the underlying beta density at
epsilon-adjusted observations. Distribution functions supplied by
callers are preserved.

GEV and sinh-arcsinh checks use stable response means and correct
residual centering and signs for both observed and simulated responses.
Their deviance magnitudes follow the fitting package's definitions: GEV
uses the square root of its saturated log-likelihood contrast;
sinh-arcsinh uses zero log density as its reference. If a sinh-arcsinh
density exceeds one, that contrast is negative. Deviance checks then
give an error rather than replace the residual with zero; response and
PIT/quantile checks remain available. These deviance definitions do not
produce comparable standardised scores across families. Use PIT/quantile
residuals for uniform/standard-normal references.

GEV means and variances require shape below one and one-half,
respectively. Signed deviance checks also require a finite mean and
shape at least minus one. PIT/quantile checks do not require these
moments. When the response mean does not exist, appraisal explains why
the observed-versus-fitted panel is unavailable and retains the
PIT/quantile panels.

nlme calculations retain covariance between random effects, including
nested groups and block designs. They also retain the contrasts and
observation order used for fitting, including after subsetting or
missing-value removal. Changing global contrast options after fitting
does not change the diagnostics.

### Conditioning and dependence

For response references, `conditioning = "conditional"` usually holds
fitted random effects and nuisance parameters fixed. `"marginal"`
simulates new effects from their fitted distributions and averages over
them. Setting random effects to zero is not a substitute for this
averaging, especially with nonlinear response links. sdmTMB has
additional field choices described below. PPCs separately include
approximate uncertainty in fitted parameters.

`"gam_component"` explicitly uses the extracted GAM component. gamm and
gamm4 diagnostics and posterior predictive checks default to the full
model with conditional conditioning. gamm4 response simulations use the
same default; the existing gamm response simulator retains its
GAM-component default. Conditional response references hold fitted
effects fixed, while conditional PPCs include uncertainty in those
fitted effects. Specify `"marginal"` to simulate new effects instead.

Conditional glmmTMB/lme4 response likelihoods factorize given all random
effects, including temporal and spatial effects. Marginal responses may
be dependent because they share those effects. An independent effect for
each observation need not create dependence between observations. The
calculations check covariance implied by the fitted random-effect
design, including correlated slopes and covariance cancellation.
Gaussian nlme residual correlation can remain after conditioning on
random effects; variance weights alone do not introduce correlation.

A conditional check at estimated effects assesses the response
distribution at those estimates. It does not provide a calibrated test
of random-effect covariance or of the whole fitted model. Marginal joint
references assess response variation implied by new simulated effects. A
QQ plot checks the distribution of residuals; inspect their ordering or
spatial pattern as well to assess dependence. Models are not refitted
when calculating reference bands.

### BAM working correlation

Gaussian identity-link AR1 BAM retains joint response simulation, with
new independent series starting at `AR.start` boundaries. For
non-Gaussian or nonidentity models fitted with `discrete = TRUE` and
nonzero `rho`, BAM applies AR1 to working residuals as a generalized
estimating equations (GEE) approximation. This does not specify a unique
joint response distribution. The restriction applies to supported
`extended.family` and GLM families. See
<https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/bam.html>.

Fitted marginal CDFs, inverse CDFs and PIT/quantile residuals remain
useful. For traditional residuals, the default QQ plot and a requested
inverse-CDF QQ plot use `method = "marginal"`. This approximates the
pooled distribution of individual residuals by numerical sampling,
without asserting independence or supplying dependence bands. Explicit
`method = "simulate"`, joint response simulation and PPCs give an
explanatory error. No additional dependence model, such as a copula, is
introduced. An explicitly requested `"normal"` approximation is
available without independence-based bands; it is never the default
here.

[`residual_correlation()`](https://gavinsimpson.github.io/gratia/reference/residual_correlation.md)
and
[`residual_correlation_plot()`](https://gavinsimpson.github.io/gratia/reference/residual_correlation.md)
provide approximate checks of correlation between residuals at different
lags. For non-Gaussian BAM, the stored `std.rsd` residuals differ from
the weighted working residuals used in fitting. Adjusting working
residuals for the fitted AR1 correlation (whitening) or using `std.rsd`
can reveal remaining serial structure. Neither supplies a generative
response model or calibrated p-values. Series boundaries and gaps from
missing observations are retained, and no significance bands based on
independence are drawn. Lags count observation positions, not spatial
distance or irregular elapsed time. Specify series and order the fitting
data to suit the dependence being checked.

### sdmTMB field choices

With fixed-effect coefficients and covariance parameters held at their
maximum likelihood estimates (MLEs):

- `field_method = "mle-mvn"` (default) follows sdmTMB's recommended
  approach: draw the fitted fields together from their approximate
  posterior distribution, then hold that draw fixed for conditional PITs
  and response references. Response errors are independent given those
  fields. Reference bands use the same draw as the observed residuals.

- `"mle-eb"` holds empirical-Bayes field estimates fixed. This matches
  sdmTMB's residual construction at fitted fields, which its
  documentation does not recommend for goodness-of-fit testing.
  Shrinkage and estimation can distort the resulting residual
  distribution.

- `"mle-mvn-integrated"` averages over approximate uncertainty in fitted
  fields using sdmTMB's joint response simulations. Residuals are
  calculated from ranks in these simulations rather than from one fixed
  draw of fields. This reuses the fitting data and can give conservative
  checks; treat the results as approximate posterior predictive checks.

- `conditioning = "marginal"` instead simulates new spatial,
  spatiotemporal and grouping effects. Simulation at fitting
  observations also renews other supported field and time-varying
  effects; smooths remain fitted. `field_method` does not cause another
  field draw in this mode.

`gratia.diagnostics.field_seed` (default 1) makes the posterior field
draw reproducible independently of PIT randomisation. All panels use the
same draw. To inspect further draws, change this option and present the
results together. The sdmTMB definitions and recommendations are
documented at
<https://sdmtmb.github.io/sdmTMB/reference/residuals.sdmTMB.html> and
<https://sdmtmb.github.io/sdmTMB/articles/residual-checking.html>.

PPCs use a separate approximation that draws fixed-effect, nuisance and
field parameters together. Marginal PPCs simulate new fields at each
parameter draw; smooths retain their posterior draws. Plot captions
identify conditioning. Parameter estimation, the Gaussian approximation
to the posterior and finite simulation error affect these checks. None
of these modes guarantees that goodness-of-fit tests attain their
nominal false-positive rate after fitting.

### Calculating reference values and bands

Analytic PIT/quantile QQ plots use uniform/standard-normal reference
quantiles. For ideal independent responses, bands are pointwise
intervals for ordered residuals. For dependent responses, bands come
from joint simulations (`n_simulate`) when a joint generator exists.
Otherwise the bands are omitted and the plot explains why. Bands
condition on the chosen fitted distribution and fields. They do not
provide simultaneous coverage or account for refitting.

When a joint simulator is available but marginal CDFs or moments cannot
be calculated analytically, one reference sample of simulated data sets
supplies means, variances and residual ranks. With `R` reference draws,
PIT is `(number below + U * (ties + 1))/(R + 1)`, where `U` is uniform
on `(0, 1)`. The extra rank position is for the observation being
checked, which keeps normal scores finite even outside the simulated
range. Observed and simulated responses use the same reference sample,
centering and residual definitions. These ranks need not equal the
empirical CDF at an observation.

`options(gratia.diagnostics.marginal_draws = 249L)` sets the reference
sample size (minimum 19). Increase it to assess sensitivity to Monte
Carlo error. `gratia.diagnostics.marginal_seed` (default 1) makes this
sample reproducible independently of residual randomisation. A finite
sample limits tail resolution and moment accuracy. Moment estimates
require finite population moments: glmmTMB Student-t response residuals
require df \> 1, and Pearson residuals require df \> 2. An undefined
moment is not replaced by a finite sample estimate. PIT/quantile checks
and joint draws remain available, but an undefined mean cannot be shown
as a fitted expectation. Bands for residuals calculated from simulation
ranks use joint draws and the same rank transformation, even if the
underlying responses are independent. Marginal deviance residuals remain
unavailable where the integrated likelihood has no defined deviance; use
response, Pearson or quantile residuals instead.

### DHARMa-style residuals

[`dharma_residuals()`](https://gavinsimpson.github.io/gratia/reference/dharma_residuals.md)
and `type = "dharma"` implement simulation-based PIT residuals using
gratia's existing generators, without requiring DHARMa or mgcViz.
Responses are ranked against `n_dharma` simulated datasets (default
250), with randomisation within ties and exact zero and one endpoints.
This calculation differs from the existing simulation-rank formula
above: it uses the empirical fractions below and below or equal to the
observation, without an extra rank position. Existing PIT and quantile
residuals retain their current definitions.

The calculation needs comparable scalar response draws, but no CDF or
finite response moments. Existing model adapters retain their
conditioning defaults. Marginal-only generators provide marginal
distribution checks. Censored and multivariate observations are
unsupported. Independent responses use uniform QQ references with
approximate pointwise bands because the simulation count is finite;
dependent responses omit independence-based bands.

### Remaining limitations and extensions

Inverse CDFs with direct lower/upper and log-probability arguments
preserve extreme interior quantiles. Older functions that accept only
lower-tail probabilities cannot always represent an extreme upper-tail
probability. If conversion rounds it to an endpoint and produces a false
infinite quantile, an explanatory error is raised. This limits extreme
upper quantiles of the compound-Poisson Tweedie distribution. The Gamma
boundary and inverse-Gaussian solver accept log probabilities directly.
True probability endpoints remain valid.

BAM/PQL working correlations need a scientifically specified joint
response model before joint simulation or PPC can be added. Additional
GJRM and other fitting-package families should supply their parameters
to the shared response distributions. Censored/survival PPCs need a way
to reapply censoring to each simulated response. Nominal multivariate
PITs need a documented category ordering or a different multivariate
diagnostic. Unrecognised random-effect structures remain unsupported.
Diagnostics do not repair `na.pass` fits where missing responses cannot
be aligned with predictions and residuals; ordinary
`na.omit`/`na.exclude` fits are covered. MCMC, model refitting and
calibrated dependence tests would require additional sampling or
refitting methods.

### Large data sets and family functions

Like mgcViz, these diagnostics use residuals with known reference
distributions where available and aggregate large data sets before
plotting. All observations and QQ endpoints contribute. Kernel density
estimates and histograms remain available as separate views. See
<https://mfasiolo.github.io/mgcViz/reference/qq.gamViz.html>. mgcViz
approximates QQ plots on a discrete grid; gratia aggregates plot points
but preserves exact pooled reference quantiles. mgcViz also plots
differences between empirical and model-implied conditional densities.
Adding such plots would require comparable smoothing bandwidths and a
validated conditional reference. The existing density and histogram
views retain information about scale misspecification rather than
rescaling every residual to unit variance. See
<https://mfasiolo.github.io/mgcViz/reference/l_densCheck.html>.

Uniform references average ordered residuals as draws are generated,
without storing all draws. Simulated QQ uses exact pooled quantiles
computed as by `stats::quantile(type = 7)` and pointwise intervals for
each residual rank. Above `options(gratia.diagnostics.max_cells = 1e7)`,
sorted draws are written to temporary files and merged exactly. This
limits simulation memory use at the cost of disk I/O. One response
vector and the reference output must still fit in memory. Scalable PPCs
summarise every draw on the same grid; discrete responses with more than
512 distinct values use labelled probabilities for 512 bins.
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
returns the raw response draws. Coefficient and random-effect draws
still need memory proportional to the number of parameters times the
number of draws.

The internal `fix_family_rd()`, `fix_family_cdf()` and `fix_family_qf()`
functions still add `$rd`, `$cdf` and `$qf` to mgcv-compatible families
while preserving functions supplied by callers. The fitted-distribution
code adapts their arguments locally. Families can supply a
`gratia_diagnostics` function for response moments, probability bounds
and other distribution information; see
[family_diagnostics](https://gavinsimpson.github.io/gratia/reference/family_diagnostics.md)
for the interface. Other fitting packages can use the same standard
response distributions, or combine them by family index, truncate them
or add point masses without changing the plotting calculations.

## See also

[`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
[`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md),
[`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md),
[`scalable_qq_plot()`](https://gavinsimpson.github.io/gratia/reference/scalable_qq_plot.md),
[`posterior_predictive_plot()`](https://gavinsimpson.github.io/gratia/reference/posterior_predictive_plot.md),
[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
