# Changelog

## gratia (development version)

### User visible changes

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  now rejects response-scale requests for general families whose first
  predictor is not a supported response mean. Use
  `scale = "linear_predictor"` for the first predictor;
  [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html) means,
  multivariate normal means and multinomial probabilities remain
  supported. Previously these unsupported requests could return a
  distribution parameter as though it were an expected response.

- Gaussian coefficient sampling for
  [`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) fits with
  linear inequality constraints now warns that the draws need not
  satisfy the fitted shapes. Metropolis-Hastings sampling now gives an
  explicit error because its proposals and likelihood calculation do not
  enforce those constraints.

- gratia has a new neon hex logo. The pkgdown site uses the pink g from
  the logo as its favicon.

- **Breaking change:** Parametric plots now use the covariates as
  evaluated in the model by default: `log(x)` is plotted against
  `log(x)`, with a matching axis label. Use `back_transform = TRUE` to
  plot against the original covariates. This option is available in
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md),
  [`assemble.gam()`](https://gavinsimpson.github.io/gratia/reference/assemble.gam.md),
  [`draw.parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md),
  including interaction axes, tensor products, rugs and conditioning
  facets ([\#412](https://github.com/gavinsimpson/gratia/issues/412)).
  Multi-column basis expansions retain their original covariate axes.
  Changing the coordinate scale does not change estimated effects or
  their intervals.

- The `transform` argument of
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  is deprecated. Replace it with `back_transform = !transform`
  ([\#412](https://github.com/gavinsimpson/gratia/issues/412)).

- **Breaking change:**
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`assemble.gam()`](https://gavinsimpson.github.io/gratia/reference/assemble.gam.md)
  now default to `parametric = NULL`, including parametric terms
  alongside smooths when `select = NULL`. Selecting smooths explicitly
  continues to draw only those smooths, unless `parametric = TRUE` is
  supplied. Use `parametric = FALSE` to restore the previous default.
  Additional panels can change plot layouts and the length of plot lists
  returned by
  [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  or `draw(wrap = FALSE)`.

- Parametric effect intervals now include intercept uncertainty by
  default, including covariance between the intercept and the term.
  Partial estimates are unchanged. Use `overall_uncertainty = FALSE` in
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md),
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) or
  [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  to recover term-only intervals. Each term uses the intercept of its
  own linear predictor; models without an intercept are unaffected.

- Surface plotting and related evaluation functions now accept `n_2d`,
  with a default of 50 points along each surface axis
  ([\#153](https://github.com/gavinsimpson/gratia/issues/153)). Default
  surface grids therefore shrink from 10,000 to 2,500 points per panel.
  Curve and conditioning slice resolutions can be controlled separately.

- Derivative functions now choose the finite-difference step
  automatically when `eps = NULL`, using the fitted covariate range,
  derivative order and difference method. This reduces rounding error in
  second derivatives. Supply a positive `eps` to specify an absolute
  step size.

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  now automatically uses scalable plots when at least 10,000
  observations were used for fitting. Set `scalable = TRUE` or `FALSE`
  to override this behaviour, or change `scalable_threshold`.

- Explicit `xlab = NULL` and `ylab = NULL` now request default axis
  labels for continuous-covariate `fs` plots and random-effect tensor
  curves, consistent with other univariate smooths. Previously, explicit
  `NULL` suppressed these labels
  ([\#308](https://github.com/gavinsimpson/gratia/issues/308)).

- Tweedie cumulative distribution functions now use fast Poisson-Gamma
  mixtures for `1 < p < 2` instead of requiring the tweedie package
  ([\#410](https://github.com/gavinsimpson/gratia/issues/410)). The
  Gamma case (`p = 2`) uses native R distribution functions. Mixture
  calculations warn and return available approximations when their work
  or accuracy limits are reached. The tweedie package is now optional,
  in Suggests, and is needed only for numerical inversion requested with
  `fallback = TRUE`.

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  now uses separate random-number streams for coefficient and response
  draws. Response draws for a given seed differ from earlier releases,
  but remain reproducible.

- gratia now requires mgcv \>= 1.9-4 and R \>= 4.4.0.

- glmmTMB support now requires the released version 1.1.15.2 or later,
  so the development version of glmmTMB is no longer needed.

### New features

- [`ale()`](https://gavinsimpson.github.io/gratia/reference/ale.md)
  computes accumulated local effects for numeric covariates in
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) fits, with link
  and response scales, distribution parameters, and pointwise or
  simultaneous intervals.
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  plots saved results with parameter labels and a scale caption.

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  now accepts family objects, explicit predictors and generating
  parameters for scalar, location-scale, categorical, multivariate and
  Cox models. Censored families can return observed intervals while
  retaining complete responses and true distribution parameters. Grouped
  families can use repeated distributions with separate parameters.

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  gains analytic shape reference functions and fitting recipes for
  [`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html), including
  combined shape constraints, tensor surfaces and Gamma models with
  constraints on the log mean and log dispersion.

- Optional
  [`bayesplot::pp_check()`](https://mc-stan.org/bayesplot/reference/pp_check.html)
  methods provide posterior predictive checks for supported GAMs and
  mixed models
  ([\#278](https://github.com/gavinsimpson/gratia/issues/278), reported
  by [@nicholasjclark](https://github.com/nicholasjclark)).

- [`log_lik()`](https://gavinsimpson.github.io/gratia/reference/log_lik.md)
  and optional
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) and
  [`loo::waic()`](https://mc-stan.org/loo/reference/waic.html) methods
  support approximate predictive comparisons for independent GAM/BAM
  fits ([\#278](https://github.com/gavinsimpson/gratia/issues/278),
  reported by [@nicholasjclark](https://github.com/nicholasjclark)).

- Diagnostic plots now support native inlaws families through their
  supplied `gratia_diagnostics` functions, including grouped-family fits
  and models with multiple response columns.

- [`dharma_residuals()`](https://gavinsimpson.github.io/gratia/reference/dharma_residuals.md)
  and `type = "dharma"` in diagnostics now calculate simulation-based
  PIT residuals using gratia’s response generators, without requiring
  DHARMa or mgcViz. `n_dharma` controls the simulation count. Residuals
  retain zero and one endpoints and are shared across
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  panels. QQ and worm plots use uniform references with approximate
  pointwise bands for independent responses; dependent responses omit
  these bands.

- mgcv-compatible families can now supply a `gratia_diagnostics`
  function for response means, variances, probability bounds and
  distribution functions. This allows diagnostics for additional count,
  ordinal, mixture and censored families without adding family-name
  checks to gratia. Traditional simulated reference residuals can apply
  a supplied observation process. See
  [`?family_diagnostics`](https://gavinsimpson.github.io/gratia/reference/family_diagnostics.md)
  for the interface.

- New site articles explain model diagnostics, grouped surveys, glmmTMB
  and sdmTMB models, prediction comparisons, response distributions, and
  smooth effects on different spatial domains. They include fitted
  examples and explain the assumptions behind estimates, intervals and
  checks.

- Residual diagnostics and posterior predictive checks now cover `lm`,
  `glm`, `gam`/`bam`, `gamm`/`gamm4`, glmmTMB, univariate GJRM and
  sdmTMB models using their fitted response distributions. Probability
  integral transform (PIT) residuals map observations to fitted
  probabilities, randomising over probability mass for discrete
  responses. Quantile residuals convert those probabilities to
  standard-normal scores. See
  [`?diagnostic_support`](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md)
  for supported families, conditioning choices and limitations. Prepared
  diagnostic objects carry the selected conditioning and sdmTMB field
  choices through residual and plotting methods.

- Mixed-model diagnostics can hold fitted random effects fixed or
  average over their fitted distribution. Joint response simulations use
  the fitting packages’ generators to retain temporal and spatial
  covariance and shared random effects. Marginal residuals can use
  means, variances and ranks from these simulations. Direct
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) and
  [`lme4::merMod`](https://rdrr.io/pkg/lme4/man/merMod-class.html) fits
  also support residual and posterior predictive checks. Non-Gaussian
  `gamm` fits estimated by penalized quasi-likelihood (PQL) support
  approximate checks when they have no residual working correlation.
  nlme calculations retain covariance between random effects in block
  designs, fitted factor contrasts and observation order after
  subsetting or missing-value removal.

- sdmTMB residual diagnostics now offer four ways to treat random
  fields: draw fitted fields once from their approximate posterior and
  hold them fixed (`field_method = "mle-mvn"`, the default), hold field
  estimates fixed (`field_method = "mle-eb"`), average over fitted-field
  uncertainty (`field_method = "mle-mvn-integrated"`), or simulate new
  fields (`conditioning = "marginal"`). Plot labels identify the choice.
  Posterior predictive checks separately include approximate parameter
  uncertainty. See
  [`?diagnostic_support`](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md)
  for what these checks assess and their limitations.

- Diagnostic plots now support uncensored univariate
  [`GJRM::gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html) models
  with N, LN, GA, GAi, P, NBI and NBII families. They use randomised
  quantile residuals with a standard-normal reference.
  Observed-versus-fitted panels show response means calculated from the
  final distribution parameters. NBII probabilities are calculated from
  the final fitted likelihood parameters, rather than relying on stored
  probabilities that can use a Poisson distribution.

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  and
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  now support native glmmTMB residuals, with conditional response
  simulation for QQ reference bands. See
  [`?'glmmTMB-methods'`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  for supported families and current limitations. Gaussian predictive
  draws use glmmTMB’s standard-deviation parameterisation. Variances for
  truncated count distributions are calculated accurately even with tiny
  means.

- Grouped-family diagnostics now support quantile residuals with
  appropriate standard-normal references and consistent component
  colours. Traditional residuals use separate facets for each component
  ([\#267](https://github.com/gavinsimpson/gratia/issues/267)). Pooled
  histograms use pale fills and solid step outlines in each component’s
  colour, so overlapping distributions remain visible. Ordinary and
  scalable plots use the same bin edges and calculate each component’s
  density separately.

- Distribution checks now cover more mgcv families, including
  [`shash()`](https://rdrr.io/pkg/mgcv/man/shash.html),
  [`twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html), ordinal,
  [`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html) and grouped families.
  The internal `fix_family_qf()` extends mgcv’s
  [`fix.family.qf()`](https://rdrr.io/pkg/mgcv/man/fix.family.link.html)
  with experimental support for
  [`scat()`](https://rdrr.io/pkg/mgcv/man/scat.html),
  [`tw()`](https://rdrr.io/pkg/mgcv/man/Tweedie.html),
  [`Tweedie()`](https://rdrr.io/pkg/mgcv/man/Tweedie.html),
  [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html),
  [`gevlss()`](https://rdrr.io/pkg/mgcv/man/gevlss.html),
  [`gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html),
  [`gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html) and
  [`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html).
  `fix_family_cdf()` adds cumulative distribution functions for
  [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html),
  [`gevlss()`](https://rdrr.io/pkg/mgcv/man/gevlss.html),
  [`gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html),
  [`gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html) and
  [`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html), making
  quantile residuals available. These helpers and `fix_family_rd()`
  preserve distribution functions supplied by callers.
  [`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html) quantile
  residuals randomise over the probability mass of each observed count
  (GRA-007). Quantile helpers retain log probabilities where supported;
  conversions that cannot represent an extreme probability give an
  explanatory error rather than a false infinite quantile.

- New Tweedie quantile helpers support uniform QQ diagnostics
  ([\#410](https://github.com/gavinsimpson/gratia/issues/410)). They use
  fast Poisson-Gamma mixtures for `1 < p < 2` and native R distribution
  functions for the Gamma case (`p = 2`). Expensive numerical inversion
  is available with `fallback = TRUE` and the optional tweedie package.
  Mixture calculations warn and return available approximations when
  their work or accuracy limits are reached. With `method = NULL`,
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  and
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  automatically select simulation for
  [`tw()`](https://rdrr.io/pkg/mgcv/man/Tweedie.html) and
  [`Tweedie()`](https://rdrr.io/pkg/mgcv/man/Tweedie.html) models,
  avoiding numerical quantiles. Other ordinary GAM families prefer
  `"uniform"`, with fallbacks when a method is unavailable. An explicit
  method overrides the automatic choice.

- [`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md)
  now supports censored
  [`cnorm()`](https://rdrr.io/pkg/mgcv/man/cnorm.html),
  [`cpois()`](https://rdrr.io/pkg/mgcv/man/cpois.html) and
  [`clog()`](https://rdrr.io/pkg/mgcv/man/clog.html) models, randomising
  over censoring intervals for PIT and quantile residuals. These
  families also gain cumulative distribution and quantile helpers, and
  `fix_family_rd()` provides random generation for their latent,
  uncensored responses. The
  [`cnorm()`](https://rdrr.io/pkg/mgcv/man/cnorm.html) and
  [`clog()`](https://rdrr.io/pkg/mgcv/man/clog.html) generators use the
  fitted family scale and prior weights. Tail probabilities are
  calculated on the logarithmic scale where supported to avoid numerical
  infinities.

- Inverse Gaussian distribution functions now support PIT/quantile
  residuals and inverse-CDF QQ/worm plots without additional
  dependencies. Their calculations account for prior weights.

- Explicit GAM sampling weights override response precision or binomial
  trial counts.

- [`posterior_predictive_plot()`](https://gavinsimpson.github.io/gratia/reference/posterior_predictive_plot.md)
  compares observed responses with posterior predictive distributions,
  using density curves for continuous responses and points with interval
  bars for discrete responses. Set `appraise(ppcheck = TRUE)` to replace
  the residual histogram with this check. Plots summarise all draws on a
  shared grid; discrete responses with more than 512 distinct values use
  labelled probabilities for 512 bins.

- Five new `scalable_*_plot()` functions provide binned QQ/worm plots,
  residual-density heatmaps, histograms and observed-versus-fitted
  heatmaps. They use all observations while keeping plotting data
  compact. Residual heatmaps estimate the density at each predictor
  value with
  [`KernSmooth::bkde2D()`](https://rdrr.io/pkg/KernSmooth/man/bkde2D.html)
  by default. A square-root colour scale makes low-density regions
  easier to see. Use `density_method = "histogram"` for unsmoothed bins,
  or `bandwidth` to control smoothing on both axes. Heatmaps of counts
  remain unsmoothed. Plot objects store the grid rather than all
  observations. KernSmooth is an imported dependency.

- QQ reference calculations now average draws as they are generated, or
  use temporary files for exact simulated quantiles above the configured
  memory limit. This reduces memory use without discarding reference
  draws. Simulated GAM QQ and worm plots are also faster, with unchanged
  numerical results.

- [`residual_correlation()`](https://gavinsimpson.github.io/gratia/reference/residual_correlation.md)
  and
  [`residual_correlation_plot()`](https://gavinsimpson.github.io/gratia/reference/residual_correlation.md)
  provide approximate checks of serial structure, including for BAM
  models that apply AR1 to working residuals. They omit significance
  bands based on independence.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md),
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  and smooth coefficient extraction now support glmmTMB models,
  including conditional, zero-inflation and dispersion smooths where
  glmmTMB supports them. All smooths are plotted by default, with
  mgcv-style predictor labels (`s(x)`, `s.1(x)`, etc.) and `select`
  applying across the model. Parametric effects and native random
  effects are also plotted, including for models without smooths. Native
  random intercepts and slopes use separate Gaussian QQ panels. Set
  `ranef_lines = TRUE` to replace a simple `(1 + x | g)` pair with lines
  showing its random contribution only; more complex terms retain QQ
  panels. Effect intervals use TMB joint covariance. See
  [`?'glmmTMB-methods'`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  for current limitations.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  and
  [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  now support formula smooths and parametric effects from sdmTMB models,
  including both predictors of delta models. Companion methods provide
  smooth labels, coefficients and effect estimates. Smooths use joint
  TMB/RTMB covariance; tensor smooths with multiple penalties are
  supported.

- Smooth plot subtitles now identify the model part or distributional
  parameter for glmmTMB, mgcv and
  [`GJRM::gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html) models
  with multiple linear predictors. Single-predictor models omit these
  labels, and constant glmmTMB dispersion alone does not trigger them.
  Information about `by` variables, smooth titles, selection and the
  default of plotting all smooths are retained. Saved
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  retain the labels in an optional `.model` column.

- Soap-film boundary (`bs = "sf"`) and wiggly (`bs = "sw"`) components
  are now supported, with distinct component types and boundary-aware
  plots ([\#375](https://github.com/gavinsimpson/gratia/issues/375)).

- Grouped-family models fitted with
  [`mgcv::gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html) now support
  response-scale
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  with component-specific mean transforms
  ([\#269](https://github.com/gavinsimpson/gratia/issues/269)), and
  [`simulate()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  with component-specific distributions and parameters
  ([\#272](https://github.com/gavinsimpson/gratia/issues/272)). Both
  accept `family_index` for prediction data; ordinal response-scale
  fitted draws remain unsupported.

- Support for classed gamm4 models now covers smooths, parametric
  effects, predictions, derivatives, simulations, posterior draws and
  variance components. Smooth and prediction methods describe the
  population GAM component and exclude additional terms supplied through
  `random`. Models require gamm4 \>= 0.2-7; historical unclassed lists
  are unsupported. See
  [`?gamm4_support`](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
  for covariance and sampling limitations, and for the full-model
  conditioning options used by diagnostics and response draws.
  Diagnostics, response simulations and posterior predictive checks use
  the full fitted mixed model with `conditioning = "conditional"` by
  default. Response references hold fitted smooth and grouping effects
  fixed; posterior predictive checks include uncertainty in those fitted
  effects. Select `conditioning = "gam_component"` to use only the
  extracted GAM component. This choice is required when supplying
  coefficient draws or prediction data to
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md);
  full-model draws use fitting observations only.

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  and
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  now support parametric interactions, including higher-order
  interactions. Numeric interactions use surfaces and conditioning
  facets; numeric-by-factor interactions use grouped curves; factor
  interactions use grouped estimates and intervals. Interaction surfaces
  centre the default diverging palette at zero, using a common fill
  range across facets; fixed scales expand the fill range as for smooth
  surfaces ([\#411](https://github.com/gavinsimpson/gratia/issues/411)).

- Smooth evaluation, plotting, bases, sampling and prediction now
  support functions of covariates such as `s(log(x))`, including
  transformed offsets and an `envir` argument for local functions and
  constants. Stored evaluated covariates allow smooth plotting when the
  original data or functions are unavailable. Automatic smooth grids
  retain transformed coordinates, and
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  and
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  can differentiate with respect to raw covariates using
  `wrt = "covariate"`. See
  [`vignette("transformed-covariates", package = "gratia")`](https://gavinsimpson.github.io/gratia/articles/transformed-covariates.md)
  for examples. Evaluation failures at new data produce actionable
  errors. Parametric effects and data slices recover raw covariates
  consistently.
  [`simulate()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  and
  [`add_fitted()`](https://gavinsimpson.github.io/gratia/reference/add_fitted.md)
  also accept `envir` for local functions in offsets and smooth
  expressions.

- `smooth_estimates(..., back_transform = TRUE)` now recovers raw
  fitting covariates, generates grids on their original scale and
  evaluates model expressions on those grids
  ([\#412](https://github.com/gavinsimpson/gratia/issues/412)). This
  supports custom functions such as `foo(x)` and tensor products without
  needing inverse functions. Smooth grids retain their existing
  model-scale defaults. Drawing precomputed estimates on the original
  scale requires retained raw coordinates; estimates with only
  transformed coordinates give an informative error. Parametric effects
  retain both scales by row, including for many-to-one transformations.
  Coordinate columns survive confidence-interval calculation and
  subsetting. Typical values of other covariates are not treated as raw
  axes, and numeric `by` multipliers can share a raw axis name. Drawing
  uses `envir` to recover observations. Supplied prediction data remain
  usable when raw observations for rugs or residuals are unavailable.
  Transformed soap-film boundaries cannot be drawn on the original scale
  and give an explicit error. Irregularly spaced surfaces use rectangles
  to preserve covariate positions
  ([\#412](https://github.com/gavinsimpson/gratia/issues/412)).

- Full and reduced-rank Markov random field (MRF) smooths now support
  area effect estimates and interval plots
  ([\#9](https://github.com/gavinsimpson/gratia/issues/9)). Use
  `mrf_plot = "qq"` for a descriptive QQ plot.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md),
  [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  now support simultaneous bands for individual smooths
  ([\#314](https://github.com/gavinsimpson/gratia/issues/314)).
  `interval = "credible"` is an alias for pointwise `"confidence"`
  intervals.

- Numeric smooth and parametric surfaces can now be drawn in 3D with
  `surface = "3d"`, using the optional ggcube package
  ([\#393](https://github.com/gavinsimpson/gratia/issues/393)). The
  existing RdBu fill palette, theme, conditioning facets, contours and
  masking are retained. `surface_3d = list(...)` controls the view
  without applying ggcube’s theme. Lighting defaults to
  `backface_scale = 1`; supply a custom `light` through `surface_3d` to
  override it. Opaque faces, contours and floor observations share depth
  ordering so hidden features are occluded.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  and
  [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  now accept `surface = "tile"` to draw flat smooth surfaces with vector
  rectangles; the default remains `surface = "raster"`
  ([\#308](https://github.com/gavinsimpson/gratia/issues/308)).
  Bivariate, trivariate, quadvariate, tensor-product and soap-film
  surfaces use the same rendering options.

- Random-effect tensor-product plots now accept `re_levels` to select
  factor levels. `n_3d` and `n_4d` also limit the number of factor
  levels used for conditioning facets
  ([\#169](https://github.com/gavinsimpson/gratia/issues/169)).

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  for penalty matrices now accepts `geom = "tile"` to draw individual
  rectangles; `geom = "raster"` remains the default
  ([\#392](https://github.com/gavinsimpson/gratia/issues/392)).

- [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  now supports delta-method standard errors and pointwise intervals via
  `uncertainty = "delta"`. Both uncertainty methods return `.se`;
  posterior simulation remains the default.

- [`conditional_differences()`](https://gavinsimpson.github.io/gratia/reference/conditional_differences.md)
  compares conditional fitted values between factor levels or joint
  factor combinations on the link or response scale, with pointwise or
  simultaneous intervals using the delta method or shared posterior
  simulations. Simultaneous bands can cover each comparison separately
  or all comparisons jointly via `simultaneous_scope`.
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  plots the differences and their intervals
  ([\#143](https://github.com/gavinsimpson/gratia/issues/143),
  [\#145](https://github.com/gavinsimpson/gratia/issues/145)).

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  now offers simultaneous intervals for differences of smooths at
  supplied covariate combinations, separately for each pair of factor
  levels, via `interval = "simultaneous"`
  ([\#380](https://github.com/gavinsimpson/gratia/issues/380)).
  Pointwise intervals remain the default.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now offers simultaneous intervals for fitted values at supplied
  covariate combinations via `interval = "simultaneous"`
  ([\#4](https://github.com/gavinsimpson/gratia/issues/4)). Pointwise
  intervals remain the default. Simulation size, parallel sampling and
  seeds can be controlled, as in
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  and
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md).
  The legacy
  [`fderiv()`](https://gavinsimpson.github.io/gratia/reference/fderiv.md)
  implementation is unchanged.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now emits a message identifying prediction rows with unobserved
  combinations of known categorical predictor levels
  ([\#164](https://github.com/gavinsimpson/gratia/issues/164)).

- [`all_levels()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  is now an alias of
  [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  with a name intended for factors
  ([\#217](https://github.com/gavinsimpson/gratia/issues/217)).

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  gains a `.by` argument to evaluate slice expressions within observed
  groups. For example, `x = evenly(x)` can use a separate range for each
  factor level, which is useful for models with factor `by` smooths
  ([\#196](https://github.com/gavinsimpson/gratia/issues/196)).

- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  now accepts `complete = FALSE` to retain only observed combinations of
  factor conditions while preserving numeric prediction grids
  ([\#337](https://github.com/gavinsimpson/gratia/issues/337)).

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  now accept `frequentist = TRUE` to use the frequentist covariance
  matrix. The default remains `FALSE`. Simultaneous intervals use the
  selected covariance matrix consistently (GRA-005).

- [`get_vcov()`](https://gavinsimpson.github.io/gratia/reference/get_vcov.md)
  is now exported for extracting a model’s covariance matrix.

- gratia now supports the `clognorm()` family from mgcvUtils.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and other family utilities now support
  [`cpois()`](https://rdrr.io/pkg/mgcv/man/cpois.html),
  [`clog()`](https://rdrr.io/pkg/mgcv/man/clog.html) and
  [`bcg()`](https://rdrr.io/pkg/mgcv/man/bcg.html). These families also
  work with
  [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md).
  Single-link families can use their supplied link functions without
  needing a recognised family name.

### Bug fixes

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  now transforms the `eg6` predictor once and returns the predictor used
  for sampling. Grouped-family simulations respect seeds, parameters,
  ordinal aliases and repeated family positions; `dist` is ignored
  throughout that legacy example. The `eg5` and `eg6` examples accept
  sample sizes not divisible by four. Explicit seeds restore an absent
  RNG state, and reference generation forwards the requested Tweedie
  power. Vector Tweedie parameters now use the correct compound
  Poisson-Gamma law.

- Large QQ reference simulations now close their file connections before
  removing temporary files, allowing cleanup to complete on Windows.

- SCAM smooth estimates and standard errors no longer depend on the
  other rows in the evaluation grid. Single-row and short-grid
  evaluations are supported. `overall_uncertainty = TRUE` includes
  intercept variance and its covariance with the smooth; `FALSE`
  includes smooth uncertainty alone. `unconditional = TRUE` now gives an
  explicit error because SCAM calculations do not support
  smoothing-parameter uncertainty.

- Scalable posterior predictive plots for SCAM models now give an
  explicit error explaining that `scalable = FALSE` is required.
  Ordinary posterior predictive plots remain available.

- Partial derivatives, fitted samples and smooth samples of soap-film
  smooths include the contribution from known boundary values.
  Complete-predictor derivative samples also differentiate those fixed
  values, without adding coefficient uncertainty. Term exclusions retain
  only the appropriate fixed contributions. Separate sampling per smooth
  now correctly subsets supplied coefficient draws.

- Multinomial fitted samples now honour `scale = "linear_predictor"`,
  returning log odds relative to the reference category instead of
  probabilities. Both scales retain all category labels; the reference
  predictor is zero. Models with one non-reference category also use
  this output format.

- GJRM smooth selection now uses one order across all model equations.
  Character selections draw only the equations containing the selected
  smooths; repeated labels have distinct keys returned by
  [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md).
  `wrap = FALSE` returns a flat list of plots.

- Student-t sdmTMB models now support residual diagnostics and posterior
  predictive plots with fixed or estimated degrees of freedom. Pearson
  residuals require degrees of freedom above two. Quantile residuals and
  predictive checks remain available with infinite variance. Predictive
  plots also work for additional native sdmTMB families without
  requiring an analytic residual distribution.

- Smooth plots drawn with `scales = "fixed"` calculate their shared
  ranges from finite estimates and interval bounds. Missing soap-film
  boundary rows no longer cause each component to use a separate colour
  scale.

- Parametric interaction plots for glmmTMB and sdmTMB retain their term
  and coordinate information across model components. This prevents
  factor interactions from being mistaken for numeric effects when
  drawing rugs.

- Response distributions now correctly account for Gaussian and Gamma
  dispersion with observation weights, factor-valued Bernoulli responses
  and numeric binomial trial counts.

- Beta diagnostics now include the probability mass created by clipping
  native draws at epsilon endpoints. Binomial probabilities immediately
  below an observed value are calculated more accurately.

- GEV and sinh-arcsinh residuals now use stable means and correct
  centering and signs. Undefined moments remain unavailable. Invalid
  sinh-arcsinh deviance contrasts give an explanatory error;
  PIT/quantile checks remain available.

- Quantile residuals now calculate probabilities on the logarithmic
  scale in the smaller tail for families that support this calculation,
  including Poisson and negative binomial. This avoids numerical
  infinities without clipping probabilities.

- Grouped-binomial quantile residuals now randomise over the probability
  mass of one success rather than one whole response proportion.

- BAM models that apply AR1 to working residuals using a generalized
  estimating equations (GEE) approximation retain marginal PIT, quantile
  and QQ checks. QQ plots no longer switch automatically to normal
  references or use bands that assume independence. The working
  correlation does not specify a joint response distribution, so joint
  response simulation and posterior predictive checks are unavailable.
  Gaussian identity-link AR1 response simulation remains supported.

- QQ bands for dependent responses now use joint simulations when
  available. Otherwise, the plot explains why the bands are omitted.

- Fitted and posterior response draws, response simulations and
  diagnostics now retain offsets supplied as fitting arguments for
  fitting observations, including ordinary
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) and extracted
  [`gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) GAM components.
  Recovery keeps formula offsets, prescribed smooth boundaries and
  additional group effects separate. Explicit prediction data still
  follow the fitter’s prediction convention and do not inherit
  argument-only fitting offsets.

- Explicit seeds now preserve the caller’s random-number state for
  coefficient and response draws.
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  also honours `seed`, including through `appraise(use_worm = TRUE)`,
  and restores the caller’s random-number state after seeded calls
  ([\#407](https://github.com/gavinsimpson/gratia/issues/407)).

- For models fitted with `na.exclude`, default fitted values, partial
  residuals, fitted and posterior samples, simulations and
  [`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md)
  now restore excluded rows as `NA`. Diagnostics align retained
  observations with predictors, and histogram bin selection handles
  missing residuals
  ([\#349](https://github.com/gavinsimpson/gratia/issues/349),
  [\#402](https://github.com/gavinsimpson/gratia/issues/402)). Explicit
  prediction data preserve row positions by default: missing required
  predictors yield `NA`, while missing responses alone do not prevent
  prediction. Discrete [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html)
  fitted-value intervals restore exclusions once and accept stored
  fitting frames without relying on tibble subsetting in native
  prediction.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`extract_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now reject unsupported parameters consistently. For
  [`twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html), `scale`, `sigma`
  and `phi` select the third linear predictor, and `power` selects the
  second. Explicit `which_eta` indices for
  [`mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html) and
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html) are now
  respected and validated.

- Custom smooth labels supplied through `draw(..., subtitle = ...)` and
  other label arguments now reach the smooth renderer correctly.

- Parametric plot coordinates on the model scale now contain the
  evaluated predictor rather than its coefficient-weighted partial
  contribution
  ([\#412](https://github.com/gavinsimpson/gratia/issues/412)).

- [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  and
  [`data_combos()`](https://gavinsimpson.github.io/gratia/reference/data_combos.md)
  no longer require an `envir` argument to recover model covariates.

- Automatic surface fill ranges now ignore non-finite estimates and
  standard errors
  ([\#308](https://github.com/gavinsimpson/gratia/issues/308)).

- Random-effect tensor products now plot curves, categorical estimates
  and faceted surfaces consistently across
  [`te()`](https://rdrr.io/pkg/mgcv/man/te.html),
  [`ti()`](https://rdrr.io/pkg/mgcv/man/te.html) and
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html)
  ([\#169](https://github.com/gavinsimpson/gratia/issues/169)).
  Two-factor products use points with confidence intervals, faceted by
  the second factor, rather than empty line plots. Products with one
  factor use the continuous covariate on the x axis regardless of margin
  order ([\#395](https://github.com/gavinsimpson/gratia/issues/395)).
  Three- and four-variable
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html) smooths with factor
  margins produce faceted surfaces.

- Spline-on-the-sphere plots now use valid cells at the poles and date
  line, with orthographic clipping and separate factor-by panels
  ([\#277](https://github.com/gavinsimpson/gratia/issues/277)).

- [`draw.gamlss()`](https://gavinsimpson.github.io/gratia/reference/draw.gamlss.md)
  now sizes its plot layout using the panels actually drawn, including
  parametric effects, avoiding rendering errors for GJRM models.

- Posterior sampling for GAMs, including the GAM component of gamm4
  models, now falls back to
  [`mgcv::rmvn()`](https://rdrr.io/pkg/mgcv/man/rmvn.html) with a
  warning if the default sampler’s Cholesky decomposition fails
  ([\#332](https://github.com/gavinsimpson/gratia/issues/332)).

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  now uses the fitted coefficient order for random-effect interactions
  fitted with `bam(discrete = TRUE)`, correcting estimates and standard
  errors ([\#387](https://github.com/gavinsimpson/gratia/issues/387)).

- [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  and
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  now honour `terms` when selecting parametric effects, while retaining
  the selected smooths
  ([\#368](https://github.com/gavinsimpson/gratia/issues/368)).

- [`variance_comp()`](https://gavinsimpson.github.io/gratia/reference/variance_comp.md)
  now handles variance components without confidence intervals,
  including default GCV fits, and returns a typed empty table when
  [`mgcv::gam.vcomp()`](https://rdrr.io/pkg/mgcv/man/gam.vcomp.html)
  returns no components
  ([\#377](https://github.com/gavinsimpson/gratia/issues/377)).

- [`rtw()`](https://gavinsimpson.github.io/gratia/reference/rtw.md) now
  assigns Gamma shapes to the correct observations when Tweedie power
  varies, including in
  [`twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html) response
  simulation. Scalar parameters are expanded consistently, and
  incompatible lengths or non-finite inputs are rejected
  ([\#409](https://github.com/gavinsimpson/gratia/issues/409)).

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  now honours custom cut points for ordinal responses and validates that
  they are finite, strictly increasing and consistent with the requested
  integer number of categories
  ([\#408](https://github.com/gavinsimpson/gratia/issues/408)).

- [`draw.rootogram()`](https://gavinsimpson.github.io/gratia/reference/draw.rootogram.md)
  now applies the square-root transformation when a custom y-axis label
  is supplied, keeping plotted frequencies independent of the label
  ([\#406](https://github.com/gavinsimpson/gratia/issues/406)).

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  now reports one degree of freedom per parametric coefficient, avoiding
  incorrect values and row-length errors for factors and models without
  an intercept
  ([\#405](https://github.com/gavinsimpson/gratia/issues/405)).

- Loading gratia no longer changes
  [`weights()`](https://rdrr.io/r/stats/weights.html) dispatch for
  linear models. QQ and worm plots preserve observation weights and
  excluded rows
  ([\#404](https://github.com/gavinsimpson/gratia/issues/404)).

- [`fixed_effects()`](https://gavinsimpson.github.io/gratia/reference/fixed_effects.gam.md)
  now excludes smooth coefficients using their model indices, so
  tensor-product smooth coefficients no longer appear as parametric
  effects ([\#403](https://github.com/gavinsimpson/gratia/issues/403)).

- [`data_combos()`](https://gavinsimpson.github.io/gratia/reference/data_combos.md)
  now honours `complete = FALSE`, returning only observed combinations
  of factor levels while retaining typical continuous values and
  variable selection
  ([\#400](https://github.com/gavinsimpson/gratia/issues/400)).

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  and
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  now preserve the correct smooth selection when excluding random-effect
  or unsupported smooths, avoiding logical-index recycling warnings
  under R-devel.
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  aligns focal variables and output labels with the selected
  multivariate smooths, including when univariate smooths are skipped
  ([\#396](https://github.com/gavinsimpson/gratia/issues/396)).

- [`derivatives.gam()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now explains when requested smooths are unsupported, with a message if
  some cannot be handled or an error if none can. This covers
  multivariate smooths such as `s(x1, x2)`, `te(x1, x2)`, `ti(x1, x2)`
  and `t2(x1, x2)`, as well as random-effect smooths
  ([\#389](https://github.com/gavinsimpson/gratia/issues/389), reported
  by [@mikeguggis](https://github.com/mikeguggis) in
  [\#362](https://github.com/gavinsimpson/gratia/issues/362)).

- [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
  now honours `unconditional = TRUE` when computing standard errors
  ([\#397](https://github.com/gavinsimpson/gratia/issues/397)).

- Adding a constant to parametric effects now shifts estimates and
  confidence intervals together, including when drawing or assembling
  plots ([\#398](https://github.com/gavinsimpson/gratia/issues/398)).

- Gaussian rootograms now use response-scale means and standard
  deviations adjusted for prior weights when computing expected counts.
  Nonpositive or non-finite prior weights are rejected
  ([\#399](https://github.com/gavinsimpson/gratia/issues/399)).

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now computes correctly ordered confidence bounds for
  location-scale-shape, zero-inflated Poisson
  ([`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html)) and Tweedie
  location-scale-shape
  ([`twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html)) models.
  Response-scale intervals retain ordered endpoints for decreasing
  inverse links, as do transformed intervals from
  [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  (GRA-001).

- [`post_draws.default()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
  and
  [`generate_draws.gam()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
  no longer use `seed = 0` for every call with `seed = NULL`. They now
  generate a valid integer seed without modifying the caller’s
  random-number state (GRA-002).

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now holds numeric `by` variables fixed when computing derivatives of
  smooths. Previously, finite differences shifted both the smooth
  covariate and its numeric multiplier, giving incorrect derivatives,
  standard errors and intervals. Only the current smooth’s covariate is
  now shifted, for supplied data and automatically generated grids
  (GRA-003).

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  now matches smooth covariate combinations across factor levels before
  computing contrasts. Previously, different row orders in supplied data
  could silently compare different covariate locations, giving incorrect
  differences, standard errors and intervals. Duplicate combinations and
  mismatched grids now produce informative errors; comparisons at a
  single location also work (GRA-004).

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  now uses consistent covariance and standard errors for simultaneous
  intervals, respecting `unconditional`. These intervals exclude
  overall-mean uncertainty, as documented (GRA-005). Requesting multiple
  smooths no longer causes an error (GRA-006).

- [`simulate.gamm()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  now respects the supplied `data` argument. `smooth_samples.gamm()` and
  `overview.gamm()` now forward additional arguments to their underlying
  GAM methods instead of ignoring them (GRA-008).

## gratia 0.11.2

CRAN release: 2026-02-07

### User visible changes

- *gratia* now only handles shape constrained models fitted using *scam*
  version 1.2-21 or later. This is due to breaking changes in *scam*,
  and new functionality in that package, which simplifies *gratia*.

- *gratia* also now depends on *dplyr* version 1.2.0 or later.

- Histogram bars drawn by
  [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md)
  and points in
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md),
  [`residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_linpred_plot.md),
  [`observed_fitted_plot()`](https://gavinsimpson.github.io/gratia/reference/observed_fitted_plot.md),
  and
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md),
  and hence
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
  are now filled with a dark shade of grey and use a light colour for
  the border of the bars.

### New features

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now works for constrained factor smooths (`bs = "sz"`), treating them
  in the same way as random factor smooths (`bs = "fs"`). Lack of
  support reported by [@vrest-png](https://github.com/vrest-png)
  [\#362](https://github.com/gavinsimpson/gratia/issues/362)

  Also handles higher order `bs = "sz"` terms.

- [`draw.derivatives()`](https://gavinsimpson.github.io/gratia/reference/draw.derivatives.md)
  can now differentiate random and constrained factor smooths through
  the colour aesthetic via new argument `differentiate_factor_smooths`.

- [`model_terms()`](https://gavinsimpson.github.io/gratia/reference/model_terms.md)
  returns the names of all terms in a model.
  [\#388](https://github.com/gavinsimpson/gratia/issues/388)

### Bug fixes

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  now excludes interactions in models with multiple linear predictors,
  matching the existing single-predictor behaviour
  ([\#369](https://github.com/gavinsimpson/gratia/issues/369)).

- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  would fail if supplied a numeric vector of data to condition on via
  the `condition` argument.
  [\#366](https://github.com/gavinsimpson/gratia/issues/366)

- *gratia* worked inconsistently with tensor product smooths containing
  a random effect `bs = "re"` marginal. Reported
  [\#358](https://github.com/gavinsimpson/gratia/issues/358) and fixed
  by [@asgersvenning](https://github.com/asgersvenning)
  [\#360](https://github.com/gavinsimpson/gratia/issues/360), with
  additional changes by Gavin Simpson.

- Argument `mvn_method` was not being passed on to the lower workhorse
  functions that did the sampling.
  [\#381](https://github.com/gavinsimpson/gratia/issues/381)

- [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  would fail if there were no other terms in the model but the focal
  term.

## gratia 0.11.1

CRAN release: 2025-08-25

### User visible changes

- [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md)
  and hence
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  now centre the middle bin of the histogram at 0. In part this was due
  to *ggplot2*’s new binning algorithm leading to potentially odd
  choices for the bin breaks in the context of model residuals.

### New features

- [`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md)
  now supports more of *mgcv*’s families:
  1.  [`scat()`](https://rdrr.io/pkg/mgcv/man/scat.html),
  2.  [`nb()`](https://rdrr.io/pkg/mgcv/man/negbin.html)
  3.  [`betar()`](https://rdrr.io/pkg/mgcv/man/Beta.html),
  4.  [`tw()`](https://rdrr.io/pkg/mgcv/man/Tweedie.html).
- Several user friendliness improvements in
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md):
  - now better handles the case where there are multiple smooths for
    which partial derivatives are required,
  - correctly identifies smooths that involve random effect terms
    (i.e. any smooth or tensor product marginal smooths with
    `bs %in% c("re", "fs")`) and ignores them,
  - identifies and ignores univariate smooths, and
  - displays more informative error messages to explain what was wrong.

  Part of discussion with [@BenFN121](https://github.com/BenFN121) in
  [\#356](https://github.com/gavinsimpson/gratia/issues/356).

### Bug fixes

- [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  threw an error when the `select` argument was used.
  [\#356](https://github.com/gavinsimpson/gratia/issues/356) reported by
  [@BenFN121](https://github.com/BenFN121)

- [`draw.conditional_values()`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_values.md)
  was setting a label on the fill aesthetic even if that aesthetic was
  not being used. In *ggplot2* v4.0.0 this resulted in a warning, which
  is fixed.

## gratia 0.11.0

CRAN release: 2025-08-18

*gratia* now has a [paper](https://doi.org/10.21105/joss.06962)
describing the package in the [Journal of Open Source
Software](https://joss.theoj.org/). If you use *gratia* in your work,
please cite this paper rather than the generic citation previously
created by [`citation()`](https://rdrr.io/r/utils/citation.html):

> Simpson, G.L. (2024) gratia: An R package for exploring generalized
> additive models. *The Journal of Open Source Software* **9**, 6962.

### User visible changes

- Experimental support for computing with parallel processes via the
  *purrr* 📦 in
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  and
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md),
  though it is of limited use in the latter. Parallel processing will
  operate if a call to
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
  has been issued in the current R session.

- Rug plots in
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now only show the unique data values to avoid needless overplotting.
  This behaviour is controlled via new argument `distinct_rug`. Setting
  this to `FALSE` will get the previous behaviour.

- [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  gains a [`print()`](https://rdrr.io/r/base/print.html) method and thus
  no longer prints its attributes.

- `citation("gratia")` now suggests to use Simpson (2024) *Journal of
  Open Source Software* **9**(104), 9862, when citing the package.

- [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  now returns a data frame, bringing it into line with
  `stats::simulate.lm()`.

- With soap film smooths,
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  no longer plots points that are deemed outside the boundary of the
  soap film (as determined by
  [`mgcv::inSide()`](https://rdrr.io/pkg/mgcv/man/inSide.html)).
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  continues to return these points however, because it might be
  confusing to suddenly lose evaluation points. This behaviour can be
  controlled by the argument `clip`, which is available to
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`smooth_estimates.gam()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  currently.

- [`draw.compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/draw.compare_smooths.md)
  now uses “Partial effect” as the y-axis label, in common with other
  partial effect plots in *gratia*.

### New features

- [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  is a new generic function for assembling sets of plots from model
  objects. It is effectively
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  but without actually plotting on the current device. Instead it
  returns a list of `ggplot` objects. Currently, a method for `"gam"`
  models,
  [`assemble.gam()`](https://gavinsimpson.github.io/gratia/reference/assemble.gam.md),
  is provided which
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  uses. Part of the wish of [@ha0ye](https://github.com/ha0ye)
  [\#35](https://github.com/gavinsimpson/gratia/issues/35)

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  with `method = "simulate"` can now handle *mgcv*’s multivariate normal
  models fitted with `family = mvn()`and Tweedie location scale models
  fitted with `family = twlss()`.

- `fix_family_rd()` is an (currently) internal function that extends
  [`mgcv::fix.family.rd()`](https://rdrr.io/pkg/mgcv/man/fix.family.link.html).
  This function adds a `rfoo()` function to the `$rd` component of the
  [`family()`](https://rdrr.io/r/stats/family.html) object stored in the
  model. This function is used to generate new values of the response at
  supplied values of the linear predictor. `fix_family_rd()` calls
  [`mgcv::fix.family.rd()`](https://rdrr.io/pkg/mgcv/man/fix.family.link.html)
  so it is backward compatible with *mgcv*’s behaviour, but it is
  designed to extend the behaviour through support for a richer set of
  models.

  Currently, `fix_family_rd()` adds support for multivariate normal
  models fitted with `family = mvn()` and location-scale Tweedie models
  fitted with `family = twlss()`, but the latter is currently slow.

  The functionality of `fix_family_rd()` may move to another package or
  even to *mgcv* if suitable.

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  can now be used to compare named smooths using
  `select = c("s(x):f1", "s(x):f2")` etc, where the vector of smooth
  names must match exactly something returned by
  [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md).
  Smooths to be compared must have the same covariate (`x` in the
  example) and same factor-by variable (`f` in the example). Tensor
  product factor-by smooths are handled likewise. This effectively
  handles the problem of
  [\#315](https://github.com/gavinsimpson/gratia/issues/315), reported
  by [@3rd3](https://github.com/3rd3).

  Additionally,
  [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  with `group_means = TRUE` can now include group means specified using
  a random effect smooth, e.g. following the above example for `select`,
  a random effect smooth `s(f, bs = "re")`, with name `s(f)`, is looked
  for in the model matrix and its effects included in the comparisons.

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  gains argument `.observed_only` which allows simple filtering of the
  combinations of values specified in the slice to those that are in the
  data frame supplied or in the model frame of a fitted GAM.

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md),
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  and
  [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  now simulate for all linear predictors (i.e., all responses) in
  [`mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html) and
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html) models as
  these are all location parameter linear predictors.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now works for GAMs fitted with families
  [`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html),
  [`mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html), and
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html). The
  non-working of `fitted-values()` for
  [`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html) models was reported
  in [\#341](https://github.com/gavinsimpson/gratia/issues/341) by
  [@rroyaute](https://github.com/rroyaute)

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md),
  `derivatives_samples()`, and
  [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  now all work with models fitted by
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html).

- [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  can now take a vector of coefficients for the basis functions when
  using the default method:
  `basis(s(x), data = df, coefficients = rnorm(10))`. Wish of
  [\#136](https://github.com/gavinsimpson/gratia/issues/136)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  can now handle soap film smooths with known boundary values, and with
  nested boundaries.

- [`scale_fill_partial_effect()`](https://gavinsimpson.github.io/gratia/reference/scale_fill_partial_effect.md)
  is a *ggplot* scale function that implements the default diverging
  red-blue gradient used by *gratia* when plotting partial effect
  surfaces for multivariate smooths.

- [`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md)
  computes probability integral transform (PIT) and randomised quantile
  residuals for some GAMs and GLMs. Currently only models fitted with a
  [`gaussian()`](https://rdrr.io/r/stats/family.html),
  [`binomial()`](https://rdrr.io/r/stats/family.html),
  [`Gamma()`](https://rdrr.io/r/stats/family.html), or
  [`poisson()`](https://rdrr.io/r/stats/family.html) family are
  supported.

- [`residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_linpred_plot.md)
  and
  [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md)
  can now use PIT or randomised quantile residuals in their diagnostic
  plots.

### Bug fixes

- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  would fail if a variable it was conditioning on was also a name of a
  function. [\#323](https://github.com/gavinsimpson/gratia/issues/323)
  (but not really fixed when this was initially closed)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  was only keeping one set of factor levels when there were multiple
  parametric factor terms in a model. Reported by
  [@tamas-ferenci](https://github.com/tamas-ferenci) as part of
  discussion to
  [\#284](https://github.com/gavinsimpson/gratia/issues/284)

- [`mh_draws()`](https://gavinsimpson.github.io/gratia/reference/mh_draws.md)
  would fail in the case of drawing only `n = 1` samples.
  [\#328](https://github.com/gavinsimpson/gratia/issues/328) Reported by
  [@zsusswein](https://github.com/zsusswein)

- [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  now works for the [`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html)
  family, which has a non-standard `rd` function in its
  [`family()`](https://rdrr.io/r/stats/family.html) object. Reported by
  [@hhp94](https://github.com/hhp94),
  [\#319](https://github.com/gavinsimpson/gratia/issues/319)

- [`gaussian_draws()`](https://gavinsimpson.github.io/gratia/reference/gaussian_draws.md)
  would fail in some cases with *gamm4* models because the coefficients
  were returned as a vector, not the expected matrix.
  [\#332](https://github.com/gavinsimpson/gratia/issues/332) reported by
  [@JaredStufft-Elion](https://github.com/JaredStufft-Elion)

- [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  would generate very many warnings when plotting spline on the sphere
  smoothers (`bs = "sos"`) because
  [`geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  was creating tiles that extended beyond valid geographical coordinates
  for the tile centre coordinates that it was being provided. The
  function that creates the data to evaluate the spline at now tries to
  avoid this. Note this fix is not a complete solution.
  [\#334](https://github.com/gavinsimpson/gratia/issues/334) Reported by
  [@StefanoMezzini](https://github.com/StefanoMezzini)

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  failed to extract the link an inverse link functions for
  [`mvn()`](https://rdrr.io/pkg/mgcv/man/mvn.html) and
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html) models.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work correctly for models fitted with the
  [`ziP()`](https://rdrr.io/pkg/mgcv/man/ziP.html) family.
  [\#341](https://github.com/gavinsimpson/gratia/issues/341) Reported by
  [@rroyaute](https://github.com/rroyaute)

- [`basis.scam()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  was incorrectly trying to identify the smooths named in `select`.

- [`basis.scam()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  would fail if the SCAM contained any non-constrained (i.e.  ordinary)
  smooths.

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  was displaying more significant digits in the p values than it was
  accurate to in terms of reporting p values as “\<0.001”. A new
  argument `digits` controls how many digits are used when formatting p
  values, with default of `3`.

- [`theta()`](https://gavinsimpson.github.io/gratia/reference/theta.md)
  now works for models fitted with the
  [`gfam()`](https://rdrr.io/pkg/mgcv/man/gfam.html) family. Previously
  it would fail because it assumes all `family()$getTheta` functions had
  an argument `trans` to transform the theta values to the correct
  scale.

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  was not showing the model intercept, and was displaying slightly
  different results from those shown by
  [`summary.gam()`](https://rdrr.io/pkg/mgcv/man/summary.gam.html).

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  was not adding any known boundary condition that was part of a soap
  film smooth.

- [`draw.gamlss()`](https://gavinsimpson.github.io/gratia/reference/draw.gamlss.md)
  (for [`GJRM::gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html)
  models) now doesn’t fail with an error if there is more than a single
  smooth in any of the linear predictors.

## gratia 0.10.0

CRAN release: 2024-12-19

### New features

- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  and its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method compute and plot predictions from a fitted GAM that are
  conditional on one or more covariates. The function is a wrapper
  around
  [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  but allows the user simple ways to specify which covariates to
  condition on and at what values those covariates should take. It
  provides similar functionality to
  [`marginaleffects::plot_predictions()`](https://rdrr.io/pkg/marginaleffects/man/plot_predictions.html),
  but is simpler. See
  [\#300](https://github.com/gavinsimpson/gratia/issues/300).

- [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  and
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  can now allow the smooth to be reparameterized such that the resulting
  basis has an identity matrix. This more clearly highlights the penalty
  null space, the functions that the penalty has no effect on.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  gain argument `caption`, which, if set to `FALSE` will not plot the
  smooth basis type as a caption on the plot.
  [\#307](https://github.com/gavinsimpson/gratia/issues/307)

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  and
  [`qq_plot.gam()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  now allow the user to set a random seed that is used when generating
  reference quantiles with `method = "uniform"` or
  `method = "simulate"`.

### Bug fixes

- [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  was ignoring the `scale` argument.
  [\#293](https://github.com/gavinsimpson/gratia/issues/293) Reported by
  [@jonathonmellor](https://github.com/jonathonmellor)

- Argument `level` to
  [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  was included accidentally. As of v0.9.2.9002 this argument is
  deprecated and using it will now generate a warning.
  [\#291](https://github.com/gavinsimpson/gratia/issues/291)

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  was not plotting cyclic P spline smooths. Reported by
  [@Zuckerbrot](https://github.com/Zuckerbrot)
  [\#297](https://github.com/gavinsimpson/gratia/issues/297)

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  would fail for `"fs"` smooths with other parametric effects in the
  model. Reported by [@mahonmb](https://github.com/mahonmb)
  [\#301](https://github.com/gavinsimpson/gratia/issues/301)

- Partial residuals in
  [`partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/partial_residuals.md)
  and
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  were wrong for GAMs fitted with `family = binomial()` where the
  `weights` argument contained the binomial sample sizes because the
  prior weights were being used to form weighted working residuals. Now
  working weights are used instead. Reported by
  [@emchuron](https://github.com/emchuron)
  [\#273](https://github.com/gavinsimpson/gratia/issues/273)

- Internal function `gammals_link()` was expecting `"theta"` as a
  synonym for the scale parameter but the master table has `"phi"` coded
  as the synonym. Now both work as expected.

- [`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  assumed that `level` would have only a single value even though it
  could handle multiple levels.
  [\#321](https://github.com/gavinsimpson/gratia/issues/321)

## gratia 0.9.2

CRAN release: 2024-06-25

### Breaking changes

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  slightly escaped the great renaming that happened for 0.9.0. Columns
  `type` and `term` did not gain a prefix `.`. This is now rectified and
  these two columns are now `.type` and `.term`.

### User visible changes

- Plots of random effects are now labelled with their smooth label.
  Previously, the title was taken fro the variable involved in the
  smooth, but this doesn’t work for terms like
  `s(subject, continuous_var, bs = "re")` for random slopes, which
  previously would have the title `"subject"`. Now such terms will have
  title `"s(subject,continuous_var)"`. Simple random intercept terms,
  `s(subject, bs = "re")`, are now titled `"s(subject)"`.
  [\#287](https://github.com/gavinsimpson/gratia/issues/287)

- The vignettes

  1.  `custom-plotting.Rmd`, and
  2.  `posterior-simulation.Rmd` were moved to `vignettes/articles` and
      thus are no longer available as package vignettes. Instead, they
      are accessible as Articles through the package website:
      <https://gavinsimpson.github.io/gratia/>

### New features

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  now works for [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) models
  with multiple linear predictors, but currently only the location
  parameter is supported. The parameter is indicated through a new
  variable `.parameter` in the returned object.

### Bug fixes

- [`partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/partial_residuals.md)
  was computing partial residuals from the *deviance* residuals. For
  compatibility with
  [`mgcv::plot.gam()`](https://rdrr.io/pkg/mgcv/man/plot.gam.html),
  partial residuals are now computed from the *working* residuals.
  Reported by [@wStockhausen](https://github.com/wStockhausen)
  [\#273](https://github.com/gavinsimpson/gratia/issues/273)

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  was not passing the `ci_col` argument on
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  and
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md).
  Reported by Sate Ahmed.

- Couldn’t pass `mvn_method` on to posterior sampling functions from
  user facing functions
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md),
  [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md),
  and `repsonse_derivatives()`. Reported by
  [@stefgehrig](https://github.com/stefgehrig)
  [\#279](https://github.com/gavinsimpson/gratia/issues/279)

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  works again for quantile GAMs fitted by `qgam()`.

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  was not applying `shift` to the estimate and upper and lower interval.
  [\#280](https://github.com/gavinsimpson/gratia/issues/280) reported by
  [@TIMAVID](https://github.com/TIMAVID) &
  [@rbentham](https://github.com/rbentham)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  and
  [`draw.parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md)
  would forget about the levels of factors (intentionally), but this
  would lead to problems with ordered factors where the ordering of
  levels was not preserved. Now,
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  returns a named list of factor levels as attribute `"factor_levels"`
  containing the required information and the order of levels is
  preserved when plotting.
  [\#284](https://github.com/gavinsimpson/gratia/issues/284) Reported by
  [@mhpob](https://github.com/mhpob)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  would fail if there were parametric terms in the model but they were
  all interaction terms (which we don’t currently handle).
  [\#282](https://github.com/gavinsimpson/gratia/issues/282)

## gratia 0.9.0

CRAN release: 2024-03-27

### Breaking changes

- Many functions now return objects with different named variables. In
  order to avoid clashes with variable names used in user’s models or
  data, a period (`.`) is now being used as a prefix for generated
  variable names. The functions whose names have changed are:
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
  [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md),
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md),
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md),
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md),
  and
  [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md).
  In addition,
  [`add_confint()`](https://gavinsimpson.github.io/gratia/reference/add_confint.md)
  also adds newly-named variables.

  ``` R
  1. `est` is now `.estimate`,
  2. `lower` and `upper` are now `.lower_ci` and `.upper_ci`,
  3. `draw` and `row` and now `.draw` and `.row` respectively,
  4. `fitted`, `se`, `crit` are now `.fitted`, `.se`, `.crit`, respectively
  5. `smooth`, `by`, and `type` in `smooth_estimates()` are now `.smooth`,
     `.by`, `.type`, respectively.
  ```

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  and
  [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  now work more like
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md);
  in place of the `var` and `data` columns, *gratia* now stores the data
  variables at which the derivatives were evaluated as columns in the
  object with their actual variable names.

- The way spline-on-the-sphere (SOS) smooths (`bs = "sos"`) are plotted
  has changed to use
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  instead of the previously-used
  [`ggplot2::coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html).
  This changed has been made as a result of
  [`coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  being soft-deprecated (“superseded”) for a few minor versions of
  ggplot2 by now already, and changes to the guides system in version
  3.5.0 of ggplot2.

  The axes on plots created with
  [`coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  never really worked correctly and changing the angle of the tick
  labels never worked. As
  [`coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  is superseded, it didn’t receive the updates to the guides system and
  a side effect of these changes, the code that plotted SOS smooths was
  producing a warning with the release of ggplot2 version 3.5.0.

  The projection settings used to draw SOS smooths was previously
  controlled via arguments `projection` and `orientation`. These
  arguments do not affect
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  Instead the projection used is controlled through new argument `crs`,
  which takes a PROJ string detailing the projection to use or an
  integer that refers to a known coordinate reference system (CRS). The
  default projection used is `+proj=ortho +lat_0=20 +lon_0=XX` where
  `XX` is the mean of the longitude coordinates of the data points.

#### Defunct and deprecated functions and arguments

##### Defunct

- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  was deprecated in gratia version 0.7.0. This function and all it’s
  methods have been removed from the package. Use
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  instead.

##### Deprecated functions

The following functions were deprecated in version 0.9.0 of gratia. They
will eventually be removed from the package as part of a clean up ahead
of an eventual 1.0.0 release. These functions will become defunct by
version 0.11.0 or 1.0.0, whichever is released soonest.

- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  has been deprecated. Use
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  instead.

- [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  has been deprecated. It never really did what it was originally
  designed to do, and has been replaced by
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md).

##### Deprecated arguments

To make functions in the package more consistent, the arguments
`select`, `term`, and `smooth` are all used for the same thing and hence
the latter two have been deprecated in favour of `select`. If a
deprecated argument is used, a warning will be issued but the value
assigned to the argument will be assigned to `select` and the function
will continue.

### User visible changes

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now uses a single call to the RNG to generate draws from the posterior
  of smooths. Previous to version 0.9.0,
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  would do a separate call to
  [`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html) for
  each smooth. As a result, the result of a call to
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  on a model with multiple smooths will now produce different results to
  those generated previously. To regain the old behaviour, add
  `rng_per_smooth = TRUE` to the
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  call.

  Note, however, that using per-smooth RNG calls with `method = "mh"`
  will be very inefficient as, with that method, posterior draws for all
  coefficients in the model are sampled at once. So, only use
  `rng_per_smooth = TRUE` with `method = "gaussian"`.

- The output of
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  and its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method have changed for tensor product smooths that involve one or
  more 2D marginal smooths. Now, if no covariate values are supplied via
  the `data` argument,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  identifies if one of the marginals is a 2d surface and allows the
  covariates involved in that surface to vary fastest, ahead of terms in
  other marginals. This change has been made as it provides a better
  default when nothing is provided to `data`.

  This also affects
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md).

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now has some level of support for location, scale, shape families.
  Supported families are
  [`mgcv::gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html),
  [`mgcv::gammals()`](https://rdrr.io/pkg/mgcv/man/gammals.html),
  [`mgcv::gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html),
  [`mgcv::gevlss()`](https://rdrr.io/pkg/mgcv/man/gevlss.html),
  [`mgcv::shash()`](https://rdrr.io/pkg/mgcv/man/shash.html),
  [`mgcv::twlss()`](https://rdrr.io/pkg/mgcv/man/twlss.html), and
  [`mgcv::ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html).

- *gratia* now requires *dplyr* versions \>= 1.1.0 and *tidyselect* \>=
  1.2.0.

- A new vignette *Posterior Simulation* is available, which describes
  how to do posterior simulation from fitted GAMs using {gratia}.

### New features

- Soap film smooths using basis `bs = "so"` are now handled by
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md),
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  etc. [\#8](https://github.com/gavinsimpson/gratia/issues/8)

- [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  is a new function for computing derivatives of the response with
  respect to a (continuous) focal variable. First or second order
  derivatives can be computed using forward, backward, or central finite
  differences. The uncertainty in the estimated derivative is determined
  using posterior sampling via
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md),
  and hence can be derived from a Gaussian approximation to the
  posterior or using a Metropolis Hastings sampler (see below.)

- [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  is the work horse function behind
  [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md),
  which computes and returns posterior draws of the derivatives of any
  additive combination of model terms. Requested by
  [@jonathanmellor](https://github.com/jonathanmellor)
  [\#237](https://github.com/gavinsimpson/gratia/issues/237)

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  can now simulate response data from gamma, Tweedie and ordered
  categorical distributions.

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  gains two new example models `"gwf2"`, simulating data only from Gu &
  Wahba’s *f2* function, and `"lwf6"`, example function 6 from Luo &
  Wahba (1997 JASA 92(437), 107-116).

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  can also simulate data for use with GAMs fitted using
  `family = gfam()` for grouped families where different types of data
  in the response are handled.
  [\#266](https://github.com/gavinsimpson/gratia/issues/266) and part of
  [\#265](https://github.com/gavinsimpson/gratia/issues/265)

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  and
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  can now use the Metropolis Hastings sampler from
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html), instead
  of a Gaussian approximation, to sample from the posterior distribution
  of the model or specific smooths respectively.

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  is a new function in the family of
  [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  and
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md).
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  returns draws from the posterior distribution of the response,
  combining the uncertainty in the estimated expected value of the
  response and the dispersion of the response distribution. The
  difference between
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  and
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
  is that the latter only includes variation due to drawing samples from
  the conditional distribution of the response (the uncertainty in the
  expected values is ignored), while the former includes both sources of
  uncertainty.

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  can new use a matrix of user-supplied posterior draws. Related to
  [\#120](https://github.com/gavinsimpson/gratia/issues/120)

- [`add_fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md),
  [`add_predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md),
  [`add_posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md),
  and
  [`add_smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md)
  are new utility functions that add the respective draws from the
  posterior distribution to an existing data object for the covariate
  values in that object: `obj |> add_posterior_draws(model)`.
  [\#50](https://github.com/gavinsimpson/gratia/issues/50)

- [`basis_size()`](https://gavinsimpson.github.io/gratia/reference/basis_size.md)
  is a new function to extract the basis dimension (number of basis
  functions) for smooths. Methods are available for objects that inherit
  from classes `"gam"`, `"gamm"`, and `"mgcv.smooth"` (for individual
  smooths).

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  gains a method for data frames and tibbles.

- [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  gains a method for data frames and tibbles.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now works with models fitted using the
  [`mgcv::ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) family. The
  predicted probability for each category is returned, alongside a Wald
  interval created using the standard error (SE) of the estimated
  probability. The SE and estimated probabilities are transformed to the
  logit (linear predictor) scale, a Wald credible interval is formed,
  which is then back-transformed to the response (probability) scale.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  now works for GAMMs fitted using
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html). Fitted
  (predicted) values only use the GAM part of the model, and thus
  exclude the random effects.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  work for models fitted using the
  [`cnorm()`](https://rdrr.io/pkg/mgcv/man/cnorm.html) family.

- A worm plot can now be drawn in place of the QQ plot with
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  via new argument `use_worm = TRUE`.
  [\#62](https://github.com/gavinsimpson/gratia/issues/62)

- [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md)
  now works for models fitted with
  [`mgcv::gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html).

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  now returns the basis dimension for each smooth and gains an argument
  `stars` which if `TRUE` add significance stars to the output plus a
  legend is printed in the tibble footer. Part of wish of
  [@noamross](https://github.com/noamross)
  [\#214](https://github.com/gavinsimpson/gratia/issues/214)

- New
  [`add_constant()`](https://gavinsimpson.github.io/gratia/reference/add_constant.md)
  and
  [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  methods for
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md).

- [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  gains arguments `lower` and `upper` to modify the lower and / or upper
  bound of the interval over which evenly spaced values will be
  generated.

- [`add_sizer()`](https://gavinsimpson.github.io/gratia/reference/add_sizer.md)
  is a new function to add information on whether the derivative of a
  smooth is significantly changing (where the credible interval excludes
  0). Currently, methods for
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  objects are implemented. Part of request of
  [@asanders11](https://github.com/asanders11)
  [\#117](https://github.com/gavinsimpson/gratia/issues/117)

- [`draw.derivatives()`](https://gavinsimpson.github.io/gratia/reference/draw.derivatives.md)
  gains arguments `add_change` and `change_type` to allow derivatives of
  smooths to be plotted with indicators where the credible interval on
  the derivative excludes 0. Options allow for periods of decrease or
  increase to be differentiated via `change_type = "sizer"` instead of
  the default `change_type = "change"`, which emphasises either type of
  change in the same way. Part of wish of
  [@asanders11](https://github.com/asanders11)
  [\#117](https://github.com/gavinsimpson/gratia/issues/117)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can now group factor by smooths for a given factor into a single
  panel, rather than plotting the smooths for each level in separate
  panels. This is achieved via new argument `grouped_by`. Requested by
  [@RPanczak](https://github.com/RPanczak)
  [\#89](https://github.com/gavinsimpson/gratia/issues/89)

  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  can now also group factor by smooths for a given factor into a single
  panel.

- The underlying plotting code used by `draw_smooth_estimates()` for
  most univariate smooths can now add change indicators to the plots of
  smooths if those change indicators are added to the object created by
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  using
  [`add_sizer()`](https://gavinsimpson.github.io/gratia/reference/add_sizer.md).
  See the example in
  [`?draw.smooth_estimates`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md).

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  can, when evaluating a 3D or 4D tensor product smooth, identify if one
  or more 2D smooths is a marginal of the tensor product. If users do
  not provide covariate values at which to evaluate the smooths,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  will focus on the 2D marginal smooth (or the first if more than one is
  involved in the tensor product), instead of following the ordering of
  the terms in the definition of the tensor product.
  [\#191](https://github.com/gavinsimpson/gratia/issues/191)

  For example, in `te(z, x, y, bs = c(cr, ds), d = c(1, 2))`, the second
  marginal smooth is a 2D Duchon spline of covariates `x` and `y`.
  Previously,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  would have generated `n` values each for `z` and `x` and `n_3d` values
  for `y`, and then evaluated the tensor product at all combinations of
  those generated values. This would ignore the structure implicit in
  the tensor product, where we are likely to want to know how the
  surface estimated by the Duchon spline of `x` and `y` smoothly varies
  with `z`. Previously
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  would generate surfaces of `z` and `x`, varying by `y`. Now,
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  correctly identifies that one of the marginal smooths of the tensor
  product is a 2D surface and will focus on that surface varying with
  the other terms in the tensor product.

  This improved behaviour is needed because in some
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) models it is not
  always possible to do the obvious thing and reorder the smooths when
  defining the tensor product to be
  `te(x, y, z, bs = c(ds, cr), d = c(2, 1))`. When `discrete = TRUE` is
  used with [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) the terms
  in the tensor product may get rearranged during model setup for
  maximum efficiency (See *Details* in
  [`?mgcv::bam`](https://rdrr.io/pkg/mgcv/man/bam.html)).

  Additionally,
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now also works the same way.

- New function
  [`null_deviance()`](https://gavinsimpson.github.io/gratia/reference/null_deviance.md)
  that extracts the null deviance of a fitted model.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md),
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
  [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md),
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md),
  and
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now all work for models fitted with
  [`scam::scam()`](https://rdrr.io/pkg/scam/man/scam.html). Where it
  matters, current support extends only to univariate smooths.

- [`generate_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
  is a new low-level function for generating posterior draws from fitted
  model coefficients. `generate_daws()` is an S3 generic function so is
  extensible by users. Currently provides a simple interface to a simple
  Gaussian approximation sampler
  ([`gaussian_draws()`](https://gavinsimpson.github.io/gratia/reference/gaussian_draws.md))
  and the simple Metropolis Hastings sample
  ([`mh_draws()`](https://gavinsimpson.github.io/gratia/reference/mh_draws.md))
  available via
  [`mgcv::gam.mh()`](https://rdrr.io/pkg/mgcv/man/gam.mh.html).
  [\#211](https://github.com/gavinsimpson/gratia/issues/211)

- [`smooth_label()`](https://gavinsimpson.github.io/gratia/reference/smooth_label.md)
  is a new function for extracting the labels ‘mgcv’ creates for smooths
  from the smooth object itself.

- [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  has a default method that works with
  [`s()`](https://rdrr.io/pkg/mgcv/man/s.html),
  [`te()`](https://rdrr.io/pkg/mgcv/man/te.html),
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html), and
  [`ti()`](https://rdrr.io/pkg/mgcv/man/te.html), which create a smooth
  specification.

- [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  gains argument `constant` to allow for the addition of a constant
  value to objects (e.g. the estimate and confidence interval). This
  enables a single `obj |> transform_fun(fun = exp, constant = 5)`
  instead of separate calls to
  [`add_constant()`](https://gavinsimpson.github.io/gratia/reference/add_constant.md)
  and then
  [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md).
  Part of the discussion of
  [\#79](https://github.com/gavinsimpson/gratia/issues/79)

- [`model_constant()`](https://gavinsimpson.github.io/gratia/reference/model_constant.md)
  is a new function that simply extracts the first coefficient from the
  estimated model.

### Bug fixes

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  and related family functions for the
  [`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) weren’t correctly
  identifying the family name and as a result would throw an error even
  when passed an object of the correct family.

  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work correctly for the
  [`betar()`](https://rdrr.io/pkg/mgcv/man/Beta.html) family in a fitted
  GAM.

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`lp_matrix()`](https://gavinsimpson.github.io/gratia/reference/lp_matrix.md)
  now converts the matrix to a data frame before conversion to a tibble.
  This makes more sense as it results in more typical behaviour as the
  columns of the printed object are doubles.

- Constrained factor smooths (`bs = "sz"`) where the factor is not the
  first variable mentioned in the smooth (i.e. `s(x, f, bs = "sz")` for
  continuous `x` and factor `f`) are now plottable with
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md).
  [\#208](https://github.com/gavinsimpson/gratia/issues/208)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  was unable to handle special parametric terms like `poly(x)` or
  `log(x)` in formulas. Reported by [@fhui28](https://github.com/fhui28)
  [\#212](https://github.com/gavinsimpson/gratia/issues/212)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  now works better for location, scale, shape models. Reported by
  [@pboesu](https://github.com/pboesu)
  [\#45](https://github.com/gavinsimpson/gratia/issues/45)

- `parametric_effects` now works when there are missing values in one or
  more variables used in a fitted GAM.
  [\#219](https://github.com/gavinsimpson/gratia/issues/219)

- [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  was incorrectly using `.data` with *tidyselect* selectors.

- [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  could not handle logical variables in a GAM fit as mgcv stores these
  as numerics in the `var.summary`. This affected
  [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  and
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md).
  [\#222](https://github.com/gavinsimpson/gratia/issues/222)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  would fail when two or more ordered factors were in the model.
  Reported by [@dsmi31](https://github.com/dsmi31)
  [\#221](https://github.com/gavinsimpson/gratia/issues/221)

- Continuous by smooths were being evaluated with the median value of
  the `by` variable instead of a value of 1.
  [\#224](https://github.com/gavinsimpson/gratia/issues/224)

- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  (and hence
  [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md))
  now handles models with offset terms in the formula. Offset terms
  supplied via the `offset` argument are ignored by
  `mgcv:::predict.gam()` and hence are ignored also by `gratia`.
  Reported by [@jonathonmellor](https://github.com/jonathonmellor)
  [\#231](https://github.com/gavinsimpson/gratia/issues/231)
  [\#233](https://github.com/gavinsimpson/gratia/issues/233)

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  would fail on a `"fs"` smooth when a multivariate base smoother was
  used *and* the factor was not the last variable specified in the
  definition of the smooth:
  `s(x1, x2, f, bs = "fs", xt = list(bs = "ds"))` would work, but
  `s(f, x1, x2, bs = "fs", xt = list(bs = "ds"))` (or any ordering of
  variables that places the factor not last) would emit an obscure
  error. The ordering of the terms involved in the smooth now doesn’t
  matter. Reported by [@chrisaak](https://github.com/chrisaak)
  [\#249](https://github.com/gavinsimpson/gratia/issues/249).

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would fail when plotting a multivariate base smoother used in an
  `"sz"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn’t know how to plot such a
  smooth. Reported by [@chrisaak](https://github.com/chrisaak)
  [\#249](https://github.com/gavinsimpson/gratia/issues/249).

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would fail when plotting a multivariate base smoother used in an
  `"fs"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn’t know how to plot such a
  smooth. Reported by [@chrisaak](https://github.com/chrisaak)
  [\#249](https://github.com/gavinsimpson/gratia/issues/249).

- [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  would fail with `order = 2` and was only computing forward finite
  differences, regardless of `type` for `order = 1`. Partly reported by
  [@samlipworth](https://github.com/samlipworth)
  [\#251](https://github.com/gavinsimpson/gratia/issues/251).

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  was normalizing the penalty to the range 0–1, not the claimed and
  documented -1–1 with argument `normalize = TRUE`. This is now fixed.

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  was failing when `data` was supplied that contained more variables
  than were used in the smooth that was being sampled. Hence this
  generally fail unless a single smooth was being sampled from or the
  model contained only a single smooth. The function never intended to
  retain all the variables in `data` but was written in such a way that
  it would fail when relocating the data columns to the end of the
  posterior sampling object.
  [\#255](https://github.com/gavinsimpson/gratia/issues/255)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  would fail when plotting a univariate tensor product smooth
  (e.g. `te(x)`, `ti(x)`, or
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html)). Reported by
  [@wStockhausen](https://github.com/wStockhausen)
  [\#260](https://github.com/gavinsimpson/gratia/issues/260)

- `plot.smooth()` was not printing the factor level in subtitles for
  ordered factor by smooths.

## gratia 0.8.2

CRAN release: 2024-01-09

- Small fixes for CRAN.

## gratia 0.8.1

CRAN release: 2023-02-02

### User visible changes

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now returns objects with variables involved in smooths that have their
  correct name. Previously variables were named `.x1`, `.x2`, etc.
  Fixing [\#126](https://github.com/gavinsimpson/gratia/issues/126) and
  improving compatibility with
  [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  allowed the variables to be named correctly.

- *gratia* now depends on version 1.8-41 or later of the *mgcv* package.

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can now handle tensor products that include a marginal random effect
  smooth. Beware plotting such smooths if there are many levels,
  however, as a separate surface plot will be produced for each level.

### Bug fixes

- Additional fixes for changes in dplyr 1.1.0.

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  now works when sampling from posteriors of multiple smooths with
  different dimension.
  [\#126](https://github.com/gavinsimpson/gratia/issues/126) reported by
  [@Aariq](https://github.com/Aariq)

## gratia 0.8.0

### User visible changes

- {gratia} now depends on R version 4.1 or later.

- A new vignette “Data slices” is supplied with {gratia}.

- Functions in {gratia} have harmonised to use an argument named `data`
  instead of `newdata` for passing new data at which to evaluate
  features of smooths. A message will be printed if `newdata` is used
  from now on. Existing code does not need to be changed as `data` takes
  its value from `newdata`.

  Note that due to the way `...` is handled in R, if your R script uses
  the `data` argument, *and* is run with versions of gratia prior to 8.0
  (when released; 0.7.3.8 if using the development version) the
  user-supplied data will be silently ignored. As such, scripts using
  `data` should check that the installed version of gratia is \>= 0.8
  and package developers should update to depend on versions \>= 0.8 by
  using `gratia (>= 0.8)` in `DESCRIPTION`.

- The order of the plots of smooths has changed in
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  so that they again match the order in which smooths were specified in
  the model formula. See *Bug Fixes* below for more detail or
  [\#154](https://github.com/gavinsimpson/gratia/issues/154).

### New features

- Added basic support for GAMLSS (distributional GAMs) fitted with the
  [`gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html) function from
  package GJRM. Support is currently restricted to a
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method.

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  can now include the group means in the difference, which many users
  expected. To include the group means use `group_means = TRUE` in the
  function call, e.g.
  `difference_smooths(model, smooth = "s(x)", group_means = TRUE`).
  Note: this function still differs from `plot_diff()` in package
  *itsadug*, which essentially computes differences of model
  predictions. The main practical difference is that other effects
  beyond the factor by smooth, including random effects, may be included
  with `plot_diff()`.

  This implements the main wish of
  [\#108](https://github.com/gavinsimpson/gratia/issues/108)
  ([@dinga92](https://github.com/dinga92)) and
  [\#143](https://github.com/gavinsimpson/gratia/issues/143)
  ([@mbolyanatz](https://github.com/mbolyanatz)) despite my
  protestations that this was complicated in some cases (it isn’t; the
  complexity just cancels out.)

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  has been totally revised. Now, the user provides the values for the
  variables they want in the slice and any variables in the model that
  are not specified will be held at typical values (i.e. the value of
  the observation that is closest to the median for numeric variables,
  or the modal factor level.)

  Data slices are now produced by passing `name` = `value` pairs for the
  variables and their values that you want to appear in the slice. For
  example

      m <- gam(y ~ s(x1) + x2 + fac)
      data_slice(model, x1 = evenly(x1, n = 100), x2 = mean(x2))

  The `value` in the pair can be an expression that will be looked up
  (evaluated) in the `data` argument or the model frame of the fitted
  model (the default). In the above example, the resulting slice will be
  a data frame of 100 observations, comprising `x1`, which is a vector
  of 100 values spread evenly over the range of `x1`, a constant value
  of the mean of `x2` for the `x2` variable, and a constant factor
  level, the model class of `fac`, for the `fac` variable of the model.

- [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  is a new function for computing partial derivatives of multivariate
  smooths (e.g. `s(x,z)`, `te(x,z)`) with respect to one of the margins
  of the smooth. Multivariate smooths of any dimension are handled, but
  only one of the dimensions is allowed to vary. Partial derivatives are
  estimated using the method of finite differences, with forward,
  backward, and central finite differences available. Requested by
  [@noamross](https://github.com/noamross)
  [\#101](https://github.com/gavinsimpson/gratia/issues/101)

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  provides a simple overview of model terms for fitted GAMs.

- The new `bs = "sz"` basis that was released with *mgcv* version
  1.18-41 is now supported in
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md),
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  and this basis has its own unique plotting method.
  [\#202](https://github.com/gavinsimpson/gratia/issues/202)

- [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  now has a method for fitted GAM(M)s which can extract the estimated
  basis from the model and plot it, using the estimated coefficients for
  the smooth to weight the basis.
  [\#137](https://github.com/gavinsimpson/gratia/issues/137)

  There is also a new
  [`draw.basis()`](https://gavinsimpson.github.io/gratia/reference/draw.basis.md)
  method for plotting the results of a call to
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md).
  This method can now also handle bivariate bases.

  [`tidy_basis()`](https://gavinsimpson.github.io/gratia/reference/tidy_basis.md)
  is a lower level function that does the heavy lifting in
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md),
  and is now exported.
  [`tidy_basis()`](https://gavinsimpson.github.io/gratia/reference/tidy_basis.md)
  returns a tidy representation of a basis supplied as an object
  inheriting from class `"mgcv.smooth"`. These objects are returned in
  the `$smooth` component of a fitted GAM(M) model.

- [`lp_matrix()`](https://gavinsimpson.github.io/gratia/reference/lp_matrix.md)
  is a new utility function to quickly return the linear predictor
  matrix for an estimated model. It is a wrapper to
  `predict(..., type = "lpmatrix")`

- [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  is a synonym for
  [`seq_min_max()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  and is preferred going forward. Gains argument `by` to produce
  sequences over a covariate that increment in units of `by`.

- [`ref_level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  and
  [`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  are new utility functions for extracting the reference or a specific
  level of a factor respectively. These will be most useful when
  specifying covariate values to condition on in a data slice.

- [`model_vars()`](https://gavinsimpson.github.io/gratia/reference/model_vars.md)
  is a new, public facing way of returning a vector of variables that
  are used in a model.

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  will now use the user-supplied data as points at which to evaluate a
  pair of smooths. Also note that the argument `newdata` has been
  renamed `data`.
  [\#175](https://github.com/gavinsimpson/gratia/issues/175)

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  now uses better labels for plot titles to avoid long labels with even
  modest factor levels.

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now works for factor-smooth interaction (`"fs"`) smooths.

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  methods now allow the angle of tick labels on the x axis of plots to
  be rotated using argument `angle`. Requested by
  [@tamas-ferenci](https://github.com/tamas-ferenci)
  [\#87](https://github.com/gavinsimpson/gratia/issues/87)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and related functions
  ([`draw.parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md),
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md))
  now add the basis to the plot using a caption.
  [\#155](https://github.com/gavinsimpson/gratia/issues/155)

- [`smooth_coefs()`](https://gavinsimpson.github.io/gratia/reference/smooth_coefs.md)
  is a new utility function for extracting the coefficients for a
  particular smooth from a fitted model.
  [`smooth_coef_indices()`](https://gavinsimpson.github.io/gratia/reference/smooth_coef_indices.md)
  is an associated function that returns the indices (positions) in the
  vector of model coefficients (returned by `coef(gam_model)`) of those
  coefficients that pertain to the stated smooth.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now better handles patchworks of plots where one or more of those
  plots has fixed aspect ratios.
  [\#190](https://github.com/gavinsimpson/gratia/issues/190)

### Bug fixes

- `draw.posterior_smooths` now plots posterior samples with a fixed
  aspect ratio if the smooth is isotropic.
  [\#148](https://github.com/gavinsimpson/gratia/issues/148)

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now ignores random effect smooths (for which derivatives don’t make
  sense anyway).
  [\#168](https://github.com/gavinsimpson/gratia/issues/168)

- `confint.gam(...., method = "simultaneous")` now works with factor by
  smooths where `parm` is passed the full name of a specific smooth
  `s(x)faclevel`.

- The order of plots produced by
  [`gratia::draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  again matches the order in which the smooths entered the model
  formula. Recent changes to the internals of
  [`gratia::draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  when the switch to
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  was undertaken lead to a change in behaviour resulting from the use of
  [`dplyr::group_split()`](https://dplyr.tidyverse.org/reference/group_split.html),
  and it’s coercion internally of a character vector to a factor. This
  factor is now created explicitly, and the levels set to the correct
  order. [\#154](https://github.com/gavinsimpson/gratia/issues/154)

- Setting the `dist` argument to set response or smooth values to `NA`
  if they lay too far from the support of the data in multivariate
  smooths, this would lead an incorrect scale for the response guide.
  This is now fixed.
  [\#193](https://github.com/gavinsimpson/gratia/issues/193)

- Argument `fun` to
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  was not being applied to any parametric terms. Reported by
  [@grasshoppermouse](https://github.com/grasshoppermouse)
  [\#195](https://github.com/gavinsimpson/gratia/issues/195)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  was adding the uncertainty for all linear predictors to smooths when
  `overall_uncertainty = TRUE` was used. Now
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  only includes the uncertainty for those linear predictors in which a
  smooth takes part.
  [\#158](https://github.com/gavinsimpson/gratia/issues/158)

- [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  works when provided with a single data point at which to evaluate the
  derivative. [\#199](https://github.com/gavinsimpson/gratia/issues/199)

- [`transform_fun.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  was addressing the wrong variable names when trying to transform the
  confidence interval.
  [\#201](https://github.com/gavinsimpson/gratia/issues/201)

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  doesn’t fail with an error when used with a model that contains an
  offset term.
  [\#198](https://github.com/gavinsimpson/gratia/issues/198)

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  no longer uses
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md),
  which is soft deprecated.
  [\#167](https://github.com/gavinsimpson/gratia/issues/167)

- [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  and
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  could compute the wrong deviance residuals used to generate the
  theoretical quantiles for some of the more exotic families
  (distributions) available in *mgcv*. This also affected
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  but only for the QQ plot; the residuals shown in the other plots and
  the deviance residuals shown on the y-axis of the QQ plot were
  correct. Only the generation of the reference intervals/quantiles was
  affected.

## gratia 0.7.3

CRAN release: 2022-05-09

### User visible changes

- Plots of smooths now use “Partial effect” for the y-axis label in
  place of “Effect”, to better indicate what is displayed.

### New features

- [`confint.fderiv()`](https://gavinsimpson.github.io/gratia/reference/confint.fderiv.md)
  and
  [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  now return their results as a tibble instead of a common-or-garden
  data frame. The latter mostly already did this.

- Examples for
  [`confint.fderiv()`](https://gavinsimpson.github.io/gratia/reference/confint.fderiv.md)
  and
  [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  were reworked, in part to remove some inconsistent output in the
  examples when run on M1 macs.

### Bug fixes

- [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
  failed when passed non-standard model “names” like
  `compare_smooths(m_gam, m_gamm$gam)` or
  `compare_smooths(l[[1]], l[[2]])` even if the evaluated objects were
  valid GAM(M) models. Reported by Andrew Irwin
  [\#150](https://github.com/gavinsimpson/gratia/issues/150)

## gratia 0.7.2

CRAN release: 2022-03-17

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  can now handle splines on the sphere (`s(lat, long, bs = "sos")`) with
  special plotting methods using
  [`ggplot2::coord_map()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  to handle the projection to spherical coordinates. An orthographic
  projection is used by default, with an essentially arbitrary (and
  northern hemisphere-centric) default for the orientation of the view.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  insures that `data` (and hence the returned object) is a tibble rather
  than a common or garden data frame.

### Bug fixes

- `draw.posterior_smooths()` was redundantly plotting duplicate data in
  the rug plot. Now only the unique set of covariate values are used for
  drawing the rug.

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  was not passing the `scale` argument in the bivariate example setting
  (`"eg2"`).

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  methods for [`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) and
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) fits were
  not passing arguments on to
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md).

- [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  would produce a subtitle with data for a continuous by smooth as if it
  were a factor by smooth. Now the subtitle only contains the name of
  the continuous by variable.

## gratia 0.7.1

Due to an issue with the size of the package source tarball, which
wasn’t discovered until after submission to CRAN, 0.7.1 was never
released.

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md):
  {gratia} can now handle smooths of 3 or 4 covariates when plotting.
  For smooths of 3 covariates, the third covariate is handled with
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  and a set (default `n` = 16) of small multiples is drawn, each a 2d
  surface evaluated at the specified value of the third covariate. For
  smooths of 4 covariates,
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  is used to draw the small multiples, with the default producing 4 rows
  by 4 columns of plots at the specific values of the third and fourth
  covariates. The number of small multiples produced is controlled by
  new arguments `n_3d` (default = `n_3d = 16`) and `n_4d` (default
  `n_4d = 4`, yielding `n_4d * n_4d` = 16 facets) respectively.

  This only affects plotting;
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  has been able to handle smooths of any number of covariates for a
  while.

  When handling higher-dimensional smooths, actually drawing the plots
  on the default device can be slow, especially with the default value
  of `n = 100` (which for 3D or 4D smooths would result in 160,000 data
  points being plotted). As such it is recommended that you reduce `n`
  to a smaller value: `n = 50` is a reasonable compromise of resolution
  and speed.

- [`model_concurvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md)
  returns concurvity measures from
  [`mgcv::concurvity()`](https://rdrr.io/pkg/mgcv/man/concurvity.html)
  for estimated GAMs in a tidy format. The synonym
  [`concrvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md)
  is also provided. A
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method is provided which produces a bar plot or a heatmap of the
  concurvity values depending on whether the overall concurvity of each
  smooth or the pairwise concurvity of each smooth in the model is
  requested.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  gains argument `resid_col = "steelblue3"` that allows the colour of
  the partial residuals (if plotted) to be changed.

### Bug fixes

- [`model_edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  was not using the `type` argument. As a result it only ever returned
  the default EDF type.

- [`add_constant()`](https://gavinsimpson.github.io/gratia/reference/add_constant.md)
  methods weren’t applying the constant to all the required variables.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md),
  [`draw.parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md)
  now actually work for a model with only parametric effects.
  [\#142](https://github.com/gavinsimpson/gratia/issues/142) Reported by
  [@Nelson-Gon](https://github.com/Nelson-Gon)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  would fail for a model with only parametric terms because
  [`predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
  returns empty arrays when passed `exclude = character(0)`.

## gratia 0.7.0

CRAN release: 2022-02-07

### Major changes

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now uses
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  internally and consequently uses its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method and underlying plotting code. This has simplified the code
  compared to
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  and its methods, which will allow for future development and addition
  of features more easily than if
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  had been retained.

  Similarly, `evaluate_parametric_terms()` is now deprecated in favour
  of
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md),
  which is also used internally by
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  if parametric terms are present in the model (and
  `parametric = TRUE`).

  While a lot of code has been reused so differences between plots as a
  result of this change should be minimal, some corner cases may have
  been missed. File an Issue if you notice something that has changed
  that you think shouldn’t.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  now plots 2D isotropic smooths (TPRS and Duchon splines) with
  equally-scaled x and y coordinates using `coord_equal(ratio = 1)`.
  Alignment of these plots will be a little different now when plotting
  models with multiple smooths. See Issue
  [\#81](https://github.com/gavinsimpson/gratia/issues/81).

#### Deprecated functions

From version 0.7.0, the following functions are considered deprecated
and their use is discouraged:

- [`fderiv()`](https://gavinsimpson.github.io/gratia/reference/fderiv.md)
  is *soft*-deprecated in favour of
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md),
- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  is *soft*-deprecated in favour of
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  is *soft*-deprecated in favour of
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md).

The first call to one of these functions will generate a warning,
pointing to the newer, alternative, function. It is safe to ignore these
warnings, but these deprecated functions will no longer receive updates
and are thus at risk of being removed from the package at some future
date. The newer alternatives can handle more types of models and
smooths, especially so in the case of
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md).

### New features

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  provides a tidy wrapper around
  [`predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html) for
  generating fitted values from the model. New covariate values can be
  provided via argument `data`. A credible interval on the fitted values
  is returned, and values can be on the link (linear predictor) or
  response scale.

  Note that this function returns expected values of the response.
  Hence, “fitted values” is used instead of “predictions” in the case of
  new covariate values to differentiate these values from the case of
  generating new response values from a fitted model.

- [`rootogram()`](https://gavinsimpson.github.io/gratia/reference/rootogram.md)
  and its
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method produce rootograms as diagnostic plots for fitted models.
  Currently only for models fitted with
  [`poisson()`](https://rdrr.io/r/stats/family.html),
  [`nb()`](https://rdrr.io/pkg/mgcv/man/negbin.html),
  [`negbin()`](https://rdrr.io/pkg/mgcv/man/negbin.html),
  [`gaussian()`](https://rdrr.io/r/stats/family.html) families.

- New helper functions
  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md),
  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  and
  [`data_combos()`](https://gavinsimpson.github.io/gratia/reference/data_combos.md)
  for quickly creating data sets for producing predictions from fitted
  models where some covariates are fixed at come typical or
  representative values.

  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  is a new helper function to return typical values for the covariates
  of a fitted model. It returns the value of the observation closest to
  the median for numerical covariates or the modal level of a factor
  while preserving the levels of that factor.
  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  is useful in preparing data slices or scenarios for which fitted
  values from the estimated model are required.

  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  extracts and returns the combinations of levels of factors found in
  data used to fit a model. Unlike
  [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md),
  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  returns all the combinations of factor levels observed in the data,
  not just the modal level. Optionally, all combinations of factor
  levels can be returned, not just those in the observed data.

  [`data_combos()`](https://gavinsimpson.github.io/gratia/reference/data_combos.md)
  combines returns the factor data from
  [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  plus the typical values of numerical covariates. This is useful if you
  want to generate predictions from the model for each combination of
  factor terms while holding any continuous covariates at their median
  values.

- [`nb_theta()`](https://gavinsimpson.github.io/gratia/reference/nb_theta.md)
  is a new extractor function that returns the theta parameter of a
  fitted negative binomial GAM (families
  [`nb()`](https://rdrr.io/pkg/mgcv/man/negbin.html) or
  [`negbin()`](https://rdrr.io/pkg/mgcv/man/negbin.html)). Additionally,
  [`theta()`](https://gavinsimpson.github.io/gratia/reference/theta.md)
  and
  [`has_theta()`](https://gavinsimpson.github.io/gratia/reference/has_theta.md)
  provide additional functionality.
  [`theta()`](https://gavinsimpson.github.io/gratia/reference/theta.md)
  is an experimental function for extracting any additional parameters
  from the model or family.
  [`has_theta()`](https://gavinsimpson.github.io/gratia/reference/has_theta.md)
  is useful for checking if any additional parameters are available from
  the family or model.

- [`edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  extracts the effective degrees of freedom (EDF) of a fitted model or a
  specific smooth in the model. Various forms for the EDF can be
  extracted.

- [`model_edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  returns the EDF of the overall model. If supplied with multiple
  models, the EDFs of each model are returned for comparison.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can now show a “rug” plot on a bivariate smooth by drawing small
  points with high transparency over the smooth surface at the data
  coordinates.

  In addition, the rugs on plots of factor by smooths now show the
  locations of covariate values for the specific level of the factor and
  not over all levels. This better reflects what data were used to
  estimate the smooth, even though the basis for each smooth was set up
  using all of the covariate locations.

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  now allow some aspects of the plot to be changed: the fill (but not
  colour) and alpha attributes of the credible interval, and the line
  colour for the smooth can now be specified using arguments `ci_col`,
  `ci_alpha`, and `smooth_col` respectively.

- Partial residuals can now be plotted on factor by smooths. To allow
  this, the partial residuals are filtered so that only residuals
  associated with a particular level’s smooth are drawn on the plot of
  the smooth.

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  uses
  [`check_user_select_smooths()`](https://gavinsimpson.github.io/gratia/reference/check_user_select_smooths.md)
  to handle user-specified selection of smooth terms. As such it is more
  flexible than previously, and allows for easier selection of smooths
  to evaluate.

- [`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) is now
  imported (and re-exported) from the *nlme* package, with methods for
  models fitted with [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html)
  and [`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html), to extract
  fixed effects estimates from fitted models.
  [`fixed_effects()`](https://gavinsimpson.github.io/gratia/reference/fixed_effects.gam.md)
  is an alias for
  [`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html).

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  can now handle 2D smooths. Additionally, the number of posterior draws
  to plot can now be specified when plotting using new argument
  `n_samples`, which will result in `n_samples` draws being selected at
  random from the set of draws for plotting. New argument `seed` allows
  the selection of draws to be repeatable.

### Bug fixes

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  was not filtering user-supplied data for the by level of the specific
  smooth when used with by factor smooths. This would result in the
  smooth being evaluated at all rows of the user-supplied data, and
  therefore would result in `nrow(user_data) * nlevels(by_variable)`
  rows in the returned object instead of `nrow(user_data)` rows.

- The
  [`add_confint()`](https://gavinsimpson.github.io/gratia/reference/add_confint.md)
  method for
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  had the upper and lower intervals reversed.
  [\#107](https://github.com/gavinsimpson/gratia/issues/107) Reported by
  [@Aariq](https://github.com/Aariq)

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  were both ignoring the `dist` argument that allows covariate values
  that lie too far from the support of the data to be excluded when
  returning estimated values from the smooth and plotting it.
  [\#111](https://github.com/gavinsimpson/gratia/issues/111) Reported by
  [@Aariq](https://github.com/Aariq)

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  with a factor by GAM would return samples for the first factor level
  only. Reported by [@rroyaute](https://github.com/rroyaute) in
  discussion of
  [\#121](https://github.com/gavinsimpson/gratia/issues/121)

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  would fail if the model contained random effect “smooths”. These are
  now ignored with a message when running
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md).
  Reported by [@isabellaghement](https://github.com/isabellaghement) in
  [\#121](https://github.com/gavinsimpson/gratia/issues/121)

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  were failing on models fitted with `family = scat()`. Reported by
  [@Aariq](https://github.com/Aariq)
  [\#130](https://github.com/gavinsimpson/gratia/issues/130)

## gratia 0.6.0

CRAN release: 2021-04-18

### Major changes

- The {cowplot} package has been replaced by the {patchwork} package for
  producing multi-panel figures in
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  and
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md).
  This shouldn’t affect any code that used {gratia} only, but if you
  passed additional arguments to
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html)
  or used the `align` or `axis` arguments of
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  and
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
  you’ll need to adapt code accordingly.

  Typically, you can simply delete the `align` or `axis` arguments and
  {patchwork} will just work and align plots nicely. Any arguments
  passed via `...` to
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html)
  will just be ignored by
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  unless those passed arguments match any of the arguments of
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

### New features

- The {patchwork} package is now used for multi-panel figures. As such,
  {gratia} no longer Imports from the {cowplot} package.

- Worm plot diagnostic plots are available via new function
  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md).
  Worm plots are detrended Q-Q plots, where deviation from the Q-Q
  reference line are emphasized as deviations around the line occupy the
  full height of the plot.

  [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  methods are available for models of classes `"gam"`, `"glm"`, and
  `"lm"`. ([\#62](https://github.com/gavinsimpson/gratia/issues/62))

- Smooths can now be compared across models using
  [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md),
  and comparisons visualised with the associated
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method. ([\#85](https://github.com/gavinsimpson/gratia/issues/85)
  [@dill](https://github.com/dill))

  This feature is a bit experimental; the returned object uses nested
  lists and may change in the future if users find this confusing.

- The reference line in
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  with `method = "normal"` was previously drawn as a line with intercept
  0 and slope 1, to match the other methods. This was inconsistent with
  [`stats::qqplot()`](https://rdrr.io/r/stats/qqnorm.html) which drew
  the line through the 1st and 3rd quartiles.
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  with `method = "normal"` now uses this robust reference line.
  Reference lines for the other methods remain drawn with slope 1 and
  intercept 0.

- [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  with `method = "normal"` now draws a point-wise reference band using
  the standard error of the order statistic.

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  now plots the penalty matrix heatmaps in a more-logical orientation,
  to match how the matrices might be written down or printed to the R
  console.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work for models fitted with the
  [`gumbls()`](https://rdrr.io/pkg/mgcv/man/gumbls.html) and
  [`shash()`](https://rdrr.io/pkg/mgcv/man/shash.html) families.
  ([\#84](https://github.com/gavinsimpson/gratia/issues/84))

- [`extract_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  is a lower level utility function related to
  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  and is now exported.

### User visible changes

- The default method name for generating reference quantiles in
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  was changed from `"direct"` to `"uniform"`, to avoid confusion with
  the [`mgcv::qq.gam()`](https://rdrr.io/pkg/mgcv/man/qq.gam.html) help
  page description of the methods. Accordingly using `method = "direct"`
  is deprecated and a message to this effect is displayed if used.

- The way smooths/terms are selected in
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  has been switched to use the same mechanism as
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)’s
  `select` argument. To get a partial match to `term`, you now need to
  also specify `partial_match = TRUE` in the call to
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md).

### Bug fixes

- [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  had a copy paste bug in the definition of the then generic.
  ([\#96](https://github.com/gavinsimpson/gratia/issues/96)
  [@Aariq](https://github.com/Aariq))

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  with user-supplied `newdata` would fail for factor by smooths with
  `interval = "simultaneous"` and would introduce rows with derivative
  == 0 with `interval = "confidence"` because it didn’t subset the rows
  of `newdata` for the specific level of the by factor when computing
  derivatives.
  ([\#102](https://github.com/gavinsimpson/gratia/issues/102)
  [@sambweber](https://github.com/sambweber))

- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  can now handle random effect smooths defined using an ordered factor.
  ([\#99](https://github.com/gavinsimpson/gratia/issues/99)
  [@StefanoMezzini](https://github.com/StefanoMezzini))

## gratia 0.5.1

CRAN release: 2021-01-23

### New features

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  can now handle

  - bivariate and multivariate thinplate regression spline smooths,
    e.g.  `s(x, z, a)`,
  - tensor product smooths
    ([`te()`](https://rdrr.io/pkg/mgcv/man/te.html),
    [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html), &
    [`ti()`](https://rdrr.io/pkg/mgcv/man/te.html)), e.g. `te(x, z, a)`
  - factor smooth interactions, e.g. `s(x, f, bs = "fs")`
  - random effect smooths, e.g. `s(f, bs = "re")`

- [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  provides a tidy representation of the penalty matrices of smooths. The
  tidy representation is most suitable for plotting with
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method is provided, which represents the penalty matrix as a heatmap.

### User visible changes

- The `newdata` argument to
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  has been changed to `data` as was originally intended.

## gratia 0.5.0

CRAN release: 2021-01-10

### New features

- Partial residuals for models can be computed with
  [`partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/partial_residuals.md).
  The partial residuals are the weighted residuals of the model added to
  the contribution of each smooth term (as returned by
  `predict(model, type = "terms")`.

  Wish of [\#76](https://github.com/gavinsimpson/gratia/issues/76)
  ([@noamross](https://github.com/noamross))

  Also, new function
  [`add_partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/add_partial_residuals.md)
  can be used to add the partial residuals to data frames.

- Users can now control to some extent what colour or fill scales are
  used when plotting smooths in those
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  methods that use them. This is most useful to change the fill scale
  when plotting 2D smooths, or to change the discrete colour scale used
  when plotting random factor smooths (`bs = "fs"`).

  The user can pass scales via arguments `discrete_colour` and
  `continuous_fill`.

- The effects of certain smooths can be excluded from data simulated
  from a model using
  [`simulate.gam()`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  and
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
  by passing `exclude` or `terms` on to
  [`predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html). This
  allows for excluding random effects, for example, from model predicted
  values that are then used to simulate new data from the conditional
  distribution. See the example in
  [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md).

  Wish of [\#74](https://github.com/gavinsimpson/gratia/issues/74)
  ([@hgoldspiel](https://github.com/hgoldspiel))

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  and related functions gain arguments `constant` and `fun` to allow for
  user-defined constants and transformations of smooth estimates and
  confidence intervals to be applied.

  Part of wish of Wish of
  [\#79](https://github.com/gavinsimpson/gratia/issues/79).

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  now works for 2D smooths also.

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  is an early version of code to replace (or more likely supersede)
  [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md).
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  can currently only handle 1D smooths of the standard types.

### User visible changes

- The meaning of `parm` in `confint.gam` has changed. This argument now
  requires a smooth label to match a smooth. A vector of labels can be
  provided, but partial matching against a smooth label only works with
  a single `parm` value.

  The default behaviour remains unchanged however; if `parm` is `NULL`
  then all smooths are evaluated and returned with confidence intervals.

- `data_class()` is no longer exported; it was only ever intended to be
  an internal function.

### Bug Fixes

- [`confint.gam()`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  was failing on a tensor product smooth due to matching issues.
  Reported by [@tamas-ferenci](https://github.com/tamas-ferenci)
  [\#88](https://github.com/gavinsimpson/gratia/issues/88)

  This also fixes
  [\#80](https://github.com/gavinsimpson/gratia/issues/80)

  1.  which was a related issue with selecting a specific smooth.

- The **vdiffr** package is now used conditionally in package tests.
  Reported by Brian Ripley
  [\#93](https://github.com/gavinsimpson/gratia/issues/93)

## gratia 0.4.1

CRAN release: 2020-05-30

### User visible changes

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  with `scales = "fixed"` now applies to all terms that can be plotted,
  including 2d smooths.

  Reported by [@StefanoMezzini](https://github.com/StefanoMezzini)
  [\#73](https://github.com/gavinsimpson/gratia/issues/73)

### Bug fixes

- [`dplyr::combine()`](https://dplyr.tidyverse.org/reference/defunct.html)
  was deprecated. Switch to
  [`vctrs::vec_c()`](https://vctrs.r-lib.org/reference/vec_c.html).

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  with `scales = "fixed"` wasn’t using fixed scales where 2d smooths
  were in the model.

  Reported by [@StefanoMezzini](https://github.com/StefanoMezzini)
  [\#73](https://github.com/gavinsimpson/gratia/issues/73)

## gratia 0.4.0

### New features

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can include partial residuals when drawing univariate smooths. Use
  `residuals = TRUE` to add partial residuals to each univariate smooth
  that is drawn. This feature is not available for smooths of more than
  one variable, by smooths, or factor-smooth interactions (`bs = "fs"`).

- The coverage of credible and confidence intervals drawn by
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  can be specified via argument `ci_level`. The default is arbitrarily
  `0.95` for no other reason than (rough) compatibility with
  [`plot.gam()`](https://rdrr.io/pkg/mgcv/man/plot.gam.html).

  This change has had the effect of making the intervals slightly
  narrower than in previous versions of *gratia*; intervals were drawn
  at ± 2 × the standard error. The default intervals are now drawn at ±
  ~1.96 × the standard error.

- New function
  [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  for computing differences between factor smooth interactions. Methods
  available for [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
  [`gamm()`](https://rdrr.io/pkg/mgcv/man/gamm.html) and
  [`gamm4::gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html). Also has
  a [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method, which can handle differences of 1D and 2D smooths currently
  (handling 3D and 4D smooths is planned).

- New functions
  [`add_fitted()`](https://gavinsimpson.github.io/gratia/reference/add_fitted.md)
  and
  [`add_residuals()`](https://gavinsimpson.github.io/gratia/reference/add_residuals.md)
  to add fitted values (expectations) and model residuals to an existing
  data frame. Currently methods available for objects fitted by
  [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`bam()`](https://rdrr.io/pkg/mgcv/man/bam.html).

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  is a tidy reimplementation of
  [`mgcv::gamSim()`](https://rdrr.io/pkg/mgcv/man/gamSim.html) with the
  added ability to use sampling distributions other than the Gaussian
  for all models implemented. Currently Gaussian, Poisson, and Bernoulli
  sampling distributions are available.

- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  can handle continuous by variable smooths such as in varying
  coefficient models.

- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  now work for all families available in *mgcv*, including the location,
  scale, shape families, and the more specialised families described in
  [`?mgcv::family.mgcv`](https://rdrr.io/pkg/mgcv/man/family.mgcv.html).

- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md),
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md),
  [`family()`](https://rdrr.io/r/stats/family.html),
  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md),
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  methods for models fitted using
  [`gamm4()`](https://rdrr.io/pkg/gamm4/man/gamm4.html) from the
  **gamm4** package.

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  can generate data for a 1-d slice (a single variable varying).

- The colour of the points, reference lines, and simulation band in
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  can now be specified via arguments

  - `point_col`,
  - `point_alpha`,
  - `ci_col`
  - `ci_alpha`
  - `line_col`

  These are passed on to
  [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md),
  [`observed_fitted_plot()`](https://gavinsimpson.github.io/gratia/reference/observed_fitted_plot.md),
  [`residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_linpred_plot.md),
  and
  [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md),
  which also now take the new arguments were applicable.

- Added utility functions
  [`is_factor_term()`](https://gavinsimpson.github.io/gratia/reference/is_factor_term.md)
  and
  [`term_variables()`](https://gavinsimpson.github.io/gratia/reference/term_variables.md)
  for working with models.
  [`is_factor_term()`](https://gavinsimpson.github.io/gratia/reference/is_factor_term.md)
  identifies is the named term is a factor using information from the
  [`terms()`](https://rdrr.io/r/stats/terms.html) object of the fitted
  model.
  [`term_variables()`](https://gavinsimpson.github.io/gratia/reference/term_variables.md)
  returns a character vector of variable names that are involved in a
  model term. These are strictly for working with parametric terms in
  models.

- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  now works for models fitted by
  [`glm()`](https://rdrr.io/r/stats/glm.html) and
  [`lm()`](https://rdrr.io/r/stats/lm.html), as do the underlying
  functions it calls, especially `qq_plot`.

  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  also works for models fitted with family
  [`gaulss()`](https://rdrr.io/pkg/mgcv/man/gaulss.html). Further
  location scale models and models fitted with extended family functions
  will be supported in upcoming releases.

### User visible changes

- [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  is now an *internal* function and is no longer exported. Use
  [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  instead.

- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  is now much stricter and can only evaluate main effect terms,
  i.e. those whose order, as stored in the `terms` object of the model
  is `1`.

### Bug fixes

- The
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for
  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  was not getting the x-axis label for factor by smooths correctly, and
  instead was using `NA` for the second and subsequent levels of the
  factor.

- The
  [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  method for class `"gam"` couldn’t possibly have worked for anything
  but the simplest models and would fail even with simple factor by
  smooths. These issues have been fixed, but the behaviour of
  [`datagen()`](https://gavinsimpson.github.io/gratia/reference/datagen.md)
  has changed, and the function is now not intended for use by users.

- Fixed an issue where in models terms of the form `factor1:factor2`
  were incorrectly identified as being numeric parametric terms.
  [\#68](https://github.com/gavinsimpson/gratia/issues/68)

## gratia 0.3.1

CRAN release: 2020-03-29

### New features

- New functions
  [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  and
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  to access the link function and its inverse from fitted models and
  family functions.

  Methods for classes: `"glm"`, `"gam"`, `"bam"`, `"gamm"` currently.
  [\#58](https://github.com/gavinsimpson/gratia/issues/58)

- Adds explicit [`family()`](https://rdrr.io/r/stats/family.html)
  methods for objects of classes `"gam"`, `"bam"`, and `"gamm"`.

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now handles non-numeric when creating shifted data for finite
  differences. Fixes a problem with `stringsAsFactors = FALSE` default
  in R-devel. [\#64](https://github.com/gavinsimpson/gratia/issues/64)

### Bug fixes

- Updated *gratia* to work with *tibble* versions \>= 3.0

## gratia 0.3.0

CRAN release: 2020-01-19

### New features

- *gratia* now uses the *mvnfast* package for random draws from a
  multivariate normal distribution
  ([`mvnfast::rmvn()`](https://rdrr.io/pkg/mvnfast/man/rmvn.html)).
  Contributed by Henrik Singmann

  2.  [\#28](https://github.com/gavinsimpson/gratia/issues/28)

- New function
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  for generating tidy representations of basis expansions from an
  *mgcv*-like definition of a smooth,
  e.g. [`s()`](https://rdrr.io/pkg/mgcv/man/s.html),
  [`te()`](https://rdrr.io/pkg/mgcv/man/te.html),
  [`ti()`](https://rdrr.io/pkg/mgcv/man/te.html), or
  [`t2()`](https://rdrr.io/pkg/mgcv/man/t2.html). The basic smooth types
  also have a simple
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for plotting the basis.
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  is a simple wrapper around
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html)
  with some post processing of the basis model matrix into a tidy
  format. [\#42](https://github.com/gavinsimpson/gratia/issues/42)

- New function
  [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  to draw samples of entire smooth functions from their posterior
  distribution. Also has a
  [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
  method for plotting the posterior samples.

### Bug fixes

- [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would produce empty plots between the panels for the parametric terms
  if there were 2 or more parametric terms in a model. Reported by
  [@sklayn](https://github.com/sklayn)
  [\#39](https://github.com/gavinsimpson/gratia/issues/39).

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  now works with factor by smooths, including ordered factor by smooths.
  The function also now works correctly for complex models with multiple
  covariates/smooths.
  [\#47](https://github.com/gavinsimpson/gratia/issues/47)

  [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  also now handles `'fs'` smooths. Reported by
  [@tomand-uio](https://github.com/tomand-uio)
  [\#57](https://github.com/gavinsimpson/gratia/issues/57).

- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  and hence
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  would fail on a [`ziplss()`](https://rdrr.io/pkg/mgcv/man/ziplss.html)
  model because i) *gratia* didn’t handle parametric terms in models
  with multiple linear predictors correctly, and ii) *gratia* didn’t
  convert to the naming convention of *mgcv* for terms in higher linear
  predictors. Reported by [@pboesu](https://github.com/pboesu)
  [\#45](https://github.com/gavinsimpson/gratia/issues/45)
