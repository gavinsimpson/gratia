# gratia (development version)

## User visible changes

* Help examples use fewer model fits and repeated plots to focus on each
  function's main use and reduce execution time.

* gratia no longer requires ggokabeito; default plot colours and their ordering
  are unchanged. Unused curl and svglite suggestions have been removed, and
  ggdist, distributional and hexbin are now listed as website dependencies
  because they are used only in a website article.

* `grouped_by` is deprecated in `draw()` and `assemble()`. Include
  `"factor-by"` in the new `grouped` argument to overlay factor-by curves,
  or omit it to draw them separately. Explicit uses of `grouped_by` still
  update factor-by grouping, including ordered factors, while retaining
  the requested grouping of `sz` and `fs` curves.

* `draw.basis()` now draws contour lines on surface plots by default
  (`contour = TRUE`), matching `draw.gam()`. Use `contour = FALSE` to restore
  the previous appearance.

* **Plot appearance and theme migration**

  Plot aesthetic arguments for colours, opacity and line widths are
  deprecated in favour of ggplot2 component themes (#215). Existing
  default appearances are unchanged, and explicitly supplied arguments
  remain fixed overrides during migration.

  Apply `theme()` with `&` to every panel in a patchwork, or with `+` to a
  single plot. For example, replace `smooth_col` with
  `geom.gratia.smooth = element_geom(colour = ...)` and `ci_alpha` with
  `gratia.alpha.interval` inside `theme()`. See `gratia_theme_get()` and
  the Customizing plots article for migration examples.

  gratia now requires ggplot2 >= 4.0.3 and patchwork >= 1.3.2.

* Native `mgcv::scasm()` smooth estimation, drawing and `confint()` now default to
  `overall_uncertainty = FALSE`. An explicit request for unavailable overall
  mean uncertainty warns once (#372, #373).

* **Sampling arguments (breaking changes)**

  **Breaking change:** Sampling functions now use `n_draws` for the total
  retained draw count and `n_grid` for automatically constructed
  evaluation grids. Sampling controls must be named. Obsolete names,
  including `n`, `n_sim`, `n_vals`, `freq`, `newdata`, and `ncores`,
  produce migration errors.

  `derivative_samples()` takes `model` rather than `object`; `add_*()`
  sampling helpers take `data` rather than `object`. See
  `vignette("sampling-migration")`.

  Coefficient and `*_samples()` functions require an explicit `n_draws`
  when generating draws; supplied coefficient draws determine their own
  count and chains. Likelihood, LOO, WAIC and bayesplot wrappers use
  `n_draws` with their documented defaults.

  Derivative sampling now defaults to a grid resolution of 200, matching
  smooth sampling; supply `n_grid = 100` for the previous derivative
  resolution.

  Gaussian threads use `n_threads`, while `n_cores` limits concurrent MH
  chains. Explicit controls that do not apply to the selected sampler are
  rejected.

* **Fitted quantities (breaking changes)**

  **Breaking change:** `fitted_samples()` now defaults to all distribution
  parameters represented by predictors, matching `fitted_values()`.
  Request `quantity = "expectation"` for the expected response, on the
  response scale only. Both functions use explicit parameter names,
  including for scalar models.

  Gaussian location-scale models return standard deviation rather than
  reciprocal standard deviation; GEV location is labelled separately from
  expectation.
  Multivariate responses and categories have separate `.y` and `.category`
  identifiers.

  Global family parameters remain fixed during coefficient sampling.
  Unknown general families require a natural-parameter or expectation
  specification; their first predictor remains available on the link scale
  (#426).

* Gaussian coefficient sampling for `mgcv::scasm()` fits with linear inequality
  constraints now warns that the draws need not satisfy the fitted shapes.
  Metropolis-Hastings sampling now gives an explicit error because its
  proposals and likelihood calculation do not enforce those constraints.

* gratia has a new neon hex logo. The pkgdown site uses the pink
  g from the logo as its favicon.

* **Covariate scales in plots**

  **Breaking change:** Parametric plots now use covariates as evaluated in
  the model: `log(x)` is plotted against `log(x)`, with a matching axis
  label. Use `back_transform = TRUE` in `draw.gam()`, `assemble.gam()`,
  `draw.parametric_effects()` or `draw.smooth_estimates()` to use original
  covariates for interaction axes, tensor products, rugs and conditioning
  facets (#412). Multi-column basis expansions retain their original
  covariate axes, and changing the coordinate scale leaves effects and
  their intervals unchanged.

  The `transform` argument of `parametric_effects()` is deprecated.
  Replace it with `back_transform = !transform` (#412).

* **Breaking change:** `draw.gam()` and `assemble.gam()` now default to
  `parametric = NULL`, including parametric terms alongside smooths when
  `select = NULL`; the extra panels can change plot layouts and the number of
  plots returned by `assemble()` or `draw(wrap = FALSE)`. Selecting smooths
  explicitly continues to draw only those smooths, unless `parametric = TRUE`
  is supplied. Use `parametric = FALSE` to restore the previous default.

* Parametric effect intervals now include intercept uncertainty and its
  covariance with the term, while partial estimates are unchanged. Use
  `overall_uncertainty = FALSE` in `parametric_effects()`, `draw()` or
  `assemble()` to recover term-only intervals. Each term uses the intercept
  of its own linear predictor; models without an intercept are unaffected.

* Derivative functions now choose the finite-difference step automatically
  when `eps = NULL`, using the fitted covariate range, derivative order and
  difference method. This reduces rounding error in second derivatives. Supply
  a positive `eps` to specify an absolute step size.

* `appraise()` now automatically uses scalable plots when at least 10,000
  observations were used for fitting. Set `scalable = TRUE` or `FALSE` to
  override this behaviour, or change `scalable_threshold`.

* Explicit `xlab = NULL` and `ylab = NULL` now request default axis labels for
  continuous-covariate `fs` plots and random-effect tensor curves, consistent
  with other univariate smooths. Previously, explicit `NULL` suppressed these
  labels (#308).

* **Tweedie distribution calculations**

  Tweedie cumulative distribution functions now use fast Poisson-Gamma
  mixtures for `1 < p < 2` instead of requiring the tweedie package (#410).
  The Gamma case (`p = 2`) uses native R distribution functions.

  Mixture calculations warn and return available approximations when their
  work or accuracy limits are reached. The tweedie package is now
  optional, in Suggests, and is needed only for numerical inversion
  requested with `fallback = TRUE`.

* `posterior_samples()` now uses separate random-number streams for
  coefficient and response draws. Response draws for a given seed differ from
  earlier releases, but remain reproducible.

* gratia now requires mgcv >= 1.9-4 and R >= 4.4.0.

* glmmTMB support now requires the released version 1.1.15.2 or later, so
  the development version of glmmTMB is no longer needed.

## New features

* `compare_smooths()` evaluates two-variable smooths at common locations,
  and `draw()` compares their surfaces in side-by-side facets with shared axes
  and colours. Use `show = "difference"` and `pair` to plot one model's
  smooth contribution minus another's, with a colour scale centred on zero (#435).
  Conditioning values retain separate panels, constant surfaces omit contours,
  and labels for distribution parameters are preserved.

* `draw.compare_smooths()` accepts `ci_level` to set the coverage of its
  pointwise intervals, matching `draw.gam()`. Unsupported `coverage` and `level`
  arguments now give an error directing callers to `ci_level`, including when
  supplied alongside it (#436).

* `difference_smooths()` computes first and second derivatives of differences
  between factor-by smooths using `order`, including partial derivatives across
  surfaces using `focal`. Intervals retain covariance between the smooths, and
  `draw()` labels the derivative values; raw-covariate comparisons also allow
  transformations that depend on the comparison factor (#264, #356).

* `draw_label_control()` provides optional label layout settings across
  `draw()` and `assemble()` methods. Wrap discrete axis labels, legend
  entries and guide titles, stagger discrete tick labels, arrange legend
  keys, and adjust colour-bar dimensions and label angles. Existing
  defaults, scale labels and palettes are retained (#430).

* **Conditional prediction surfaces**

  `conditional_values()` accepts two numeric covariates grouped in the
  first condition, such as `condition = list(c("x1", "x2"), "treatment")`, to
  evaluate and draw prediction surfaces. Remaining conditions supply
  facets.

  Its `draw()` method supports raster, tile and optional ggcube 3D
  surfaces, including interval surfaces via `ci_3d = TRUE`. The default
  `n_vals = NULL` uses 100 values for curves and 50 along each surface axis.

* **Random-effect sampling and marginal predictions**

  `random_effect_samples()` draws new random intercepts, slopes and
  factor-smooth curves from their estimated Gaussian distributions.

  `marginalise` integrates response means in fitted predictions using
  Gaussian quadrature or Monte Carlo, and generates new-group effects in
  response simulations. Smoothing parameters and dispersion remain fixed
  at their fitted estimates.

  Factor-by curves retain their by-level selection when other random terms
  are marginalised. Identity, log and probit links use closed-form
  integrals; other links check quadrature with interlaced nodes and an
  independent adaptive rule to detect unresolved link transitions. Fits
  using `min.sp` are rejected because their fitted lower bounds are not
  retained for covariance reconstruction.

* `variance_comp()` has expanded support for variance components in `gam()`,
  `bam()`, `gamm()` and `gamm4()` models, and the new `type = "terms"` returns
  fitted term variances and proportions of total variance, accounting for
  covariance between terms and providing posterior intervals. A new `draw()`
  method plots variance components, standard deviations or proportions.

* `basis()` accepts `unweighted = TRUE` for fitted GAM(M)s to return basis
  functions without multiplying by fitted coefficients. `draw()` shows these
  shared bases, and bases from factor-by smooth specifications, once per
  factor-by set. Coefficient-weighted fitted bases remain separate by level
  (#428).

* **Fitted quantities and family parameters**

  `fitted_values()` and `fitted_samples()` accept `quantity` to select
  distribution parameters or expected responses. Parameter draws retain
  their joint coefficient uncertainty; coupled expectation intervals use
  the full covariance.

  `family_parameters()` describes natural parameters, native
  representations, predictor order and fixed global parameters. These
  shared descriptions also provide parameters for posterior response
  simulation, including when an expectation does not exist.

  Grouped ordinal expectations return category probabilities alongside
  scalar component means (#426).

* Grouped-family diagnostics now accept `response = NULL` for all family
  components or a single integer family-list index for one component.
  This uses the same selection convention as joint GJRM diagnostics.
  `by_family` independently controls the existing pooling and faceting
  behaviour, and selected results retain their original component labels.

* `assemble()`, `draw()` and `smooth_estimates()` now support joint models
  fitted by `GJRM::gjrm()`, using the final joint coefficients and covariance
  matrices for smooths, random effects and parametric effects. Diagnostic
  functions support ordinary uncensored joint models whose predictors do not
  include modeled responses; `appraise()` facets each panel by response.

* **Grouping and drawing factor-related curves**

  `draw()` and `assemble()` can separate `sz` and `fs` curves into
  individual plots (#244). `grouped` names the types to overlay:
  `"factor-by"`, `"sz"`, and `"fs"`. `show_ci` independently selects which
  types show intervals, including optional intervals for `fs` curves.

  The defaults retain existing grouping and interval visibility.
  Ordered-factor deviations use the factor-by settings and keep their
  reference smooth separate. Saved smooth estimates support the same
  controls.

  Common axis ranges use displayed estimates and visible intervals.
  Grouped factor-by plots retain observations from every plotted level and
  accept custom colour and fill scales.

  Smooth and custom titles retain colons within their text.

  Separate curves use the usual black line and translucent black interval.
  Set `grouped = FALSE` to separate all factor-related curves, or
  `show_ci = FALSE` to hide their intervals.

* `assemble()` now supports models fitted by `mgcv::gamm()`, returning named
  plots of smooth and parametric contributions. It shares preparation of the
  fitted GAM component with `draw()`.

* `assemble()` now supports models fitted by `GJRM::gamlss()`, returning a
  named list of smooth and parametric plots across equations. It shares model
  preparation, selection and scale handling with `draw()`.

* `assemble()` now supports glmmTMB models, returning named plots for
  smooths, native random effects and parametric terms. It shares model
  preparation and argument checks with `draw()`.

* **Polar plots of cyclic smooths**

  `draw()` and `assemble()` can draw one-dimensional cyclic smooths with
  `cyclic = "polar"`, including factor-by and one-dimensional tensor smooths
  across supported model classes. Fitted cycle boundaries configure the
  angular axis and automatic evaluation grids span a complete cycle. Saved
  smooth estimates retain these boundaries; supplied evaluation points are
  unchanged.

  `polar_args` customizes the polar coordinate display. Radial padding
  separates interval bands from the outer observation rug. Radial axes
  appear inside the circle by default; override their position through
  `polar_args`.

* `draw.basis()` now plots basis functions of three- and four-covariate
  smooths as separate surfaces with conditioning facets (#258, #259).
  `basis()` and `draw.basis()` accept `variables` to choose the axes and
  conditioning order. Automatic grids use the existing `n_2d`, `n_3d`, and
  `n_4d` controls, and saved bases retain their chosen covariate order.

* **Response moments**

  `fitted_moments()` and `conditional_moments()` estimate response means,
  variances, SDs, coefficients of variation, skewness and kurtosis.
  Standard, distributional, categorical, multivariate-normal, grouped and
  latent censored response families are supported where their moments
  exist. Conditional moments can be drawn with `draw()`.

  Delta-method intervals and joint Gaussian simulation propagate
  covariance between distribution parameters. Supported ML/REML fits also
  propagate estimated global family parameters and dispersion;
  `nuisance = "fixed"` explicitly conditions on their fitted values.
  Dispersion and coefficient covariance use the same ML/REML scale
  estimate as the saved fitting curvature, including models with linked or
  fixed smoothing parameters.

* `smooth_estimates()`, `derivatives()`, and `difference_smooths()` offer
  continuous-interval Gaussian simultaneous bands for supported one-dimensional
  cubic splines via `simultaneous_method = "tube"`. Bands use a conservative
  threshold-crossing bound, preserve the selected covariance and group means,
  and record the domain and numerical diagnostics in interval metadata.
  `draw()`, `assemble()`, and `confint()` forward the controls.

* **Axes and conditioning facets**

  Surface plotting and related evaluation functions now accept `n_2d`,
  with a default of 50 points along each surface axis (#153). Default
  surface grids therefore shrink from 10,000 to 2,500 points per panel.
  Curve and conditioning slice resolutions can be controlled separately.

  Smooth estimation and drawing, including `difference_smooths()`, now
  accept `variables` to choose x and y axes and the order of conditioning
  facets. Differences between 3D and 4D smooths can now be drawn as
  faceted surfaces (#256, #257).

  Their automatic grids use `n_3d = 16` and `n_4d = 4`, matching individual
  smooths; use `NULL` to use `n` for those dimensions. Resolved covariate
  orders are retained when drawing saved estimates. Supply `data` to
  choose explicit conditioning values.

* **Surrogate residuals**

  Surrogate residuals are available for `mgcv::ocat()` and
  `mgcv::multinom()` GAMs through `surrogate_residuals()`,
  `type = "surrogate"` in ordinary and scalable residual diagnostic plots,
  and `add_residuals()`. Ordinal residuals follow Liu and Zhang (2018),
  with logistic references (#359). Nominal residuals follow Cheng, Wang
  and Zhang (2021), retaining a centred Gumbel utility component for every
  category, including category 0.

  All appraisal panels share one draw, and categories have separate
  panels. Both methods require no additional package and preserve accuracy
  in extreme tails.

* `derivatives()`, `partial_derivatives()`, `smooth_samples()` and
  `difference_smooths()` use workers configured by `mirai::daemons()` for
  independent smooth or comparison calculations, or run serially without
  workers (#348). Joint posterior draws, seeded simulation results and
  result order are preserved across worker counts. Custom smooth prediction
  methods run in the caller so workers use the same bases.

* **Plot themes and palettes**

  Plots now support native ggplot2 theme settings for smooths, intervals,
  residuals, diagnostics and other components, including completed
  patchwork figures. `gratia_theme_get()`, `gratia_theme_set()` and
  `gratia_theme_update()` manage the optional `gratia.plot.theme`
  override.

  `gratia_palette()` provides optional Paul Tol palettes. Current default
  appearances are retained, including group legends without symbols for
  fixed-colour partial residuals (#215).

* **SCAM model support**

  All 43 smooth constructors in scam 1.2-23 are supported for smooth
  estimates, bases, sampling and plots, including bivariate smooths,
  cyclic margins, interactions and explicit `By` classes. This includes
  `tedi` and `teid` surfaces that increase along one covariate and
  decrease along the other.

  `draw()` and `assemble()` accept `centre = "plot"` to match scam's
  centring of bivariate displays; the default retains the fitted function.

  SCAM `smooth_type()` labels use compact descriptions such as
  `P spline (inc)` and `Tensor (inc, free)`. Saved estimates with earlier type
  labels remain drawable.

  Plot captions describe the basis and constraints separately, using
  arrows for monotonicity, covariate names for tensor constraints and
  fitted change points for local constraints. Saved smooth estimates
  retain these captions.

  SCAM models gain conditional simultaneous smooth bands, derivatives,
  contrasts, conditional predictions, parametric effects, residual
  helpers, model summaries, concurvity, penalties and variance-component
  ratios. Independent-response likelihoods, LOO/WAIC, supplied coefficient
  draws and scalable posterior predictive plots are also supported. See
  the SCAM support article for supported combinations and uncertainty
  calculations.

* **Shape-constrained intervals**

  `shape_intervals()` applies monotone rearrangement and convex or concave
  envelopes to existing interval endpoints on a finite grid. Supported
  scasm smooths expose this through `interval_method = "shape"` and an
  explicit Gaussian or stored-bootstrap initial source. The transformation
  preserves whole-grid containment events; it does not establish nominal
  coverage or preserve marginal coverage of pointwise limits.

  `shape_confidence_set()` and `in_shape_set()` represent and inspect
  intersections with coefficient inequalities and equalities. See the
  shape-constrained intervals article for supported combinations, tensor
  requirements and examples (#372, #373).

  `draw()` and `assemble()` also accept `frequentist` for the initial
  smooth covariance calculation.

  General `pc` replacements and derivative bases are rejected when they do
  not justify the named function shape. Curvature requires equally spaced
  fitted knots to rounding precision, and extrapolation is rejected.

  Convex envelope interpolation preserves exactly flat segments. Saved
  estimates reject interval computation controls that require the fitted
  model.

  `interval_method` selects native scasm bootstrap or Gaussian pointwise
  intervals for smooths, fitted values, smooth derivatives and
  `confint()`.

  Shifting or transforming smooth confidence intervals retains asymmetric
  limits. Replacing saved intervals with `add_confint()` records their
  Gaussian source.

  Automatic selection uses stored bootstrap draws, including mgcv's
  smoothing-bias correction. Simultaneous bootstrap bands remain
  unsupported (#372, #373).

* Scasm captions show retained shape constraints with portable mathematical
  arrows. Saved estimates retain constraint metadata for each tensor margin;
  ignored requests, including named constraints replaced by general `pc`
  inequalities, are labelled and produce a warning (#373). Shape-enforced
  intervals keep the basis and constraint caption, with interval details in
  the documentation and metadata.

* Gaussian simultaneous bands now support positive-semidefinite coefficient
  covariance matrices, including scasm fits with exact equality constraints.

* `penalty()` extracts stored tensor and `t2()` penalty matrices, including
  marginal penalties via `margins = TRUE` and their original scaling.

* **Posterior sampling and diagnostics**

  Posterior sampling supports multiple chains, including parallel MH
  chains using mirai. `n_draws = 1000, chains = 4` retains 250 draws per chain
  after separate burn-in. Chain identities are preserved through fitted,
  response, smooth, and derivative samples.

  `as_mcmc_draws()` converts draws for posterior, coda, and bayesplot;
  `diagnostics = TRUE` and `sampling_diagnostics()` provide coefficient
  convergence summaries and acceptance rates (#281).

  `draw(sampling_diagnostics(x))` plots native coefficient traces or
  pooled-rank histogram outlines by chain. Sampling with
  `diagnostics = TRUE` retains the coefficient draws for these plots through
  fitted, response, smooth and derivative sampling.

  Sampling for models with several distribution parameters supports fitting
  rows, subsets and new covariates without observed responses. Parameters
  represented by predictors use the same joint coefficient draw.

  `fitted_samples()` defaults to distribution parameters; use
  `quantity = "expectation"` for expected responses. On the response scale,
  `derivative_samples()` differentiates the expected response, including
  means that depend on scale, shape or hurdle probabilities.

  Fixed-coefficient response sampling uses the same parameter
  transformations. Missing predictors or explicit sampling weights produce
  `NA` response draws consistently.

  For models with several distribution parameters, response generation now
  reuses model-matrix blocks and constructs the output table once, preserving
  seeded responses and chain identities.

* **Metropolis–Hastings backends**

  `mh_backend = "gratia"` adds a compiled MH evaluator for common response
  families in ordinary gam and bam fits with independent observations.
  Large designs can be evaluated in blocks.

  `mh_backend = "family"` uses the compiled MH loop with the fitted family's
  likelihood functions, including compatible extended, grouped, and
  multiple-predictor families. It prepares and reuses a full design
  matrix, smoothing penalty, and proposal factorisation across chains.

  Smoothing parameters and parameters outside the coefficient vector
  remain fixed; LSS scale and shape coefficients vary jointly with
  location. The default backend remains `mgcv::gam.mh()`. The new backends
  reject BAM fits with nonzero prescribed smooth contributions, where
  native fitting and prediction use different offsets.

* **Simulated data**

  `data_sim()` accepts descriptive aliases for the original example
  models, including `"additive"`, `"bivariate_surface"`, `"gu_wahba_f2"`
  and `"luo_wahba_f6"`. All existing names remain supported and produce
  identical seeded output.

  `data_sim()` now accepts family objects, explicit predictors and
  generating parameters for scalar, location-scale, categorical,
  multivariate and Cox models. Censored families can return observed
  intervals while retaining complete responses and true distribution
  parameters. Grouped families can use repeated distributions with
  separate parameters.

  `data_sim()` gains analytic shape reference functions and fitting
  recipes for `mgcv::scasm()`, including combined shape constraints,
  tensor surfaces and Gamma models with constraints on the log mean and
  log dispersion. The additive Poisson recipe uses 240 observations and
  `k = 6` for each smooth to avoid fitting errors with mgcv 1.9-4.

* **Accumulated local effects**

  `ale()` computes accumulated local effects for numeric covariates in
  `mgcv::gam()` and `mgcv::bam()` fits, with link and response scales,
  distribution parameters, and pointwise or simultaneous intervals.
  `draw()` plots saved results with parameter labels and a scale caption.

  Prediction blocks can use workers configured by `mirai::daemons()`.
  Block accumulation order, shared interval draws and caller RNG handling
  are preserved.

  Custom prediction methods and transformations that require caller
  functions or S3 methods retain serial prediction. Method checks do not
  invoke active or lazy caller bindings.

* **Residuals against covariates**

  `residuals_covariate_plot()` and `scalable_residuals_covariate_plot()`
  plot complete-model residuals against numeric or categorical covariates
  and pairs of numeric covariates. Exact mgcv smooth labels can select
  their constituent covariates.

  Grouped displays, mean trends and joint maps help identify remaining
  mean structure or changes in residual spread. By default, mean trends
  are fitted only for groups with at most 20,000 observations;
  `smooth = TRUE` or `FALSE` overrides this limit.

* Optional `bayesplot::pp_check()` methods provide posterior predictive checks
  for supported GAMs and mixed models (#278, reported by @nicholasjclark).
  Sampling controls, including chains, are supplied through `sampling_args`.
  Response draws remain aligned with likelihood draws for leave-one-out plots,
  including supplied matrices with stored chain and draw identities.

* `log_lik()` and optional `loo::loo()` and `loo::waic()` methods support
  approximate predictive comparisons for independent GAM/BAM fits
  (#278, reported by @nicholasjclark).
  They retain coefficient diagnostics and chain identities. LOO uses stored
  iteration order for relative effective sample sizes without changing the
  likelihood draw order; unlabelled supplied draws need explicit efficiency
  or chain information.

* **Diagnostics supplied by response families**

  Diagnostic plots now support native inlaws families through their
  supplied `gratia_diagnostics` functions, including grouped-family fits
  and models with multiple response columns.

  mgcv-compatible families can now supply a `gratia_diagnostics` function
  for response means, variances, probability bounds and distribution
  functions. This allows diagnostics for additional count, ordinal,
  mixture and censored families without adding family-name checks to
  gratia.

  Traditional simulated reference residuals can apply a supplied
  observation process. See `?family_diagnostics` for the interface.

* `dharma_residuals()` and `type = "dharma"` in diagnostics calculate
  simulation-based PIT residuals using gratia's response generators, without
  requiring DHARMa or mgcViz. `n_dharma` controls the simulation count, and
  residuals retain zero and one endpoints and are shared across `appraise()`
  panels. QQ and worm plots use uniform references with approximate
  pointwise bands for independent responses; dependent responses omit these
  bands.

* New site articles explain model diagnostics, grouped surveys, glmmTMB and
  sdmTMB models, prediction comparisons, response distributions, and smooth
  effects on different spatial domains. They include fitted examples and
  explain the assumptions behind estimates, intervals and checks.

* **Residual diagnostics and posterior predictive checks**

  Residual diagnostics and posterior predictive checks now cover `lm`,
  `glm`, `gam`/`bam`, `gamm`/`gamm4`, glmmTMB, univariate GJRM and sdmTMB
  models using their fitted response distributions. See
  `?diagnostic_support` for supported families, conditioning choices and
  limitations.

  Probability integral transform (PIT) residuals map observations to
  fitted probabilities, randomising over probability mass for discrete
  responses. Quantile residuals convert those probabilities to
  standard-normal scores.

  Prepared diagnostic objects carry the selected conditioning and sdmTMB
  field choices through residual and plotting methods.

* **Mixed-model diagnostics**

  Mixed-model diagnostics can hold fitted random effects fixed or average
  over their fitted distribution. Joint response simulations use the
  fitting packages' generators to retain temporal and spatial covariance
  and shared random effects. Marginal residuals can use means, variances
  and ranks from these simulations.

  Direct `nlme::lme()` and `lme4::merMod` fits also support residual and
  posterior predictive checks. Non-Gaussian `gamm` fits estimated by
  penalized quasi-likelihood (PQL) support approximate checks when they
  have no residual working correlation. nlme calculations retain
  covariance between random effects in block designs, fitted factor
  contrasts and observation order after subsetting or missing-value
  removal.

* **sdmTMB diagnostics**

  sdmTMB residual diagnostics now offer four ways to treat random fields:
  draw fitted fields once from their approximate posterior and hold them
  fixed (`field_method = "mle-mvn"`, the default), hold field estimates
  fixed (`field_method = "mle-eb"`), average over fitted-field uncertainty
  (`field_method = "mle-mvn-integrated"`), or simulate new fields
  (`conditioning = "marginal"`). Plot labels identify the choice.

  Posterior predictive checks separately include approximate parameter
  uncertainty. See `?diagnostic_support` for what these checks assess and
  their limitations.

  Delta-model diagnostics use the response family's dispersion parameter.
  Fits with a dispersion formula give an explicit error because
  observation-specific dispersion is not yet supported by these
  diagnostics.

  Student-t sdmTMB models now support residual diagnostics and posterior
  predictive plots with fixed or estimated degrees of freedom. Pearson
  residuals require degrees of freedom above two. Quantile residuals and
  predictive checks remain available with infinite variance.

  Predictive plots also work for additional native sdmTMB families without
  requiring an analytic residual distribution.

* **GJRM diagnostics**

  Diagnostic plots now support uncensored univariate `GJRM::gamlss()`
  models with N, LN, GA, GAi, P, NBI, NBII, BE, LO, WEI, IG, GU and rGU
  families. Residual plots, response simulation and ordinary and scalable
  posterior predictive checks use the final fitted distribution.

  Posterior draws retain covariance between distributional equations,
  including beta's logit-linked scale. They use randomised quantile
  residuals with a standard-normal reference.

  Observed-versus-fitted panels show response means calculated from the
  final distribution parameters, including lognormal, Weibull and Gumbel
  mean corrections. NBII probabilities are calculated from the final
  fitted likelihood parameters, rather than relying on stored
  probabilities that can use a Poisson distribution.

* **glmmTMB diagnostics**

  `appraise()`, `qq_plot()` and `worm_plot()` now support native glmmTMB
  residuals, with conditional response simulation for QQ reference bands.
  See `?'glmmTMB-methods'` for supported families and current limitations.

  Gaussian predictive draws use glmmTMB's standard-deviation
  parameterisation. Variances for truncated count distributions are
  calculated accurately even with tiny means.

* **Grouped-family residual plots**

  Grouped-family diagnostics now support quantile residuals with
  appropriate standard-normal references and consistent component colours.
  Traditional residuals use separate facets for each component (#267).

  Pooled histograms use pale fills and solid step outlines in each
  component's colour, so overlapping distributions remain visible.
  Ordinary and scalable plots use the same bin edges and calculate each
  component's density separately.

* **Distribution functions and checks**

  Distribution checks now cover more mgcv families, including `shash()`,
  `twlss()`, ordinal, `ziP()` and grouped families. The internal
  `fix_family_qf()` extends mgcv's `fix.family.qf()` with experimental
  support for `scat()`, `tw()`, `Tweedie()`, `gaulss()`, `gevlss()`,
  `gumbls()`, `gammals()`, `ziplss()` and `mvn()`. `fix_family_cdf()` adds
  cumulative distribution functions for `gaulss()`, `gevlss()`,
  `gumbls()`, `gammals()`, `ziplss()` and `mvn()`, making quantile
  residuals available.

  For `mvn()`, both helpers use each response's marginal Gaussian
  distribution with its fitted variance (#183). Categorical CDFs and
  quantiles for `multinom()` use mgcv's category codes `0, ..., K` as the
  accumulation order, enabling randomised PIT/quantile residuals and
  inverse-CDF diagnostic references (#183).

  These helpers and `fix_family_rd()` preserve distribution functions
  supplied by callers.

  `ziplss()` quantile residuals randomise over the probability mass of
  each observed count (GRA-007). Quantile helpers retain log probabilities
  where supported; conversions that cannot represent an extreme
  probability give an explanatory error rather than a false infinite
  quantile.

* **Tweedie quantiles and QQ references**

  New Tweedie quantile helpers support uniform QQ diagnostics (#410). They
  use fast Poisson-Gamma mixtures for `1 < p < 2` and native R distribution
  functions for the Gamma case (`p = 2`). Expensive numerical inversion is
  available with `fallback = TRUE` and the optional tweedie package.

  Mixture calculations warn and return available approximations when their
  work or accuracy limits are reached.

  With `method = NULL`, `appraise()`, `qq_plot()` and `worm_plot()`
  automatically select simulation for `tw()` and `Tweedie()` models,
  avoiding numerical quantiles. Other ordinary GAM families prefer
  `"uniform"`, with fallbacks when a method is unavailable. An explicit
  method overrides the automatic choice.

* **Censored-model residuals and distribution functions**

  `quantile_residuals()` now supports censored `cnorm()`, `cpois()` and
  `clog()` models, randomising over censoring intervals for PIT and
  quantile residuals.

  These families also gain cumulative distribution and quantile helpers,
  and `fix_family_rd()` provides random generation for their latent,
  uncensored responses. The `cnorm()` and `clog()` generators use the
  fitted family scale and prior weights. Tail probabilities are calculated
  on the logarithmic scale where supported to avoid numerical infinities.

* Inverse Gaussian distribution functions now support PIT/quantile residuals
  and inverse-CDF QQ/worm plots without additional dependencies. Their
  calculations account for prior weights.

* Explicit GAM sampling weights override response precision or binomial trial
  counts.

* `posterior_predictive_plot()` compares observed responses with posterior
  predictive distributions, using density curves for continuous responses and
  points with interval bars for discrete responses. Set
  `appraise(ppcheck = TRUE)` to replace the residual histogram with this
  check. Plots summarise all draws on a shared grid; discrete responses with
  more than 512 distinct values use labelled probabilities for 512 bins.

* **Scalable diagnostic plots**

  Five new `scalable_*_plot()` functions provide binned QQ/worm plots,
  residual-density heatmaps, histograms and observed-versus-fitted
  heatmaps. They use all observations while keeping plotting data compact.
  Plot objects store the grid rather than all observations.

  Residual heatmaps estimate the density at each predictor value with
  `KernSmooth::bkde2D()` by default. A square-root colour scale makes
  low-density regions easier to see. Use `density_method = "histogram"` for
  unsmoothed bins, or `bandwidth` to control smoothing on both axes.

  Heatmaps of counts remain unsmoothed.

  KernSmooth is an imported dependency.

* QQ reference calculations now average draws as they are generated, or use
  temporary files for exact simulated quantiles above the configured memory
  limit. This reduces memory use without discarding reference draws. Simulated
  GAM QQ and worm plots are also faster, with unchanged numerical results.

* `residual_correlation()` and `residual_correlation_plot()` provide
  approximate checks of serial structure, including for BAM models that apply
  AR1 to working residuals. They omit significance bands based on
  independence.

* **glmmTMB effects and plots**

  `draw()`, `smooth_estimates()` and smooth coefficient extraction now
  support glmmTMB models, including conditional, zero-inflation and
  dispersion smooths where glmmTMB supports them. All smooths are plotted
  by default, with mgcv-style predictor labels (`s(x)`, `s.1(x)`, etc.)
  and `select` applying across the model.

  Parametric effects and native random effects are also plotted, including
  for models without smooths. Native random intercepts and slopes use
  separate Gaussian QQ panels. Set `ranef_lines = TRUE` to replace a simple
  `(1 + x | g)` pair with lines showing its random contribution only; more
  complex terms retain QQ panels.

  Effect intervals use TMB joint covariance. See `?'glmmTMB-methods'` for
  current limitations.

* **sdmTMB effects and plots**

  `draw()` and `assemble()` support formula smooths and parametric effects
  from sdmTMB models, including both predictors of delta models, with
  companion methods for smooth labels, coefficients and effect estimates.
  Support requires sdmTMB >= 1.2.0 models fitted with the RTMB backend.
  Smooths use joint RTMB covariance, including tensor smooths with
  multiple penalties.

  `get_smooth()` extracts sdmTMB smooths by exact name and `n_smooths()`
  counts them, including both predictors of delta models, without calculating
  coefficient covariances (#441).

  Breakpoint (`breakpt()`) and logistic (`logistic()`) curves are
  available through `parametric_terms()` and `parametric_effects()`, and
  are selected with `terms` in `draw()` and `assemble()`. Their pointwise
  delta-method intervals include threshold parameter uncertainty and, by
  default, intercept uncertainty and its covariance with the threshold
  parameters.

  Time-varying coefficients from `rw`, `rw0` and `ar1` models combine the
  ordinary coefficient and its temporal deviation where appropriate, with
  pointwise intervals using their joint covariance. Select trajectories
  with labels such as `time_varying(temperature)` in `terms`; the
  corresponding ordinary-effect panels are omitted by default but remain
  available through explicit selection. Use
  `time_varying_style = "pointrange"` for points and interval bars instead
  of the default line, points and ribbon.

* **Model parts in plot subtitles**

  Smooth and parametric plot subtitles now identify the model part or
  distributional parameter for glmmTMB, sdmTMB, mgcv, `GJRM::gamlss()` and
  `GJRM::gjrm()` models with multiple linear predictors. Single-predictor
  models omit these labels, and constant glmmTMB dispersion alone does not
  trigger them.

  Information about `by` variables, smooth titles, selection and the
  default of plotting all smooths are retained. Shared terms name every
  predictor to which they contribute.

  Saved `smooth_estimates()` and `parametric_effects()` retain the labels
  in an optional `.model` column. Supply `subtitle` to replace the labels,
  or `subtitle = ""` to suppress them, including on parametric panels.
  Repeated parametric plot names include equation numbers for GJRM models
  (#430).

* Soap-film boundary (`bs = "sf"`) and wiggly (`bs = "sw"`) components are now
  supported, with distinct component types and boundary-aware plots (#375).

* **Grouped-family sampling**

  Grouped-family models fitted with `mgcv::gfam()` now support
  response-scale `fitted_samples()` with component-specific mean
  transforms (#269), and `simulate()` with component-specific
  distributions and parameters (#272). These functions and
  `posterior_samples()` accept supplied data and `family_index` for
  component membership without observed responses. Posterior responses
  preserve joint coefficient draws, component labels, offsets and sampling
  weights; ordinal response-scale fitted draws remain unsupported.

  Derivative sampling retains family membership when shifting supplied
  covariates.

* **gamm4 model support**

  Support for classed gamm4 models now covers smooths, parametric effects,
  predictions, derivatives, simulations, posterior draws and variance
  components. Smooth and prediction methods describe the population GAM
  component and exclude additional terms supplied through `random`. Models
  require gamm4 >= 0.2-7; historical unclassed lists are unsupported.

  See `?gamm4_support` for covariance and sampling limitations, and for
  the full-model conditioning options used by diagnostics and response
  draws. Diagnostics, response simulations and posterior predictive checks
  use the full fitted mixed model with `conditioning = "conditional"` by
  default. Response references hold fitted smooth and grouping effects
  fixed; posterior predictive checks include uncertainty in those fitted
  effects.

  Select `conditioning = "gam_component"` to use only the extracted GAM
  component. This choice is required when supplying coefficient draws or
  prediction data to `posterior_samples()`; full-model draws use fitting
  observations only.

* `parametric_effects()` and `draw()` now support parametric interactions,
  including higher-order interactions. Numeric interactions use surfaces and
  conditioning facets; numeric-by-factor interactions use grouped curves;
  factor interactions use grouped estimates and intervals. Interaction
  surfaces centre the default diverging palette at zero, using a common fill
  range across facets; fixed scales expand the fill range as for smooth
  surfaces (#411).

* **Functions of covariates**

  Smooth evaluation, plotting, bases, sampling and prediction now support
  functions of covariates such as `s(log(x))`, including transformed
  offsets and an `envir` argument for local functions and constants.
  Stored evaluated covariates allow smooth plotting when the original data
  or functions are unavailable. See
  `vignette("transformed-covariates", package = "gratia")` for examples.

  Automatic smooth grids retain transformed coordinates, and
  `derivatives()` and `partial_derivatives()` can differentiate with
  respect to raw covariates using `wrt = "covariate"`. Evaluation failures
  at new data produce actionable errors. Parametric effects and data
  slices recover raw covariates consistently.

  `simulate()` and `add_fitted()` also accept `envir` for local functions
  in offsets and smooth expressions.

* **Recovering original covariate scales**

  `smooth_estimates(..., back_transform = TRUE)` now recovers raw fitting
  covariates, generates grids on their original scale and evaluates model
  expressions on those grids (#412). This supports custom functions such
  as `foo(x)` and tensor products without needing inverse functions.
  Smooth grids retain their existing model-scale defaults.

  Drawing precomputed estimates on the original scale requires retained
  raw coordinates; estimates with only transformed coordinates give an
  informative error. Parametric effects retain both scales by row,
  including for many-to-one transformations. Coordinate columns survive
  confidence-interval calculation and subsetting.

  Typical values of other covariates are not treated as raw axes, and
  numeric `by` multipliers can share a raw axis name. Drawing uses `envir`
  to recover observations. Supplied prediction data remain usable when raw
  observations for rugs or residuals are unavailable.

  Transformed soap-film boundaries cannot be drawn on the original scale
  and give an explicit error. Irregularly spaced surfaces use rectangles
  to preserve covariate positions (#412).

* Full and reduced-rank Markov random field (MRF) smooths now support area
  effect estimates and interval plots (#9). Use `mrf_plot = "qq"` for a
  descriptive QQ plot.

* `draw()`, `assemble()` and `smooth_estimates()` now support simultaneous
  bands for individual smooths (#314). `interval = "credible"` is an alias for
  pointwise `"confidence"` intervals.

* **3D surface plots**

  Numeric smooth and parametric surfaces can now be drawn in 3D with
  `surface = "3d"`, using the optional ggcube package (#393). The existing
  RdBu fill palette, theme, conditioning facets, contours and masking are
  retained. `surface_3d = list(...)` controls the view without applying
  ggcube's theme.

  Lighting defaults to `backface_scale = 1`; supply a custom `light` through
  `surface_3d` to override it. Opaque faces, contours and floor
  observations share depth ordering so hidden features are occluded.

  Set `ci_3d = TRUE` to add translucent lower and upper interval surfaces to
  smooths and numeric parametric interactions across supported model
  classes. `ci_3d_alpha` and `ci_3d_col` style the bounds independently of
  curve ribbons. Interval surfaces retain the estimate's mesh and masking;
  their heights expand the vertical range without training the continuous
  fill scale.

* `draw()` and `assemble()` now accept `surface = "tile"` to draw flat smooth
  surfaces with vector rectangles; the default remains `surface = "raster"`
  (#308). Bivariate, trivariate, quadvariate, tensor-product and soap-film
  surfaces use the same rendering options.

* Random-effect tensor-product plots now accept `re_levels` to select factor
  levels. `n_3d` and `n_4d` also limit the number of factor levels used for
  conditioning facets (#169).

* `draw()` for penalty matrices now accepts `geom = "tile"` to draw individual
  rectangles; `geom = "raster"` remains the default (#392).

* `response_derivatives()` now supports delta-method standard errors and
  pointwise intervals via `uncertainty = "delta"`. Both uncertainty methods
  return `.se`; posterior simulation remains the default.

* **Conditional differences**

  `conditional_differences()` compares conditional fitted values between
  factor levels or joint factor combinations on the link or response
  scale, with pointwise or simultaneous intervals using the delta method
  or shared posterior simulations. Simultaneous bands can cover each
  comparison separately or all comparisons jointly via
  `simultaneous_scope`. `draw()` plots the differences and their intervals
  (#143, #145).

  Grouping two numeric covariates in the first non-comparison condition
  evaluates difference surfaces, drawn as raster, tile or optional ggcube
  3D surfaces. Further conditions supply facets; `ci_3d = TRUE` adds
  interval surfaces. The default `n_vals = NULL` uses 100 values for curves
  and 50 along each surface axis.

* `difference_smooths()` now offers simultaneous intervals for differences of
  smooths at supplied covariate combinations, separately for each pair of
  factor levels, via `interval = "simultaneous"` (#380). Pointwise intervals
  remain the default.

* **Fitted-value intervals and diagnostics**

  `fitted_values()` now offers simultaneous intervals for fitted values at
  supplied covariate combinations via `interval = "simultaneous"` (#4).
  Pointwise intervals remain the default. Simulation size, parallel
  sampling and seeds can be controlled, as in `derivatives()` and
  `partial_derivatives()`.

  `fitted_values()` now emits a message identifying prediction rows with
  unobserved combinations of known categorical predictor levels (#164).

* `all_levels()` is now an alias of `evenly()` with a name intended for
  factors (#217).

* `data_slice()` gains a `.by` argument to evaluate slice expressions within
  observed groups. For example, `x = evenly(x)` can use a separate range for
  each factor level, which is useful for models with factor `by` smooths
  (#196).

* `conditional_values()` now accepts `complete = FALSE` to retain only
  observed combinations of factor conditions while preserving numeric
  prediction grids (#337).

* `confint.gam()` and `smooth_estimates()` now accept `frequentist = TRUE` to
  use the frequentist covariance matrix. The default remains `FALSE`.
  Simultaneous intervals use the selected covariance matrix consistently
  (GRA-005).

* `get_vcov()` is now exported for extracting a model's covariance matrix.

* gratia now supports the `clognorm()` family from mgcvUtils.

* `link()`, `inv_link()` and other family utilities now support `cpois()`,
  `clog()` and `bcg()`. These families also work with `fitted_values()`.
  Single-link families can use their supplied link functions without needing a
  recognised family name.

## Bug fixes

* **Response derivatives**

  `response_derivatives()` now retains unavailable prediction rows with
  missing estimates, standard errors and intervals under both uncertainty
  methods, while respecting explicit `na.action` requests (#437).
  Original row order and response identifiers are preserved.

  Delta uncertainty now rejects response-mean derivatives for scaled t
  families with fitted degrees of freedom at or below one, matching simulation
  (#437). Means with degrees of freedom between one and two and
  linear-predictor derivatives remain supported.

* `add_fitted()` now advertises `value = ".fitted"` in its generic and help,
  matching its established GAM output. Explicit column names are unchanged
  (#441).

* `confint.fderiv()` rejects missing, empty, non-numeric and non-finite confidence
  levels, and levels outside the open interval between zero and one. Vectors
  still warn and use their first value (#441).

* `ref_level()` and `level()` preserve ordered-factor status as well as the
  original levels and their order (#441).

* **Response simulation**

  `simulate.gam()` validates `nsim` as one finite positive whole number within
  the supported integer range before prediction or changes to the random-number
  state, including grouped-family and marginal simulations (#441).

  Explicitly seeded GAM simulations restore the caller's previous random-number
  state, including its absence, on success and error. Unseeded simulation
  continues to advance or initialize the random-number state (#441).

* `overview()` now returns the parametric coefficients for GAMs without
  smooth terms, or an empty overview when `parametric = FALSE` (#438).

* `model_terms()` now returns the coefficient name for intercept-only GAMs
  and models with a single slope and no intercept (#438).

* `response_derivatives()` rejects `ci_level` and `coverage` with guidance to
  use `level` for both delta and simulation uncertainty, including when `level`
  is also supplied. These arguments can no longer silently leave delta intervals
  at the default coverage (#436).

* `fitted_values()` retains small ordinal category probabilities and their
  standard errors at extreme predictor values. Ordinal and multinomial
  intervals remain defined when probabilities round to zero or one, including
  for grouped ordinal models (#434). Closely spaced ordinal thresholds and
  large multinomial predictors retain valid category probabilities.

* `confint(shift = TRUE)` now adds the full intercept of each smooth's own
  linear predictor, including contributions from shared formula blocks.
  `model_constant()` returns zero for predictors without an intercept
  and keeps selected predictor names aligned; `add_fitted()` uses native
  prediction constants for term predictions (#433).

* `log_lik()` now divides the dispersion by each observation's weight for
  Gamma and inverse Gaussian densities, including grouped-family components
  (#432). This corrects their normalising constants and the LOO and WAIC
  scores calculated from them.

* `response_derivatives()` now summarises each response category and
  multivariate response separately within each prediction row, including
  categories in grouped families (#431). This corrects derivative estimates,
  standard errors and intervals that previously mixed those quantities.
  Summaries preserve the supplied row order and no longer retain draw,
  chain or iteration identifiers.

* Bivariate factor-by basis plots now put each basis function in its own
  panel, instead of drawing their surfaces on top of one another (#428).

* Gumbel LSS fitted location estimates are no longer exponentiated. Natural
  location is computed separately from the scale-dependent expectation.

* Grouped plots of factor-by smooths now keep different smooth terms and
  linear predictors separate, including when drawing saved smooth estimates.
  Models with several factor-by terms no longer fail during plot assembly.

* For GJRM models, `scales = "fixed"` now shares effect ranges without
  expanding the covariate axes of two-dimensional smooth plots.

* `appraise(rank = TRUE)` now uses within-response or within-family predictor
  ranks in ordinary diagnostic panels, matching the standalone and scalable
  plots.

* `partial_residuals()` and `add_partial_residuals()` now give an informative
  error for models without working residuals or working weights, including
  mgcv's location-scale families. `draw(residuals = TRUE)` warns once and draws
  the smooths without partial residuals for these models (#414).

* `draw.difference_smooth()` now uses supplied `xlab` and `caption` values
  when drawing differences between curves.

* `n_smooths()` now counts smooth terms in glmmTMB models across the
  conditional, zero-inflation and dispersion predictors, excluding native
  random effects. Models without smooths return zero (#240).

* Direct calls to `smooth_estimates()` now work for `GJRM::gamlss()` models
  (#352). Estimates use the final fitted coefficients and covariance, with
  smooth selection across equations and parameter labels retained for drawing
  saved estimates. Frequentist covariance also comes from the final fit;
  unavailable smoothing-parameter corrections warn and use its Bayesian
  covariance.

* B-spline evaluation handles empty data and factor-by calls whose evaluation
  data omit fitted levels, including constrained tensor products.
  `smooth_estimates()` retains estimate, standard error and predictor
  columns and their types when no evaluation rows remain. Nested output
  keeps each selected smooth with its empty data table.

* **Sampling diagnostics and workers**

  MH acceptance rates are extracted from the correct mgcv return
  components.

  Workers started for sampling exit cleanly so coverage data can be
  written completely.

* **Simulated data**

  `data_sim()` now transforms the `eg6` predictor once and returns the
  predictor used for sampling. The `eg5` and `eg6` examples accept sample
  sizes not divisible by four.

  Grouped-family simulations respect seeds, parameters, ordinal aliases
  and repeated family positions; `dist` is ignored throughout that legacy
  example.

  Explicit seeds restore an absent RNG state, and reference generation
  forwards the requested Tweedie power. Vector Tweedie parameters now use
  the correct compound Poisson-Gamma law.

* Large QQ reference simulations now close their file connections before
  removing temporary files, allowing cleanup to complete on Windows.

* **SCAM estimates, predictions and sampling**

  SCAM smooth estimates and standard errors no longer depend on the other
  rows in the evaluation grid. Single-row and short-grid evaluations are
  supported.

  `overall_uncertainty = TRUE` includes intercept variance and its
  covariance with the smooth; `FALSE` includes smooth uncertainty alone.
  `unconditional = TRUE` now gives an explicit error because SCAM
  calculations do not support smoothing-parameter uncertainty.

  Smooth and full-model predictions now share coefficient mappings,
  including endpoint and local constraints. Formula transformations,
  offsets and missing training rows retain their fitted meaning.

  The `mifo` smooth is rendered.

  SCAM coefficient draws now sample unrestricted fitting parameters and
  apply the fitted positivity transformation, preserving the smooth
  constraints. Singular positive semidefinite covariance matrices are
  supported; invalid covariance matrices and non-finite transformed draws
  give explicit errors.

  Gaussian AR1 response simulation now uses fitted correlation and series
  boundaries instead of drawing independent responses.

  Concurvity excludes basis columns whose coefficients are fixed to zero,
  restoring the correct calculation for curves that become flat.

* **Soap-film derivatives and sampling**

  Partial derivatives, fitted samples and smooth samples of soap-film
  smooths include the contribution from known boundary values.
  Complete-predictor derivative samples also differentiate those fixed
  values, without adding coefficient uncertainty. Term exclusions retain
  only the appropriate fixed contributions.

  Separate sampling per smooth now correctly subsets supplied coefficient
  draws.

* Multinomial fitted samples now honour `scale = "linear_predictor"`, returning
  log odds relative to the reference category instead of probabilities. Both
  scales retain all category labels; the reference predictor is zero. Models
  with one non-reference category also use this output format.

* GJRM smooth selection now uses one order across all model equations.
  Character selections draw only the equations containing the selected smooths;
  repeated labels have distinct keys returned by `smooths()`. `wrap = FALSE`
  returns a flat list of plots.

* Smooth plots drawn with `scales = "fixed"` calculate their shared ranges
  from finite estimates and interval bounds. Missing soap-film boundary rows
  no longer cause each component to use a separate colour scale.

* Parametric interaction plots for glmmTMB and sdmTMB retain their term and
  coordinate information across model components. This prevents factor
  interactions from being mistaken for numeric effects when drawing rugs.

* Response distributions now correctly account for Gaussian and Gamma
  dispersion with observation weights, factor-valued Bernoulli responses and
  numeric binomial trial counts.

* Beta diagnostics now include the probability mass created by clipping native
  draws at epsilon endpoints. Binomial probabilities immediately below an
  observed value are calculated more accurately.

* GEV and sinh-arcsinh residuals now use stable means and correct centering
  and signs. Undefined moments remain unavailable. Invalid sinh-arcsinh
  deviance contrasts give an explanatory error; PIT/quantile checks remain
  available.

* Quantile residuals now calculate probabilities on the logarithmic scale in
  the smaller tail for families that support this calculation, including
  Poisson and negative binomial. This avoids numerical infinities without
  clipping probabilities.

* Grouped-binomial quantile residuals now randomise over the probability mass
  of one success rather than one whole response proportion.

* BAM models using a generalized estimating equations (GEE) approximation
  for AR1 working residuals retain marginal PIT, quantile and QQ checks,
  while Gaussian identity-link AR1 response simulation remains supported. QQ
  plots no longer switch automatically to normal references or use bands
  that assume independence. The working correlation does not specify a joint
  response distribution, so joint response simulation and posterior
  predictive checks are unavailable for these GEE fits.

* QQ bands for dependent responses now use joint simulations when available.
  Otherwise, the plot explains why the bands are omitted.

* Fitted and posterior response draws, response simulations and diagnostics
  now retain offsets supplied as fitting arguments for fitting observations,
  including ordinary `bam()` and extracted `gamm4()` GAM components. Recovery
  keeps formula offsets, prescribed smooth boundaries and additional group
  effects separate. Explicit prediction data still follow the fitter's
  prediction convention and do not inherit argument-only fitting offsets.

* Explicit seeds now preserve the caller's random-number state for coefficient
  and response draws. `worm_plot()` also honours `seed`, including through
  `appraise(use_worm = TRUE)`, and restores the caller's random-number state
  after seeded calls (#407).

* **Missing observations and prediction rows**

  For models fitted with `na.exclude`, default fitted values, partial
  residuals, fitted and posterior samples, simulations and
  `quantile_residuals()` now restore excluded rows as `NA`. Diagnostics
  align retained observations with predictors, and histogram bin selection
  handles missing residuals (#349, #402).

  Explicit prediction data preserve row positions by default: missing
  required predictors yield `NA`, while missing responses alone do not
  prevent prediction.

  Discrete `bam()` fitted-value intervals restore exclusions once and
  accept stored fitting frames without relying on tibble subsetting in
  native prediction.

* `link()`, `inv_link()` and `extract_link()` now reject unsupported
  parameters consistently. For `twlss()`, `scale`, `sigma` and `phi` select
  the third linear predictor, and `power` selects the second. Explicit
  `which_eta` indices for `mvn()` and `multinom()` are now respected and
  validated.

* Custom smooth labels supplied through `draw(..., subtitle = ...)` and other
  label arguments now reach the smooth renderer correctly.

* Parametric plot coordinates on the model scale now contain the evaluated
  predictor rather than its coefficient-weighted partial contribution (#412).

* `typical_values()` and `data_combos()` no longer require an `envir` argument
  to recover model covariates.

* Automatic surface fill ranges now ignore non-finite estimates and standard
  errors (#308).

* **Random-effect tensor-product plots**

  Random-effect tensor products now plot curves, categorical estimates and
  faceted surfaces consistently across `te()`, `ti()` and `t2()` (#169).
  Two-factor products use points with confidence intervals, faceted by the
  second factor, rather than empty line plots. Products with one factor
  use the continuous covariate on the x axis regardless of margin order
  (#395).

  Three- and four-variable `t2()` smooths with factor margins produce
  faceted surfaces.

* Spline-on-the-sphere plots now use valid cells at the poles and date line,
  with orthographic clipping and separate factor-by panels (#277).

* `draw.gamlss()` now sizes its plot layout using the panels actually drawn,
  including parametric effects, avoiding rendering errors for GJRM models.

* Posterior sampling for GAMs, including the GAM component of gamm4 models,
  now falls back to `mgcv::rmvn()` with a warning if the default sampler's
  Cholesky decomposition fails (#332).

* `smooth_estimates()` now uses the fitted coefficient order for random-effect
  interactions fitted with `bam(discrete = TRUE)`, correcting estimates and
  standard errors (#387).

* `assemble()` and `draw()` now honour `terms` when selecting parametric
  effects, while retaining the selected smooths (#368).

* `variance_comp()` now handles variance components without confidence
  intervals, including default GCV fits, and returns a typed empty table when
  `mgcv::gam.vcomp()` returns no components (#377).

* `rtw()` now assigns Gamma shapes to the correct observations when Tweedie
  power varies, including in `twlss()` response simulation. Scalar parameters
  are expanded consistently, and incompatible lengths or non-finite inputs are
  rejected (#409).

* `data_sim()` now honours custom cut points for ordinal responses and
  validates that they are finite, strictly increasing and consistent with the
  requested integer number of categories (#408).

* `draw.rootogram()` now applies the square-root transformation when a custom
  y-axis label is supplied, keeping plotted frequencies independent of the
  label (#406).

* `overview()` now reports one degree of freedom per parametric coefficient,
  avoiding incorrect values and row-length errors for factors and models
  without an intercept (#405).

* Loading gratia no longer changes `weights()` dispatch for linear models. QQ
  and worm plots preserve observation weights and excluded rows (#404).

* `fixed_effects()` now excludes smooth coefficients using their model
  indices, so tensor-product smooth coefficients no longer appear as
  parametric effects (#403).

* `data_combos()` now honours `complete = FALSE`, returning only observed
  combinations of factor levels while retaining typical continuous values and
  variable selection (#400).

* `derivatives()` and `partial_derivatives()` now preserve the correct smooth
  selection when excluding random-effect or unsupported smooths, avoiding
  logical-index recycling warnings under R-devel. `partial_derivatives()`
  aligns focal variables and output labels with the selected multivariate
  smooths, including when univariate smooths are skipped (#396).

* `derivatives.gam()` now explains when requested smooths are unsupported,
  with a message if some cannot be handled or an error if none can. This
  covers multivariate smooths such as `s(x1, x2)`, `te(x1, x2)`, `ti(x1, x2)`
  and `t2(x1, x2)`, as well as random-effect smooths (#389, reported by
  @mikeguggis in #362).

* `compare_smooths()` now honours `unconditional = TRUE` when computing
  standard errors (#397).

* Adding a constant to parametric effects now shifts estimates and confidence
  intervals together, including when drawing or assembling plots (#398).

* Gaussian rootograms now use response-scale means and standard deviations
  adjusted for prior weights when computing expected counts. Nonpositive or
  non-finite prior weights are rejected (#399).

* `fitted_values()` now computes correctly ordered confidence bounds for
  location-scale-shape, zero-inflated Poisson (`ziplss()`) and Tweedie
  location-scale-shape (`twlss()`) models. Response-scale intervals retain
  ordered endpoints for decreasing inverse links, as do transformed intervals
  from `confint.gam()` (GRA-001).

* `post_draws.default()` and `generate_draws.gam()` no longer use `seed = 0`
  for every call with `seed = NULL`. They now generate a valid integer seed
  without modifying the caller's random-number state (GRA-002).

* `derivatives()` now holds numeric `by` variables fixed when computing
  derivatives of smooths. Previously, finite differences shifted both the
  smooth covariate and its numeric multiplier, giving incorrect derivatives,
  standard errors and intervals. Only the current smooth's covariate is now
  shifted, for supplied data and automatically generated grids (GRA-003).

* `difference_smooths()` now matches smooth covariate combinations across
  factor levels before computing contrasts. Previously, different row orders
  in supplied data could silently compare different covariate locations,
  giving incorrect differences, standard errors and intervals. Duplicate
  combinations and mismatched grids now produce informative errors;
  comparisons at a single location also work (GRA-004).

* `confint.gam()` now uses consistent covariance and standard errors for
  simultaneous intervals: `unconditional = TRUE` includes uncertainty from
  estimating smoothing parameters in the Bayesian covariance when available,
  while `frequentist = TRUE` selects the frequentist covariance (GRA-005).
  Both pointwise and simultaneous intervals respect `overall_uncertainty`,
  including uncertainty in the overall mean by default for ordinary GAMs;
  native scasm fits default to `FALSE`. Requesting multiple smooths no longer
  causes an error (GRA-006).

* `simulate.gamm()` now respects the supplied `data` argument.
  `smooth_samples.gamm()` and `overview.gamm()` now forward additional
  arguments to their underlying GAM methods instead of ignoring them
  (GRA-008).

# gratia 0.11.2

## User visible changes

* *gratia* now only handles shape constrained models fitted using *scam*
  version 1.2-21 or later. This is due to breaking changes in *scam*, and new
  functionality in that package, which simplifies *gratia*.

* *gratia* also now depends on *dplyr* version 1.2.0 or later.

* Histogram bars drawn by `residuals_hist_plot()` and points in `qq_plot()`,
  `residuals_linpred_plot()`, `observed_fitted_plot()`, and `worm_plot()`, and
  hence `appraise()`, are now filled with a dark shade of grey and use a light
  colour for the border of the bars.

##  New features

* `derivatives()` now works for constrained factor smooths (`bs = "sz"`),
  treating them in the same way as random factor smooths (`bs = "fs"`).
  Lack of support reported by @vrest-png #362

  Also handles higher order `bs = "sz"` terms.

* `draw.derivatives()` can now differentiate random and constrained factor
  smooths through the colour aesthetic via new argument 
  `differentiate_factor_smooths`.

* `model_terms()` returns the names of all terms in a model. #388

## Bug fixes

* `parametric_effects()` now excludes interactions in models with multiple linear
  predictors, matching the existing single-predictor behaviour (#369).

* `conditional_values()` would fail if supplied a numeric vector of data to 
  condition on via the `condition` argument. #366

* *gratia* worked inconsistently with tensor product smooths containing a
  random effect `bs = "re"` marginal. Reported #358 and fixed by
  @asgersvenning #360, with additional changes by Gavin Simpson.

* Argument `mvn_method` was not being passed on to the lower workhorse functions
  that did the sampling. #381 

* `response_derivatives()` would fail if there were no other terms in the model 
  but the focal term.

# gratia 0.11.1

## User visible changes

* `residuals_hist_plot()` and hence `appraise()` now centre the middle bin of
  the histogram at 0. In part this was due to *ggplot2*'s new binning algorithm
  leading to potentially odd choices for the bin breaks in the context of model
  residuals.

##  New features

* `quantile_residuals()` now supports more of *mgcv*'s families:
    1. `scat()`,
    2. `nb()`
    3. `betar()`,
    4. `tw()`.

* Several user friendliness improvements in `partial_derivatives()`:
    * now better handles the case where there are multiple smooths for which
      partial derivatives are required,
    * correctly identifies smooths that involve random effect terms (i.e. any
      smooth or tensor product marginal smooths with `bs %in% c("re", "fs")`)
      and ignores them,
    * identifies and ignores univariate smooths, and
    * displays more informative error messages to explain what was wrong.
  
  Part of discussion with @BenFN121 in #356.

## Bug fixes

* `partial_derivatives()` threw an error when the `select` argument was used.
  #356 reported by @BenFN121

* `draw.conditional_values()` was setting a label on the fill aesthetic even if
  that aesthetic was not being used. In *ggplot2* v4.0.0 this resulted in a
  warning, which is fixed.

# gratia 0.11.0

*gratia* now has a [paper](https://doi.org/10.21105/joss.06962) describing the
package in the [Journal of Open Source Software](https://joss.theoj.org/). If
you use *gratia* in your work, please cite this paper rather than the generic
citation previously created by `citation()`:

> Simpson, G.L. (2024) gratia: An R package for exploring generalized additive
> models. *The Journal of Open Source Software* **9**, 6962.

## User visible changes

* Experimental support for computing with parallel processes via the *purrr* 📦
  in `smooth_estimates()` and `basis()`, though it is of limited use in the 
  latter. Parallel processing will operate if a call to `mirai::daemons()` has
  been issued in the current R session.

* Rug plots in `draw.gam()` now only show the unique data values to avoid 
  needless overplotting. This behaviour is controlled via new argument
  `distinct_rug`. Setting this to `FALSE` will get the previous behaviour.

* `simulate.gam()` gains a `print()` method and thus no longer prints its
  attributes.

* `citation("gratia")` now suggests to use Simpson (2024) *Journal of Open
  Source Software* **9**(104), 9862, when citing the package.

* `simulate.gam()` now returns a data frame, bringing it into line with
  `stats::simulate.lm()`.

* With soap film smooths, `draw.gam()` no longer plots points that are deemed 
  outside the boundary of the soap film (as determined by `mgcv::inSide()`).
  `smooth_estimates()` continues to return these points however, because it
  might be confusing to suddenly lose evaluation points. This behaviour can be
  controlled by the argument `clip`, which is available to `draw.gam()` and
  `smooth_estimates.gam()` currently.

* `draw.compare_smooths()` now uses "Partial effect" as the y-axis label, in
  common with other partial effect plots in *gratia*.

## New features

* `assemble()` is a new generic function for assembling sets of plots from model
  objects. It is effectively `draw()` but without actually plotting on the
  current device. Instead it returns a list of `ggplot` objects. Currently, a
  method for `"gam"` models, `assemble.gam()`, is provided which `draw.gam()`
  uses. Part of the wish of @ha0ye #35

* `appraise()` with `method = "simulate"` can now handle *mgcv*'s multivariate
  normal models fitted with `family = mvn()`and Tweedie location scale models
  fitted with `family = twlss()`.

* `fix_family_rd()` is an (currently) internal function that extends 
  `mgcv::fix.family.rd()`. This function adds a `rfoo()` function to the `$rd` 
  component of the `family()` object stored in the model. This function is used
  to generate new values of the response at supplied values of the linear
  predictor. `fix_family_rd()` calls `mgcv::fix.family.rd()` so it is backward 
  compatible with *mgcv*'s behaviour, but it is designed to extend the
  behaviour through support for a richer set of models.

  Currently, `fix_family_rd()` adds support for multivariate normal models 
  fitted with `family = mvn()` and location-scale Tweedie models fitted with
  `family = twlss()`, but the latter is currently slow.

  The functionality of `fix_family_rd()` may move to another package or even 
  to *mgcv* if suitable.

* `difference_smooths()` can now be used to compare named smooths using
  `select = c("s(x):f1", "s(x):f2")` etc, where the vector of smooth names must
  match exactly something returned by `smooths()`. Smooths to be compared must
  have the same covariate (`x` in the example) and same factor-by variable
  (`f` in the example). Tensor product factor-by smooths are handled likewise.
  This effectively handles the problem of #315, reported by @3rd3.

  Additionally, `difference_smooths()` with `group_means = TRUE` can now
  include group means specified using a random effect smooth, e.g. following
  the above example for `select`, a random effect smooth `s(f, bs = "re")`, with
  name `s(f)`, is looked for in the model matrix and its effects included in the
  comparisons.

* `data_slice()` gains argument `.observed_only` which allows simple filtering
  of the combinations of values specified in the slice to those that are in the
  data frame supplied or in the model frame of a fitted GAM.

* `posterior_samples()`, `predicted_samples()`, `fitted_samples()`, and
  `simulate.gam()` now simulate for all linear predictors (i.e., all responses)
  in `mvn()` and `multinom()` models as these are all location parameter linear
  predictors.

* `fitted_values()` now works for GAMs fitted with families `ziP()`, `mvn()`,
  and `multinom()`. The non-working of `fitted-values()` for `ziP()` models was
  reported in #341 by @rroyaute

* `posterior_samples()`, `fitted_samples()`, `predicted_samples()`,
  `derivatives_samples()`, and `response_derivatives()` now all work with
  models fitted by `scam::scam()`.

* `basis()` can now take a vector of coefficients for the basis functions when
  using the default method: `basis(s(x), data = df, coefficients = rnorm(10))`.
  Wish of #136

* `draw.gam()` and `draw.smooth_estimates()` can now handle soap film smooths 
  with known boundary values, and with nested boundaries.

* `scale_fill_partial_effect()` is a *ggplot* scale function that implements
  the default diverging red-blue gradient used by *gratia* when plotting partial
  effect surfaces for multivariate smooths.

* `quantile_residuals()` computes probability integral transform (PIT) and
  randomised quantile residuals for some GAMs and GLMs. Currently only models
  fitted with a `gaussian()`, `binomial()`, `Gamma()`, or `poisson()` family
  are supported.

* `residuals_linpred_plot()` and `residuals_hist_plot()` can now use PIT or
  randomised quantile residuals in their diagnostic plots.

## Bug fixes

* `conditional_values()` would fail if a variable it was conditioning on was
  also a name of a function. #323 (but not really fixed when this was initially
  closed)

* `parametric_effects()` was only keeping one set of factor levels when there 
  were multiple parametric factor terms in a model. Reported by @tamas-ferenci 
  as part of discussion to #284

* `mh_draws()` would fail in the case of drawing only `n = 1` samples. #328
  Reported by @zsusswein

* `simulate.gam()` now works for the `ocat()` family, which has a non-standard
  `rd` function in its `family()` object. Reported by @hhp94, #319

* `gaussian_draws()` would fail in some cases with *gamm4* models because the
  coefficients were returned as a vector, not the expected matrix. #332 reported
  by @JaredStufft-Elion

* `draw.smooth_estimates()` would generate very many warnings when plotting
  spline on the sphere smoothers (`bs = "sos"`) because `geom_tile()` was
  creating tiles that extended beyond valid geographical coordinates for the
  tile centre coordinates that it was being provided. The function that creates
  the data to evaluate the spline at now tries to avoid this. Note this fix is
  not a complete solution. #334 Reported by @StefanoMezzini

* `link()` and `inv_link()` failed to extract the link an inverse link functions
  for `mvn()` and `multinom()` models.

* `link()` and `inv_link()` now work correctly for models fitted with the
  `ziP()` family. #341 Reported by @rroyaute

* `basis.scam()` was incorrectly trying to identify the smooths named in 
  `select`.

* `basis.scam()` would fail if the SCAM contained any non-constrained (i.e. 
  ordinary) smooths.

* `overview()` was displaying more significant digits in the p values than it
  was accurate to in terms of reporting p values as "<0.001". A new argument
  `digits` controls how many digits are used when formatting p values, with 
  default of `3`.

* `theta()` now works for models fitted with the `gfam()` family. Previously it
  would fail because it assumes all `family()$getTheta` functions had an
  argument `trans` to transform the theta values to the correct scale.

* `overview()` was not showing the model intercept, and was displaying slightly
  different results from those shown by `summary.gam()`.

* `smooth_estimates()` was not adding any known boundary condition that was
  part of a soap film smooth.

* `draw.gamlss()` (for `GJRM::gamlss()` models) now doesn't fail with an error 
  if there is more than a single smooth in any of the linear predictors.

# gratia 0.10.0

## New features

* `conditional_values()` and its `draw()` method compute and plot predictions 
  from a fitted GAM that are conditional on one or more covariates. The 
  function is a wrapper around `fitted_values()` but allows the user simple ways
  to specify which covariates to condition on and at what values those
  covariates should take. It provides similar functionality to
  `marginaleffects::plot_predictions()`, but is simpler. See #300.

* `penalty()` and `basis()` can now allow the smooth to be reparameterized such
  that the resulting basis has an identity matrix. This more clearly highlights
  the penalty null space, the functions that the penalty has no effect on.

* `draw.gam()` and `draw.smooth_estimates()` gain argument `caption`, which, if
  set to `FALSE` will not plot the smooth basis type as a caption on the plot.
  #307

* `appraise()` and `qq_plot.gam()` now allow the user to set a random seed that
  is used when generating reference quantiles with `method = "uniform"` or
  `method = "simulate"`.

## Bug fixes

* `derivative_samples()` was ignoring the `scale` argument. #293 Reported by
  @jonathonmellor

* Argument `level` to `derivative_samples()` was included accidentally. As of
  v0.9.2.9002 this argument is deprecated and using it will now generate a
  warning. #291

* `draw()` was not plotting cyclic P spline smooths. Reported by @Zuckerbrot
  #297

* `derivatives()` would fail for `"fs"` smooths with other parametric effects in
  the model. Reported by @mahonmb #301

* Partial residuals in `partial_residuals()` and `draw.gam()` were wrong for
  GAMs fitted with `family = binomial()` where the `weights` argument contained
  the binomial sample sizes because the prior weights were being used to form
  weighted working residuals. Now working weights are used instead. Reported by
  @emchuron #273

* Internal function `gammals_link()` was expecting `"theta"` as a synonym for
  the scale parameter but the master table has `"phi"` coded as the synonym.
  Now both work as expected.

* `level()` assumed that `level` would have only a single value even though it
  could handle multiple levels. #321

# gratia 0.9.2

## Breaking changes

* `parametric_effects()` slightly escaped the great renaming that happened for
  0.9.0. Columns `type` and `term` did not gain a prefix `.`. This is now
  rectified and these two columns are now `.type` and `.term`.

## User visible changes

* Plots of random effects are now labelled with their smooth label. Previously,
  the title was taken fro the variable involved in the smooth, but this doesn't
  work for terms like `s(subject, continuous_var, bs = "re")` for random slopes, 
  which previously would have the title `"subject"`. Now such terms will have
  title `"s(subject,continuous_var)"`. Simple random intercept terms,
  `s(subject, bs = "re")`, are now titled `"s(subject)"`. #287

* The vignettes
    1. `custom-plotting.Rmd`, and
    2. `posterior-simulation.Rmd`
  were moved to `vignettes/articles` and thus are no longer available as package
  vignettes. Instead, they are accessible as Articles through the package
  website: <https://gavinsimpson.github.io/gratia/>

## New features

* `fitted_samples()` now works for `gam()` models with multiple linear
  predictors, but currently only the location parameter is supported. The
  parameter is indicated through a new variable `.parameter` in the returned
  object.

## Bug fixes

* `partial_residuals()` was computing partial residuals from the *deviance*
  residuals. For compatibility with `mgcv::plot.gam()`, partial residuals are
  now computed from the *working* residuals. Reported by @wStockhausen #273

* `appraise()` was not passing the `ci_col` argument on `qq_plot()` and
  `worm_plot()`. Reported by Sate Ahmed.

* Couldn't pass `mvn_method` on to posterior sampling functions from user facing
  functions `fitted_samples()`, `posterior_samples()`, `smooth_samples()`,
  `derivative_samples()`, and `repsonse_derivatives()`. Reported by @stefgehrig
  #279

* `fitted_values()` works again for quantile GAMs fitted by `qgam()`.

* `confint.gam()` was not applying `shift` to the estimate and upper and lower
  interval. #280 reported by @TIMAVID & @rbentham

* `parametric_effects()` and `draw.parametric_effects()` would forget about the
  levels of factors (intentionally), but this would lead to problems with
  ordered factors where the ordering of levels was not preserved. Now,
  `parametric_effects()` returns a named list of factor levels as attribute
  `"factor_levels"` containing the required information and the order of levels
  is preserved when plotting. #284 Reported by @mhpob

* `parametric_effects()` would fail if there were parametric terms in the model
  but they were all interaction terms (which we don't currently handle). #282

# gratia 0.9.0

## Breaking changes

* Many functions now return objects with different named variables. In order to
  avoid clashes with variable names used in user's models or data, a period
  (`.`) is now being used as a prefix for generated variable names. The
  functions whose names have changed are: `smooth_estimates()`,
  `fitted_values()`, `fitted_samples()`, `posterior_samples()`, `derivatives()`,
  `partial_derivatives()`, and `derivative_samples()`. In addition,
  `add_confint()` also adds newly-named variables.

      1. `est` is now `.estimate`,
      2. `lower` and `upper` are now `.lower_ci` and `.upper_ci`,
      3. `draw` and `row` and now `.draw` and `.row` respectively,
      4. `fitted`, `se`, `crit` are now `.fitted`, `.se`, `.crit`, respectively
      5. `smooth`, `by`, and `type` in `smooth_estimates()` are now `.smooth`,
         `.by`, `.type`, respectively.

* `derivatives()` and `partial_derivatives()` now work more like
  `smooth_estimates()`; in place of the `var` and `data` columns, *gratia* now
  stores the data variables at which the derivatives were evaluated as columns
  in the object with their actual variable names.

* The way spline-on-the-sphere (SOS) smooths (`bs = "sos"`) are plotted has
  changed to use `ggplot2::coord_sf()` instead of the previously-used
  `ggplot2::coord_map()`. This changed has been made as a result of
  `coord_map()` being soft-deprecated ("superseded") for a few minor versions of
  ggplot2 by now already, and changes to the guides system in version 3.5.0 of
  ggplot2.
  
  The axes on plots created with `coord_map()` never really worked
  correctly and changing the angle of the tick labels never worked. As
  `coord_map()` is superseded, it didn't receive the updates to the guides
  system and a side effect of these changes, the code that plotted SOS smooths
  was producing a warning with the release of ggplot2 version 3.5.0.

  The projection settings used to draw SOS smooths was previously controlled via
  arguments `projection` and `orientation`. These arguments do not affect
  `ggplot2::coord_sf()`, Instead the projection used is controlled through new
  argument `crs`, which takes a PROJ string detailing the projection to use or
  an integer that refers to a known coordinate reference system (CRS). The
  default projection used is `+proj=ortho +lat_0=20 +lon_0=XX` where `XX` is the
  mean of the longitude coordinates of the data points.

### Defunct and deprecated functions and arguments

#### Defunct

* `evaluate_smooth()` was deprecated in gratia version 0.7.0. This function and
  all it's methods have been removed from the package. Use `smooth_estimates()`
  instead.

#### Deprecated functions

The following functions were deprecated in version 0.9.0 of gratia. They will
eventually be removed from the package as part of a clean up ahead of an
eventual 1.0.0 release. These functions will become defunct by version 0.11.0 or
1.0.0, whichever is released soonest.

* `evaluate_parametric_term()` has been deprecated. Use `parametric_effects()`
  instead.

* `datagen()` has been deprecated. It never really did what it was originally
  designed to do, and has been replaced by `data_slice()`.

#### Deprecated arguments

To make functions in the package more consistent, the arguments `select`,
`term`, and `smooth` are all used for the same thing and hence the latter two
have been deprecated in favour of `select`. If a deprecated argument is used, a
warning will be issued but the value assigned to the argument will be assigned
to `select` and the function will continue.

## User visible changes

* `smooth_samples()` now uses a single call to the RNG to generate draws from
  the posterior of smooths. Previous to version 0.9.0, `smooth_samples()` would
  do a separate call to `mvnfast::rmvn()` for each smooth. As a result, the
  result of a call to `smooth_samples()` on a model with multiple smooths will
  now produce different results to those generated previously. To regain the
  old behaviour, add `rng_per_smooth = TRUE` to the `smooth_samples()` call.
  
  Note, however, that using per-smooth RNG calls with `method = "mh"` will be
  very inefficient as, with that method, posterior draws for all coefficients
  in the model are sampled at once. So, only use `rng_per_smooth = TRUE` with
  `method = "gaussian"`.

* The output of `smooth_estimates()` and its `draw()` method have changed for
  tensor product smooths that involve one or more 2D marginal smooths. Now,
  if no covariate values are supplied via the `data` argument,
  `smooth_estimates()` identifies if one of the marginals is a 2d surface and
  allows the covariates involved in that surface to vary fastest, ahead of terms
  in other marginals. This change has been made as it provides a better default
  when nothing is provided to `data`.

  This also affects `draw.gam()`.

* `fitted_values()` now has some level of support for location, scale, shape
  families. Supported families are `mgcv::gaulss()`, `mgcv::gammals()`,
  `mgcv::gumbls()`, `mgcv::gevlss()`, `mgcv::shash()`, `mgcv::twlss()`, and
  `mgcv::ziplss()`.

* *gratia* now requires *dplyr* versions >= 1.1.0 and *tidyselect* >= 1.2.0.

* A new vignette *Posterior Simulation* is available, which describes how to
  do posterior simulation from fitted GAMs using {gratia}.

## New features

* Soap film smooths using basis `bs = "so"` are now handled by `draw()`,
  `smooth_estimates()` etc. #8

* `response_derivatives()` is a new function for computing derivatives of the
  response with respect to a (continuous) focal variable. First or second
  order derivatives can be computed using forward, backward, or central
  finite differences. The uncertainty in the estimated derivative is determined
  using posterior sampling via `fitted_samples()`, and hence can be derived
  from a Gaussian approximation to the posterior or using a Metropolis Hastings
  sampler (see below.)

* `derivative_samples()` is the work horse function behind
  `response_derivatives()`, which computes and returns posterior draws of the
  derivatives of any additive combination of model terms. Requested by
  @jonathanmellor #237

* `data_sim()` can now simulate response data from gamma, Tweedie and ordered
  categorical distributions.

* `data_sim()` gains two new example models `"gwf2"`, simulating data only from
  Gu & Wahba's *f2* function, and `"lwf6"`, example function 6 from Luo & Wahba
  (1997 JASA 92(437), 107-116).

* `data_sim()` can also simulate data for use with GAMs fitted using
  `family = gfam()` for grouped families where different types of data in
  the response are handled. #266 and part of #265

* `fitted_samples()` and `smooth_samples()` can now use the Metropolis Hastings
  sampler from `mgcv::gam.mh()`, instead of a Gaussian approximation, to sample
  from the posterior distribution of the model or specific smooths
  respectively.

* `posterior_samples()` is a new function in the family of `fitted_samples()`
  and `smooth_samples()`. `posterior_samples()` returns draws from the
  posterior distribution of the response, combining the uncertainty in the
  estimated expected value of the response and the dispersion of the response
  distribution. The difference between `posterior_samples()` and
  `predicted_samples()` is that the latter only includes variation due to
  drawing samples from the conditional distribution of the response (the
  uncertainty in the expected values is ignored), while the former includes
  both sources of uncertainty.

* `fitted_samples()` can new use a matrix of user-supplied posterior draws.
  Related to #120

* `add_fitted_samples()`, `add_predicted_samples()`, `add_posterior_samples()`,
  and `add_smooth_samples()` are new utility functions that add the respective
  draws from the posterior distribution to an existing data object for the
  covariate values in that object: `obj |> add_posterior_draws(model)`. #50

* `basis_size()` is a new function to extract the basis dimension (number of
  basis functions) for smooths. Methods are available for objects that inherit
  from classes `"gam"`, `"gamm"`, and `"mgcv.smooth"` (for individual smooths).

* `data_slice()` gains a method for data frames and tibbles.

* `typical_values()` gains a method for data frames and tibbles.

* `fitted_values()` now works with models fitted using the `mgcv::ocat()`
  family. The predicted probability for each category is returned, alongside a
  Wald interval created using the standard error (SE) of the estimated
  probability. The SE and estimated probabilities are transformed to the logit
  (linear predictor) scale, a Wald credible interval is formed, which is then
  back-transformed to the response (probability) scale.

* `fitted_values()` now works for GAMMs fitted using `mgcv::gamm()`. Fitted
  (predicted) values only use the GAM part of the model, and thus exclude the
  random effects.

* `link()` and `inv_link()` work for models fitted using the `cnorm()` family.

* A worm plot can now be drawn in place of the QQ plot with `appraise()` via
  new argument `use_worm = TRUE`. #62

* `smooths()` now works for models fitted with `mgcv::gamm()`.

* `overview()` now returns the basis dimension for each smooth and gains an
  argument `stars` which if `TRUE` add significance stars to the output plus a
  legend is printed in the tibble footer. Part of wish of @noamross #214

* New `add_constant()` and `transform_fun()` methods for `smooth_samples()`.

* `evenly()` gains arguments `lower` and `upper` to modify the lower and / or
  upper bound of the interval over which evenly spaced values will be generated.

* `add_sizer()` is a new function to add information on whether the derivative
  of a smooth is significantly changing (where the credible interval excludes
  0). Currently, methods for `derivatives()` and `smooth_estimates()` objects
  are implemented. Part of request of @asanders11 #117

* `draw.derivatives()` gains arguments `add_change` and `change_type` to allow
  derivatives of smooths to be plotted with indicators where the credible
  interval on the derivative excludes 0. Options allow for periods of decrease
  or increase to be differentiated via `change_type = "sizer"` instead of the
  default `change_type = "change"`, which emphasises either type of change in
  the same way. Part of wish of @asanders11 #117

* `draw.gam()` can now group factor by smooths for a given factor into a single
  panel, rather than plotting the smooths for each level in separate panels.
  This is achieved via new argument `grouped_by`. Requested by @RPanczak #89

  `draw.smooth_estimates()` can now also group factor by smooths for a given
  factor into a single panel.

* The underlying plotting code used by `draw_smooth_estimates()` for most
  univariate smooths can now add change indicators to the plots of smooths if
  those change indicators are added to the object created by
  `smooth_estimates()` using `add_sizer()`. See the example in
  `?draw.smooth_estimates`.

* `smooth_estimates()` can, when evaluating a 3D or 4D tensor product smooth,
  identify if one or more 2D smooths is a marginal of the tensor product. If
  users do not provide covariate values at which to evaluate the smooths,
  `smooth_estimates()` will focus on the 2D marginal smooth (or the first if
  more than one is involved in the tensor product), instead of following the
  ordering of the terms in the definition of the tensor product. #191

  For example, in `te(z, x, y, bs = c(cr, ds), d = c(1, 2))`, the second
  marginal smooth is a 2D Duchon spline of covariates `x` and `y`. Previously,
  `smooth_estimates()` would have generated `n` values each for `z` and `x` and
  `n_3d` values for `y`, and then evaluated the tensor product at all
  combinations of those generated values. This would ignore the structure
  implicit in the tensor product, where we are likely to want to know how the
  surface estimated by the Duchon spline of `x` and `y` smoothly varies with
  `z`. Previously `smooth_estimates()` would generate surfaces of `z` and `x`,
  varying by `y`. Now, `smooth_estimates()` correctly identifies that one of the
  marginal smooths of the tensor product is a 2D surface and will focus on that
  surface varying with the other terms in the tensor product.

  This improved behaviour is needed because in some `bam()` models it is not
  always possible to do the obvious thing and reorder the smooths when defining
  the tensor product to be `te(x, y, z, bs = c(ds, cr), d = c(2, 1))`. When
  `discrete = TRUE` is used with `bam()` the terms in the tensor product may
  get rearranged during model setup for maximum efficiency (See *Details* in
  `?mgcv::bam`).

  Additionally, `draw.gam()` now also works the same way.

* New function `null_deviance()` that extracts the null deviance of a fitted
  model.

* `draw()`, `smooth_estimates()`, `fitted_values()`, `data_slice()`, and
  `smooth_samples()` now all work for models fitted with `scam::scam()`.
  Where it matters, current support extends only to univariate smooths.

* `generate_draws()` is a new low-level function for generating posterior draws
  from fitted model coefficients. `generate_daws()` is an S3 generic function so
  is extensible by users. Currently provides a simple interface to a simple
  Gaussian approximation sampler (`gaussian_draws()`) and the simple Metropolis
  Hastings sample (`mh_draws()`) available via `mgcv::gam.mh()`. #211

* `smooth_label()` is a new function for extracting the labels 'mgcv' creates for
  smooths from the smooth object itself.

* `penalty()` has a default method that  works with `s()`, `te()`, `t2()`, and
  `ti()`, which create a smooth specification.

* `transform_fun()` gains argument `constant` to allow for the addition of a
  constant value to objects (e.g. the estimate and confidence interval). This
  enables a single `obj |> transform_fun(fun = exp, constant = 5)` instead of
  separate calls to `add_constant()` and then `transform_fun()`. Part of the
  discussion of #79

* `model_constant()` is a new function that simply extracts the first
  coefficient from the estimated model.

## Bug fixes

* `link()`, `inv_link()`, and related family functions for the `ocat()` weren't
  correctly identifying the family name and as a result would throw an error
  even when passed an object of the correct family.

  `link()` and `inv_link()` now work correctly for the `betar()` family in a
  fitted GAM.

* The `print()` method for `lp_matrix()` now converts the matrix to a data frame
  before conversion to a tibble. This makes more sense as it results in more
  typical behaviour as the columns of the printed object are doubles.

* Constrained factor smooths (`bs = "sz"`) where the factor is not the first
  variable mentioned in the smooth (i.e. `s(x, f, bs = "sz")` for continuous
  `x` and factor `f`) are now plottable with `draw()`. #208

* `parametric_effects()` was unable to handle special parametric terms like
  `poly(x)` or `log(x)` in formulas. Reported by @fhui28 #212

* `parametric_effects()` now works better for location, scale, shape models.
  Reported by @pboesu #45

* `parametric_effects` now works when there are missing values in one or more
  variables used in a fitted GAM. #219

* `response_derivatives()` was incorrectly using `.data` with *tidyselect*
  selectors.

* `typical_values()` could not handle logical variables in a GAM fit as mgcv
  stores these as numerics in the `var.summary`. This affected `evenly()` and
  `data_slice()`. #222

* `parametric_effects()` would fail when two or more ordered factors were in
  the model. Reported by @dsmi31 #221

* Continuous by smooths were being evaluated with the median value of the `by`
  variable instead of a value of 1. #224

* `fitted_samples()` (and hence `posterior_samples()`) now handles models with
  offset terms in the formula. Offset terms supplied via the `offset` argument
  are ignored by `mgcv:::predict.gam()` and hence are ignored also by `gratia`.
  Reported by @jonathonmellor #231 #233

* `smooth_estimates()` would fail on a `"fs"` smooth when a multivariate base
  smoother was used *and* the factor was not the last variable specified in the
  definition of the smooth: `s(x1, x2, f, bs = "fs", xt = list(bs = "ds"))`
  would work, but `s(f, x1, x2, bs = "fs", xt = list(bs = "ds"))` (or any
  ordering of variables that places the factor not last) would emit an obscure
  error. The ordering of the terms involved in the smooth now doesn't matter.
  Reported by @chrisaak #249.

* `draw.gam()` would fail when plotting a multivariate base smoother used in an
  `"sz"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn't know how to plot such a smooth.
  Reported by @chrisaak #249.

* `draw.gam()` would fail when plotting a multivariate base smoother used in an
  `"fs"` smooth. Now, this use case is identified and a message printed
  indicating that (currently) gratia doesn't know how to plot such a smooth.
  Reported by @chrisaak #249.

* `derivative_samples()` would fail with `order = 2` and was only computing
  forward finite differences, regardless of `type` for `order = 1`. Partly
  reported by @samlipworth #251.

* The `draw()` method for `penalty()` was normalizing the penalty to the range
  0--1, not the claimed and documented -1--1 with argument `normalize = TRUE`.
  This is now fixed.

* `smooth_samples()` was failing when `data` was supplied that contained more
  variables than were used in the smooth that was being sampled. Hence this
  generally fail unless a single smooth was being sampled from or the model
  contained only a single smooth. The function never intended to retain all the
  variables in `data` but was written in such a way that it would fail when
  relocating the data columns to the end of the posterior sampling object. #255

* `draw.gam()` and `draw.smooth_estimates()` would fail when plotting a
  univariate tensor product smooth (e.g. `te(x)`, `ti(x)`, or `t2()`). Reported
  by @wStockhausen #260

* `plot.smooth()` was not printing the factor level in subtitles for ordered 
  factor by smooths.

# gratia 0.8.2

* Small fixes for CRAN.

# gratia 0.8.1

## User visible changes

* `smooth_samples()` now returns objects with variables involved in smooths
  that have their correct name. Previously variables were named `.x1`, `.x2`,
  etc. Fixing #126 and improving compatibility with `compare_smooths()` and
  `smooth_estimates()` allowed the variables to be named correctly.

* *gratia* now depends on version 1.8-41 or later of the *mgcv* package.

## New features

* `draw.gam()` can now handle tensor products that include a marginal random
  effect smooth. Beware plotting such smooths if there are many levels,
  however, as a separate surface plot will be produced for each level.

## Bug fixes

* Additional fixes for changes in dplyr 1.1.0.

* `smooth_samples()` now works when sampling from posteriors of multiple smooths
  with different dimension. #126 reported by @Aariq

# gratia 0.8.0

## User visible changes

* {gratia} now depends on R version 4.1 or later.

* A new vignette "Data slices" is supplied with {gratia}.

* Functions in {gratia} have harmonised to use an argument named `data` instead
  of `newdata` for passing new data at which to evaluate features of smooths. A
  message will be printed if `newdata` is used from now on. Existing code does
  not need to be changed as `data` takes its value from `newdata`.
  
  Note that due to the way `...` is handled in R, if your R script uses the
  `data` argument, *and* is run with versions of gratia prior to 8.0 (when
  released; 0.7.3.8 if using the development version) the user-supplied data
  will be silently ignored. As such, scripts using `data` should check that the
  installed version of gratia is >= 0.8 and package developers should update
  to depend on versions >= 0.8 by using `gratia (>= 0.8)` in `DESCRIPTION`.

* The order of the plots of smooths has changed in `draw.gam()` so that they
  again match the order in which smooths were specified in the model formula.
  See *Bug Fixes* below for more detail or #154.

## New features

* Added basic support for GAMLSS (distributional GAMs) fitted with the
  `gamlss()` function from package GJRM. Support is currently restricted to a
  `draw()` method.

* `difference_smooths()` can now include the group means in the difference,
  which many users expected. To include the group means use `group_means = TRUE`
  in the function call, e.g.
  `difference_smooths(model, smooth = "s(x)", group_means = TRUE`). Note: this
  function still differs from `plot_diff()` in package *itsadug*, which
  essentially computes differences of model predictions. The main practical
  difference is that other effects beyond the factor by smooth, including random
  effects, may be included with `plot_diff()`.

  This implements the main wish of #108 (@dinga92) and #143 (@mbolyanatz)
  despite my protestations that this was complicated in some cases (it isn't;
  the complexity just cancels out.)

* `data_slice()` has been totally revised. Now, the user provides the values for
  the variables they want in the slice and any variables in the model that are
  not specified will be held at typical values (i.e. the value of the
  observation that is closest to the median for numeric variables, or the modal
  factor level.)

  Data slices are now produced by passing `name` = `value` pairs for the
  variables and their values that you want to appear in the slice. For example

  ```
  m <- gam(y ~ s(x1) + x2 + fac)
  data_slice(model, x1 = evenly(x1, n = 100), x2 = mean(x2))
  ```

  The `value` in the pair can be an expression that will be looked up
  (evaluated) in the `data` argument or the model frame of the fitted model
  (the default). In the above example, the resulting slice will be a data frame
  of 100 observations, comprising `x1`, which is a vector of 100 values spread
  evenly over the range of `x1`, a constant value of the mean of `x2` for the
  `x2` variable, and a constant factor level, the model class of `fac`, for the
  `fac` variable of the model.

* `partial_derivatives()` is a new function for computing partial derivatives
  of multivariate smooths (e.g. `s(x,z)`, `te(x,z)`) with respect to one of
  the margins of the smooth. Multivariate smooths of any dimension are handled,
  but only one of the dimensions is allowed to vary. Partial derivatives are
  estimated using the method of finite differences, with forward, backward,
  and central finite differences available. Requested by @noamross #101

* `overview()` provides a simple overview of model terms for fitted GAMs.

* The new `bs = "sz"` basis that was released with *mgcv* version 1.18-41 is
  now supported in `smooth_estimates()`, `draw.gam()`, and
  `draw.smooth_estimates()` and this basis has its own unique plotting method.
  #202

* `basis()` now has a method for fitted GAM(M)s which can extract the estimated
  basis from the model and plot it, using the estimated coefficients for the
  smooth to weight the basis. #137

  There is also a new `draw.basis()` method for plotting the results of a call
  to `basis()`. This method can now also handle bivariate bases.

  `tidy_basis()` is a lower level function that does the heavy lifting in
  `basis()`, and is now exported. `tidy_basis()` returns a tidy representation
  of a basis supplied as an object inheriting from class `"mgcv.smooth"`. These
  objects are returned in the `$smooth` component of a fitted GAM(M) model.

* `lp_matrix()` is a new utility function to quickly return the linear predictor
  matrix for an estimated model. It is a wrapper to
  `predict(..., type = "lpmatrix")`

* `evenly()` is a synonym for `seq_min_max()` and is preferred going forward.
  Gains argument `by` to produce sequences over a covariate that increment in
  units of `by`.

* `ref_level()` and `level()` are new utility functions for extracting the
  reference or a specific level of a factor respectively. These will be most
  useful when specifying covariate values to condition on in a data slice.

* `model_vars()` is a new, public facing way of returning a vector of variables
  that are used in a model.

* `difference_smooths()` will now use the user-supplied data as points at
  which to evaluate a pair of smooths. Also note that the argument `newdata` has
  been renamed `data`. #175

* The `draw()` method for `difference_smooths()` now uses better labels for
  plot titles to avoid long labels with even modest factor levels.

* `derivatives()` now works for factor-smooth interaction (`"fs"`) smooths.

* `draw()` methods now allow the angle of tick labels on the x axis of plots to
  be rotated using argument `angle`. Requested by @tamas-ferenci #87

* `draw.gam()` and related functions (`draw.parametric_effects()`,
  `draw.smooth_estimates()`) now add the basis to the plot using a caption.
  #155

* `smooth_coefs()` is a new utility function for extracting the coefficients
  for a particular smooth from a fitted model. `smooth_coef_indices()` is an
  associated function that returns the indices (positions) in the vector of
  model coefficients (returned by `coef(gam_model)`) of those coefficients that
  pertain to the stated smooth.

* `draw.gam()` now better handles patchworks of plots where one or more of
  those plots has fixed aspect ratios. #190

## Bug fixes

* `draw.posterior_smooths` now plots posterior samples with a fixed aspect ratio
  if the smooth is isotropic. #148

* `derivatives()` now ignores random effect smooths (for which derivatives
  don't make sense anyway). #168

* `confint.gam(...., method = "simultaneous")` now works with factor by smooths
  where `parm` is passed the full name of a specific smooth `s(x)faclevel`.

* The order of plots produced by `gratia::draw.gam()` again matches the order
  in which the smooths entered the model formula. Recent changes to the
  internals of `gratia::draw.gam()` when the switch to `smooth_estimates()` was
  undertaken lead to a change in behaviour resulting from the use of
  `dplyr::group_split()`, and it's coercion internally of a character vector to
  a factor. This factor is now created explicitly, and the levels set to the
  correct order. #154

* Setting the `dist` argument to set response or smooth values to `NA` if they
  lay too far from the support of the data in multivariate smooths, this would
  lead an incorrect scale for the response guide. This is now fixed. #193

* Argument `fun` to `draw.gam()` was not being applied to any parametric terms.
  Reported by @grasshoppermouse #195

* `draw.gam()` was adding the uncertainty for all linear predictors to smooths
  when `overall_uncertainty = TRUE` was used. Now `draw.gam()` only includes the
  uncertainty for those linear predictors in which a smooth takes part. #158

* `partial_derivatives()` works when provided with a single data point at
  which to evaluate the derivative. #199

* `transform_fun.smooth_estimates()` was addressing the wrong variable names
  when trying to transform the confidence interval. #201

* `data_slice()` doesn't fail with an error when used with a model that contains
  an offset term. #198

* `confint.gam()` no longer uses `evaluate_smooth()`, which is soft deprecated.
  #167

* `qq_plot()` and `worm_plot()` could compute the wrong deviance residuals used
  to generate the theoretical quantiles for some of the more exotic families
  (distributions) available in *mgcv*. This also affected `appraise()` but only
  for the QQ plot; the residuals shown in the other plots and the deviance
  residuals shown on the y-axis of the QQ plot were correct. Only the
  generation of the reference intervals/quantiles was affected.

# gratia 0.7.3

## User visible changes

* Plots of smooths now use "Partial effect" for the y-axis label in place of
  "Effect", to better indicate what is displayed.

## New features

* `confint.fderiv()` and `confint.gam()` now return their results as a tibble
  instead of a common-or-garden data frame. The latter mostly already did this.

* Examples for `confint.fderiv()` and `confint.gam()` were reworked, in part to
  remove some inconsistent output in the examples when run on M1 macs.

## Bug fixes

* `compare_smooths()` failed when passed non-standard model "names" like
  `compare_smooths(m_gam, m_gamm$gam)` or `compare_smooths(l[[1]], l[[2]])`
  even if the evaluated objects were valid GAM(M) models. Reported by Andrew
  Irwin #150

# gratia 0.7.2

## New features

* `draw.gam()` and `draw.smooth_estimates()` can now handle splines on the
  sphere (`s(lat, long, bs = "sos")`) with special plotting methods using
  `ggplot2::coord_map()` to handle the projection to spherical coordinates. An
  orthographic projection is used by default, with an essentially arbitrary
  (and northern hemisphere-centric) default for the orientation of the view.

* `fitted_values()` insures that `data` (and hence the returned object) is a
  tibble rather than a common or garden data frame.

## Bug fixes

* `draw.posterior_smooths()` was redundantly plotting duplicate data in the rug
  plot. Now only the unique set of covariate values are used for drawing the
  rug.

* `data_sim()` was not passing the `scale` argument in the bivariate example
  setting (`"eg2"`).

* `draw()` methods for `gamm()` and `gamm4::gamm4()` fits were not passing 
  arguments on to `draw.gam()`.

* `draw.smooth_estimates()` would produce a subtitle with data for a continuous
  by smooth as if it were a factor by smooth. Now the subtitle only contains the
  name of the continuous by variable.

# gratia 0.7.1

Due to an issue with the size of the package source tarball, which wasn't
discovered until after submission to CRAN, 0.7.1 was never released.

## New features

* `draw.gam()` and `draw.smooth_estimates()`: {gratia} can now handle smooths
  of 3 or 4 covariates when plotting. For smooths of 3 covariates, the third
  covariate is handled with `ggplot2::facet_wrap()` and a set (default `n` = 16)
  of small multiples is drawn, each a 2d surface evaluated at the specified
  value of the third covariate. For smooths of 4 covariates,
  `ggplot2::facet_grid()` is used to draw the small multiples, with the default
  producing 4 rows by 4 columns of plots at the specific values of the third
  and fourth covariates. The number of small multiples produced is controlled
  by new arguments `n_3d` (default = `n_3d = 16`) and `n_4d` (default
  `n_4d = 4`, yielding `n_4d * n_4d` = 16 facets) respectively.

  This only affects plotting; `smooth_estimates()` has been able to handle
  smooths of any number of covariates for a while.

  When handling higher-dimensional smooths, actually drawing the plots on the
  default device can be slow, especially with the default value of `n = 100`
  (which for 3D or 4D smooths would result in 160,000 data points being
  plotted). As such it is recommended that you reduce `n` to a smaller value:
  `n = 50` is a reasonable compromise of resolution and speed.

* `model_concurvity()` returns concurvity measures from `mgcv::concurvity()`
  for estimated GAMs in a tidy format. The synonym `concrvity()` is also
  provided. A `draw()` method is provided which produces a bar plot or a heatmap
  of the concurvity values depending on whether the overall concurvity of each
  smooth or the pairwise concurvity of each smooth in the model is requested.

* `draw.gam()` gains argument `resid_col = "steelblue3"` that allows the colour
  of the partial residuals (if plotted) to be changed.

## Bug fixes

* `model_edf()` was not using the `type` argument. As a result it only ever
   returned the default EDF type.

* `add_constant()` methods weren't applying the constant to all the required
  variables.

* `draw.gam()`, `draw.parametric_effects()` now actually work for a model with
  only parametric effects. #142 Reported by @Nelson-Gon

* `parametric_effects()` would fail for a model with only parametric terms
  because `predict.gam()` returns empty arrays when passed 
  `exclude = character(0)`.

# gratia 0.7.0

## Major changes

* `draw.gam()` now uses `smooth_estimates()` internally and consequently uses
  its `draw()` method and underlying plotting code. This has simplified the code
  compared to `evaluate_smooth()` and its methods, which will allow for future
  development and addition of features more easily than if `evaluate_smooth()`
  had been retained.

  Similarly, `evaluate_parametric_terms()` is now deprecated in favour of
  `parametric_effects()`, which is also used internally by `draw.gam()` if
  parametric terms are present in the model (and `parametric = TRUE`).

  While a lot of code has been reused so differences between plots as a result
  of this change should be minimal, some corner cases may have been missed. File
  an Issue if you notice something that has changed that you think shouldn't.

* `draw.gam()` now plots 2D isotropic smooths (TPRS and Duchon splines) with
  equally-scaled x and y coordinates using `coord_equal(ratio = 1)`. Alignment
  of these plots will be a little different now when plotting models with
  multiple smooths. See Issue #81.

### Deprecated functions

From version 0.7.0, the following functions are considered deprecated and their
use is discouraged:

* `fderiv()` is *soft*-deprecated in favour of `derivatives()`,
* `evaluate_smooth()` is *soft*-deprecated in favour of `smooth_estimates()`,
* `evaluate_parametric_term()` is *soft*-deprecated in favour of
  `parametric_effects()`.

The first call to one of these functions will generate a warning, pointing to
the newer, alternative, function. It is safe to ignore these warnings, but
these deprecated functions will no longer receive updates and are thus at risk
of being removed from the package at some future date. The newer alternatives
can handle more types of models and smooths, especially so in the case of
`smooth_estimates()`.

## New features

* `fitted_values()` provides a tidy wrapper around `predict.gam()` for
  generating fitted values from the model. New covariate values can be provided
  via argument `data`. A credible interval on the fitted values is returned, and
  values can be on the link (linear predictor) or response scale.

  Note that this function returns expected values of the response. Hence,
  "fitted values" is used instead of "predictions" in the case of new covariate
  values to differentiate these values from the case of generating new response
  values from a fitted model.

* `rootogram()` and its `draw()` method produce rootograms as diagnostic plots
  for fitted models. Currently only for models fitted with `poisson()`,
  `nb()`, `negbin()`, `gaussian()` families.

* New helper functions `typical_values()`, `factor_combos()` and
  `data_combos()` for quickly creating data sets for producing predictions from
  fitted models where some covariates are fixed at come typical or
  representative values.

    `typical_values()` is a new helper function to return typical values for the
    covariates of a fitted model. It returns the value of the observation
    closest to the median for numerical covariates or the modal level of a
    factor while preserving the levels of that factor. `typical_values()` is
    useful in preparing data slices or scenarios for which fitted values from
    the estimated model are required.
    
    `factor_combos()` extracts and returns the combinations of levels of factors
    found in data used to fit a model. Unlike `typical_values()`,
    `factor_combos()` returns all the combinations of factor levels observed
    in the data, not just the modal level. Optionally, all combinations of
    factor levels can be returned, not just those in the observed data.
    
    `data_combos()` combines returns the factor data from `factor_combos()` plus
    the typical values of numerical covariates. This is useful if you want to
    generate predictions from the model for each combination of factor terms
    while holding any continuous covariates at their median values.

* `nb_theta()` is a new extractor function that returns the theta parameter of
  a fitted negative binomial GAM (families `nb()` or `negbin()`). Additionally,
  `theta()` and `has_theta()` provide additional functionality. `theta()` is an
  experimental function for extracting any additional parameters from the model
  or family. `has_theta()` is useful for checking if any additional parameters
  are available from the family or model. 

* `edf()` extracts the effective degrees of freedom (EDF) of a fitted model or a
  specific smooth in the model. Various forms for the EDF can be extracted.

* `model_edf()` returns the EDF of the overall model. If supplied with multiple
  models, the EDFs of each model are returned for comparison.

* `draw.gam()` can now show a "rug" plot on a bivariate smooth by drawing small
  points with high transparency over the smooth surface at the data coordinates.

  In addition, the rugs on plots of factor by smooths now show the locations of
  covariate values for the specific level of the factor and not over all levels.
  This better reflects what data were used to estimate the smooth, even though
  the basis for each smooth was set up using all of the covariate locations.

* `draw.gam()` and `draw.smooth_estimates()` now allow some aspects of the plot
  to be changed: the fill (but not colour) and alpha attributes of the credible
  interval, and the line colour for the smooth can now be specified using
  arguments `ci_col`, `ci_alpha`, and `smooth_col` respectively.

* Partial residuals can now be plotted on factor by smooths. To allow this, the
  partial residuals are filtered so that only residuals associated with a
  particular level's smooth are drawn on the plot of the smooth.

* `smooth_estimates()` uses `check_user_select_smooths()` to handle
  user-specified selection of smooth terms. As such it is more flexible than
  previously, and allows for easier selection of smooths to evaluate.

* `fixef()` is now imported (and re-exported) from the *nlme* package, with
  methods for models fitted with `gam()` and `gamm()`, to extract fixed effects
  estimates from fitted models. `fixed_effects()` is an alias for `fixef()`.

* The `draw()` method for `smooth_samples()` can now handle 2D smooths.
  Additionally, the number of posterior draws to plot can now be specified when
  plotting using new argument `n_samples`, which will result in `n_samples`
  draws being selected at random from the set of draws for plotting. New
  argument `seed` allows the selection of draws to be repeatable.

## Bug fixes

* `smooth_estimates()` was not filtering user-supplied data for the by level of
  the specific smooth when used with by factor smooths. This would result in the
  smooth being evaluated at all rows of the user-supplied data, and therefore
  would result in `nrow(user_data) * nlevels(by_variable)` rows in the returned
  object instead of `nrow(user_data)` rows.

* The `add_confint()` method for `smooth_estimates()` had the upper and lower
  intervals reversed. #107 Reported by @Aariq

* `draw.gam()` and `smooth_estimates()` were both ignoring the `dist` argument
  that allows covariate values that lie too far from the support of the data to
  be excluded when returning estimated values from the smooth and plotting it.
  #111 Reported by @Aariq

* `smooth_samples()` with a factor by GAM would return samples for the first
  factor level only. Reported by @rroyaute in discussion of #121

* `smooth_samples()` would fail if the model contained random effect "smooths".
  These are now ignored with a message when running `smooth_samples()`.
  Reported by @isabellaghement in #121

* `link()`, `inv_link()` were failing on models fitted with `family = scat()`.
  Reported by @Aariq #130

# gratia 0.6.0

## Major changes

* The {cowplot} package has been replaced by the {patchwork} package for
  producing multi-panel figures in `draw()` and `appraise()`. This shouldn't
  affect any code that used {gratia} only, but if you passed additional
  arguments to `cowplot::plot_grid()` or used the `align` or `axis` arguments of
  `draw()` and `appraise()`, you'll need to adapt code accordingly.

  Typically, you can simply delete the `align` or `axis` arguments and
  {patchwork} will just work and align plots nicely. Any arguments passed via
  `...` to `cowplot::plot_grid()` will just be ignored by
  `patchwork::wrap_plots()` unless those passed arguments match any of the
  arguments of `patchwork::wrap_plots()`.

## New features

* The {patchwork} package is now used for multi-panel figures. As such, {gratia}
  no longer Imports from the {cowplot} package.

* Worm plot diagnostic plots are available via new function `worm_plot()`. Worm
  plots are detrended Q-Q plots, where deviation from the Q-Q reference line are
  emphasized as deviations around the line occupy the full height of the plot.

  `worm_plot()` methods are available for models of classes `"gam"`, `"glm"`,
  and `"lm"`. (#62)

* Smooths can now be compared across models using `compare_smooths()`, and
  comparisons visualised with the associated `draw()` method. (#85 @dill)

  This feature is a bit experimental; the returned object uses nested lists and
  may change in the future if users find this confusing.

* The reference line in `qq_plot()` with `method = "normal"` was previously
  drawn as a line with intercept 0 and slope 1, to match the other methods. This
  was inconsistent with `stats::qqplot()` which drew the line through the 1st
  and 3rd quartiles. `qq_plot()` with `method = "normal"` now uses this robust
  reference line. Reference lines for the other methods remain drawn with slope
  1 and intercept 0.

* `qq_plot()` with `method = "normal"` now draws a point-wise reference band
  using the standard error of the order statistic.

* The `draw()` method for `penalty()` now plots the penalty matrix heatmaps in a
  more-logical orientation, to match how the matrices might be written down or
  printed to the R console.

* `link()`, and `inv_link()` now work for models fitted with the `gumbls()` and
  `shash()` families. (#84)

* `extract_link()` is a lower level utility function related to `link()` and
  `inv_link()`, and is now exported.

## User visible changes

* The default method name for generating reference quantiles in `qq_plot()` was
  changed from `"direct"` to `"uniform"`, to avoid confusion with the
  `mgcv::qq.gam()` help page description of the methods. Accordingly using
  `method = "direct"` is deprecated and a message to this effect is displayed if
  used.

* The way smooths/terms are selected in `derivatives()` has been switched to use
  the same mechanism as `draw.gam()`'s `select` argument. To get a partial match
  to `term`, you now need to also specify `partial_match = TRUE` in the call to
  `derivatives()`.

## Bug fixes

* `transform_fun()` had a copy paste bug in the definition of the then generic.
  (#96 @Aariq)

* `derivatives()` with user-supplied `newdata` would fail for factor by smooths
  with `interval = "simultaneous"` and would introduce rows with derivative == 0
  with `interval = "confidence"` because it didn't subset the rows of `newdata`
  for the specific level of the by factor when computing derivatives.
  (#102 @sambweber)

* `evaluate_smooth()` can now handle random effect smooths defined using an
  ordered factor. (#99 @StefanoMezzini)

# gratia 0.5.1

## New features

* `smooth_estimates()` can now handle

    * bivariate and multivariate thinplate regression spline smooths, e.g. 
      `s(x, z, a)`,
    * tensor product smooths (`te()`, `t2()`, & `ti()`), e.g. `te(x, z, a)`
    * factor smooth interactions, e.g. `s(x, f, bs = "fs")`
    * random effect smooths, e.g. `s(f, bs = "re")`

* `penalty()` provides a tidy representation of the penalty matrices of
  smooths. The tidy representation is most suitable for plotting with
  `ggplot()`.

  A `draw()` method is provided, which represents the penalty matrix as a
  heatmap.

## User visible changes

* The `newdata` argument to `smooth_estimates()` has been changed to `data` as
  was originally intended.

# gratia 0.5.0

## New features

* Partial residuals for models can be computed with `partial_residuals()`. The
  partial residuals are the weighted residuals of the model added to the
  contribution of each smooth term (as returned by `predict(model,
  type = "terms")`.
  
  Wish of [#76](https://github.com/gavinsimpson/gratia/issues/76) (@noamross)

  Also, new function `add_partial_residuals()` can be used to add the partial
  residuals to data frames.

* Users can now control to some extent what colour or fill scales are used when
  plotting smooths in those `draw()` methods that use them. This is most useful
  to change the fill scale when plotting 2D smooths, or to change the discrete
  colour scale used when plotting random factor smooths (`bs = "fs"`).
  
  The user can pass scales via arguments `discrete_colour` and
  `continuous_fill`.

* The effects of certain smooths can be excluded from data simulated from a
  model using `simulate.gam()` and `predicted_samples()` by passing `exclude` or
  `terms` on to `predict.gam()`. This allows for excluding random effects, for
  example, from model predicted values that are then used to simulate new data
  from the conditional distribution. See the example in `predicted_samples()`.
  
  Wish of [#74](https://github.com/gavinsimpson/gratia/issues/74) (@hgoldspiel)

* `draw.gam()` and related functions gain arguments `constant` and `fun` to
  allow for user-defined constants and transformations of smooth estimates and
  confidence intervals to be applied.
  
  Part of wish of Wish of
  [#79](https://github.com/gavinsimpson/gratia/issues/79).

* `confint.gam()` now works for 2D smooths also.

* `smooth_estimates()` is an early version of code to replace (or more likely
  supersede) `evaluate_smooth()`. `smooth_estimates()` can currently only handle
  1D smooths of the standard types. 

## User visible changes

* The meaning of `parm` in `confint.gam` has changed. This argument now requires
  a smooth label to match a smooth. A vector of labels can be provided, but
  partial matching against a smooth label only works with a single `parm` value.
  
  The default behaviour remains unchanged however; if `parm` is `NULL` then all
  smooths are evaluated and returned with confidence intervals.

* `data_class()` is no longer exported; it was only ever intended to be an
  internal function.

## Bug Fixes

* `confint.gam()` was failing on a tensor product smooth due to matching issues.
  Reported by @tamas-ferenci
  [#88](https://github.com/gavinsimpson/gratia/issues/88)
  
  This also fixes [#80](https://github.com/gavinsimpson/gratia/issues/80)
  (@butellyn) which was a related issue with selecting a specific smooth.

* The **vdiffr** package is now used conditionally in package tests.
  Reported by Brian Ripley
  [#93](https://github.com/gavinsimpson/gratia/issues/93)

# gratia 0.4.1

## User visible changes

* `draw.gam()` with `scales = "fixed"` now applies to all terms that can be
  plotted, including 2d smooths.

  Reported by @StefanoMezzini
  [#73](https://github.com/gavinsimpson/gratia/issues/73)

## Bug fixes

* `dplyr::combine()` was deprecated. Switch to `vctrs::vec_c()`.

* `draw.gam()` with `scales = "fixed"` wasn't using fixed scales where 2d
  smooths were in the model.

  Reported by @StefanoMezzini
  [#73](https://github.com/gavinsimpson/gratia/issues/73)

# gratia 0.4.0

## New features

* `draw.gam()` can include partial residuals when drawing univariate smooths.
  Use `residuals = TRUE` to add partial residuals to each univariate smooth that
  is drawn. This feature is not available for smooths of more than one variable,
  by smooths, or factor-smooth interactions (`bs = "fs"`).

* The coverage of credible and confidence intervals drawn by `draw.gam()` can be
  specified via argument `ci_level`. The default is arbitrarily `0.95` for no
  other reason than (rough) compatibility with `plot.gam()`.
  
  This change has had the effect of making the intervals slightly narrower than
  in previous versions of *gratia*; intervals were drawn at &plusmn; 2 &times;
  the standard error. The default intervals are now drawn at &plusmn; ~1.96
  &times; the standard error.

* New function `difference_smooths()` for computing differences between factor
  smooth interactions. Methods available for `gam()`, `bam()`, `gamm()` and
  `gamm4::gamm4()`. Also has a `draw()` method, which can handle differences of
  1D and 2D smooths currently (handling 3D and 4D smooths is planned).

* New functions `add_fitted()` and `add_residuals()` to add fitted values
  (expectations) and model residuals to an existing data frame. Currently
  methods available for objects fitted by `gam()` and `bam()`.

* `data_sim()` is a tidy reimplementation of `mgcv::gamSim()` with the added
  ability to use sampling distributions other than the Gaussian for all models
  implemented. Currently Gaussian, Poisson, and Bernoulli sampling distributions
  are available.

* `smooth_samples()` can handle continuous by variable smooths such as in
  varying coefficient models.

* `link()` and `inv_link()` now work for all families available in *mgcv*,
  including the location, scale, shape families, and the more specialised
  families described in `?mgcv::family.mgcv`.

* `evaluate_smooth()`, `data_slice()`, `family()`, `link()`, `inv_link()`
  methods for models fitted using `gamm4()` from the **gamm4** package.

* `data_slice()` can generate data for a 1-d slice (a single variable varying).

* The colour of the points, reference lines, and simulation band in `appraise()`
  can now be specified via arguments

    * `point_col`,
    * `point_alpha`,
    * `ci_col`
    * `ci_alpha`
    * `line_col`

  These are passed on to `qq_plot()`, `observed_fitted_plot()`,
  `residuals_linpred_plot()`, and `residuals_hist_plot()`, which also now take
  the new arguments were applicable.

* Added utility functions `is_factor_term()` and `term_variables()` for working
  with models. `is_factor_term()` identifies is the named term is a factor using
  information from the `terms()` object of the fitted model. `term_variables()`
  returns a character vector of variable names that are involved in a model
  term. These are strictly for working with parametric terms in models.

* `appraise()` now works for models fitted by `glm()` and `lm()`, as do the
  underlying functions it calls, especially `qq_plot`.
  
  `appraise()` also works for models fitted with family `gaulss()`. Further
  location scale models and models fitted with extended family functions will
  be supported in upcoming releases.

## User visible changes

* `datagen()` is now an *internal* function and is no longer exported. Use
  `data_slice()` instead.

* `evaluate_parametric_term()` is now much stricter and can only evaluate main
  effect terms, i.e. those whose order, as stored in the `terms` object of the
  model is `1`.

## Bug fixes

* The `draw()` method for `derivatives()` was not getting the x-axis label for
  factor by smooths correctly, and instead was using `NA` for the second and
  subsequent levels of the factor.

* The `datagen()` method for class `"gam"` couldn't possibly have worked for
  anything but the simplest models and would fail even with simple factor by
  smooths. These issues have been fixed, but the behaviour of `datagen()` has
  changed, and the function is now not intended for use by users.

* Fixed an issue where in models terms of the form `factor1:factor2` were
  incorrectly identified as being numeric parametric terms.
  [#68](https://github.com/gavinsimpson/gratia/issues/68)

# gratia 0.3.1

## New features

* New functions `link()` and `inv_link()` to access the link function and its
  inverse from fitted models and family functions.
  
  Methods for classes: `"glm"`, `"gam"`, `"bam"`, `"gamm"` currently.
  [#58](https://github.com/gavinsimpson/gratia/issues/58)

* Adds explicit `family()` methods for objects of classes `"gam"`, `"bam"`, and
  `"gamm"`.

* `derivatives()` now handles non-numeric when creating shifted data for finite
  differences. Fixes a problem with `stringsAsFactors = FALSE` default in
  R-devel. [#64](https://github.com/gavinsimpson/gratia/issues/64)

## Bug fixes

* Updated *gratia* to work with *tibble* versions >= 3.0

# gratia 0.3.0

## New features

* *gratia* now uses the *mvnfast* package for random draws from a multivariate
  normal distribution (`mvnfast::rmvn()`). Contributed by Henrik Singmann
  (@singmann) [#28](https://github.com/gavinsimpson/gratia/issues/28)

* New function `basis()` for generating tidy representations of basis expansions
  from an *mgcv*-like definition of a smooth, e.g. `s()`, `te()`, `ti()`, or
  `t2()`. The basic smooth types also have a simple `draw()` method for plotting
  the basis. `basis()` is a simple wrapper around `mgcv::smoothCon()` with some
  post processing of the basis model matrix into a tidy format.
  [#42](https://github.com/gavinsimpson/gratia/issues/42)

* New function `smooth_samples()` to draw samples of entire smooth functions
  from their posterior distribution. Also has a `draw()` method for plotting the
  posterior samples.

## Bug fixes

* `draw.gam()` would produce empty plots between the panels for the parametric
    terms if there were 2 or more parametric terms in a model. Reported by
    @sklayn [#39](https://github.com/gavinsimpson/gratia/issues/39).

* `derivatives()` now works with factor by smooths, including ordered factor by
    smooths. The function also now works correctly for complex models with
    multiple covariates/smooths.
    [#47](https://github.com/gavinsimpson/gratia/issues/47)

    `derivatives()` also now handles `'fs'` smooths.  Reported by
    @tomand-uio [#57](https://github.com/gavinsimpson/gratia/issues/57).

* `evaluate_parametric_term()` and hence `draw.gam()` would fail on a `ziplss()`
  model because i) *gratia* didn't handle parametric terms in models with
  multiple linear predictors correctly, and ii) *gratia* didn't convert to the
  naming convention of *mgcv* for terms in higher linear predictors. Reported by
  @pboesu [#45](https://github.com/gavinsimpson/gratia/issues/45)
