# Article coverage for the release after CRAN 0.11.2

## Starting state and scope

Checked on 14 October 2026. [Official CRAN DESCRIPTION](https://cran.r-project.org/web/packages/gratia/DESCRIPTION) reports 0.11.2,
published 7 February 2026. Local release commit `79bdd1598` has that version;
NEWS entries above `# gratia 0.11.2`, history since that commit, public API
changes, tests and manuals define this audit. Starting `main` was clean at
`419930d19`, 27 commits ahead of origin/main. Diagnostics commit `f68af5a5a`,
follow-up `13d2590cb`, and version bump `3b02510fa` are ancestors of main.
DESCRIPTION is already 0.11.2-49. Work began on
`gls/release-workflow-articles`, explicitly from main. Installed gratia is
older (0.11.2.34), so validation loads this checkout.

The initial table was prepared before implementation and reconciled after
execution and independent review. The final table distinguishes executed
examples from guidance. All eleven newly exported functions appear in worked
examples or the scalable appraise panels; the family helpers are implementation
evidence, not public APIs. Internal renderer refactoring
and corrections that restore documented behaviour are covered by their relevant
workflow; they are not presented as new statistical methods.

| Feature | Public functions | Existing documentation | Article section | Executed example or explicit guide |
|---|---|---|---|---|
| Simultaneous prediction and smooth bands; credible alias; frequentist covariance | fitted_values(), smooth_estimates(), draw(), confint(), get_vcov() | fitted_values, smooth_estimates, confint.gam manuals | prediction-comparisons; glmmTMB-support: interpreting trend uncertainty | CO2 treatment curves; Nile smooth |
| Whole-model contrasts with delta or shared posterior uncertainty and joint coverage | conditional_differences(), draw() | conditional_differences manual | prediction-comparisons: treatment contrasts | CO2 chilled versus nonchilled |
| Simultaneous differences between factor smooths | difference_smooths() | difference_smooths manual | prediction-comparisons: shape versus mean differences | CO2 treatment shapes |
| Response derivatives with delta uncertainty; automatic steps; raw-coordinate derivatives | response_derivatives(), derivatives(), partial_derivatives() | derivative manuals; transformed-covariates vignette | prediction-comparisons: rate of change | CO2 concentration response |
| Group ranges and observed factor combinations | data_slice(.by), all_levels(), conditional_values(complete), fitted_values() | data-slices; data_slice/conditional_values manuals | prediction-comparisons: prediction design | CO2 support ranges and absent combinations |
| Expressions, offsets, raw/model coordinates and missing rows | smooth_estimates(), parametric_effects(), draw(back_transform), sampling functions | transformed-covariates vignette; sampling manuals | prediction-comparisons; diagnostics: response encodings | CO2/log concentration; excluded observations |
| Parametric interactions and default panels; intercept covariance | parametric_effects(), draw(), assemble() | parametric_effects/draw.gam manuals | smooth-effects: interactions and uncertainty | CO2 experiment |
| Surface resolution, tiles, 3D; penalty tiles | draw(), assemble(), smooth_estimates(), penalty() | draw.gam/smooth_estimates/draw.penalty_df manuals | smooth-effects / spatial-smooths: displaying surfaces | Airquality interaction/soap surface; optional ggcube |
| Random-effect tensor factor margins and spherical boundaries | draw(re_levels), smooth_estimates() | draw.gam manual and regression tests | spatial-smooths: domain and factor structure | Tensor factor effects; spherical simulation |
| MRF full and reduced ranks, descriptive QQ | smooth_estimates(), draw(mrf_plot) | smooth_estimates/draw.gam manuals | spatial-smooths: neighbouring areas | Columbus districts |
| Soap boundary and wiggly components sf/sw | smooth_estimates(), draw(), boundary() | draw.gam/boundary manuals | spatial-smooths: coastline constraints | Official mgcv estuary example |
| Penalised glmmTMB splines in model components; extraction, effects, native random effects | smooths(), smooth_coefs(), smooth_estimates(), parametric_effects(), draw() | glmmTMB-methods | glmmTMB-support: smoothness and fitted effects | Nile flow with estimated smoothing; meaningful group effects |
| sdmTMB formula effects, delta predictors, multiple-penalty tensors | smooths(), smooth_coefs(), smooth_estimates(), parametric_effects(), draw() | sdmTMB-methods | sdmTMB-support: habitat effects and density | Official Pacific cod survey: delta Gamma with spatial/year fields; Bernoulli depth-by-year tensor |
| Unified residuals and PIT/quantile reference calculations | quantile_residuals(), qq_plot(), worm_plot(), appraise() | diagnostic_support and plot manuals | model-checking / response-distributions: what a reference checks | Poisson GAM and binomial GLM; Orthodont lme/lmer/gamm4 |
| Holding effects fixed, fitted-effect uncertainty, new effects; direct lme/merMod and gamm/gamm4 | appraise(), simulate(), posterior_samples(), posterior_predictive_plot(), draw(), fitted_values(), variance_comp() | diagnostic_support/gamm4_support/sampling manuals | model-checking / response-distributions: repeated measurements | Orthodont growth lme/lmer/gamm4; direct lm and non-Gaussian PQL guide only |
| BAM Gaussian AR1 versus GEE working AR1 restrictions, ordered checks | residual_correlation(), residual_correlation_plot(), qq_plot() | diagnostic_support/residual_correlation | model-checking / response-distributions: serial structure | Repeated time series with Gaussian and count responses |
| sdmTMB one field draw, estimates, integrated fitted fields, new fields | appraise(), quantile_residuals(), posterior_predictive_plot() | diagnostic_support and official sdmTMB residual docs | sdmTMB-support: what field choice checks | Pacific cod maps and delta components |
| Scalable five panels, automatic threshold, smoothed densities, pooled component histograms | scalable_*_plot(), appraise() | scalable_qq_plot/appraise manuals | model-checking / response-distributions: many observations | 12000 simulated count observations |
| Posterior predictive response plots, continuous/discrete, compact summaries | posterior_predictive_plot(), appraise(ppcheck) | posterior_predictive_plot | model-checking / response-distributions; gfam; fitter articles | Counts, growth, grouped responses, cod |
| Grouped diagnostics, calibrated components and pooling | quantile_residuals(), appraise(), qq_plot(), worm_plot() | diagnostic_support, grouped tests | gfam-support: check each measurement process | Linked presence and counts |
| Grouped response-scale draws and simulation with family_index | fitted_samples(), simulate(), posterior_samples() | fitted_samples/simulate manuals | gfam-support: predictions for each data source | New surveys with component membership |
| Family CDF/quantile/random generation additions, Tweedie mixtures and default references | quantile_residuals(), qq_plot(), worm_plot(), simulate(), posterior_samples() | family helper manuals; diagnostic_support | model-checking / response-distributions: distributions and tails | Executed Tweedie zero mass, weighted Gamma/inverse Gaussian and cnorm; other family additions guide only |
| Distributional families (gaulss, gammals, gumbls, gevlss, shash, twlss, ordinal, ziP); links | quantile_residuals(), appraise(), link(), inv_link(), extract_link() | diagnostic_support/link/family manuals | model-checking / response-distributions: several distribution parameters | Executed gaulss and GJRM N mcycle; ordinal example; other distributional families explicitly guide only |
| Univariate GJRM final distribution parameters and diagnostic panels | appraise(), quantile_residuals(), draw(), posterior_predictive_plot() | diagnostic_support; GJRM tests | model-checking / response-distributions: mean and scale | mcycle location and scale |
| Censored cnorm/cpois/clog; bcg and optional mgcvUtils clognorm links | quantile_residuals(), fitted_values(), link(), inv_link() | family/link manuals; censored-family tests | model-checking / response-distributions: detection limits and support boundaries | Executed cnorm; cpois/clog/bcg/clognorm explicitly guide only, unsupported censored PPC explained |
| Weights, binomial encodings, offsets, stable tails, undefined moments and sampler streams | quantile_residuals(), simulate(), posterior_samples() | diagnostic_support and sampling manuals | model-checking / response-distributions: distribution definitions and limitations | Executed Gamma precision weights and grouped trials; weighted Gaussian, factor Bernoulli, fitting-argument offsets and extreme-tail guards guide only |

## Coverage boundaries and remaining gaps

The four main articles are model-checking, gfam-support, glmmTMB-support and
sdmTMB-support. Four companions cover prediction comparisons, smooth effects,
spatial domains and response distributions. The existing transformed-covariates,
data-slices, posterior-simulation and custom-plotting material is retained and
cross-linked. The pkgdown index groups these by workflows.

Worked examples demonstrate the new operations, rather than fitting every
permitted family, backend and latent-effect structure. These are explicit
remaining example gaps, not claims of executed coverage:

- GJRM families other than N; mgcv gammals/gumbls/gevlss/shash/twlss/ziP/scat/betar
  and multivariate diagnostics have guidance and linked references but no new
  full fitted example here. Ordered categories, Gamma, inverse Gaussian,
  Tweedie, gaulss and cnorm are executed. Link selection for twlss/mvn is
  executable. bcg and mgcvUtils clognorm have prediction/link guidance only.
- glmmTMB zero-inflation and dispersion smooths are explained through the
  shared component-selection conventions, but the substantive worked fits use
  conditional splines. Gaussian and Gamma responses and plant intercept/slopes
  are worked; other response and covariance families are support guidance.
- sdmTMB demonstrates delta-Gamma depth/year effects and a Bernoulli
  multiple-penalty tensor with spatial/year fields. RTMB, time-varying/group
  effects and Poisson-link coupled deltas are not separately fitted.
- Direct lm adapters and non-Gaussian PQL checks, factor Bernoulli encoding,
  weighted Gaussian variance, and offsets supplied as fitting arguments are
  explained but do not have separate new analyses. Existing sampling and
  regression tests remain implementation evidence.
- Higher-dimensional and two-factor tensor display rules are described; the
  new fitted tensor examples demonstrate continuous-by-factor and numeric
  surfaces. Fine-mesh sensitivity, richer delta-family comparisons and final
  high-simulation analyses are recommendations, not claimed results.

Restored behaviour is illustrated where it affects an analysis (missing rows,
transformed coordinates, fitted coefficient ordering, surface geometry and
uncertainty), with remaining small bug fixes described by NEWS and reference
documentation. They are not all given separate worked articles. In particular,
Gaussian weighted rootograms, QQ naming speed, legacy fderiv behaviour and
internal renderer refactoring are not new modelling methods.

R >= 4.4.0 and mgcv >= 1.9-4 are stated in the shared diagnostics article.
Website-only dependencies are recorded in DESCRIPTION's Config/Needs/website.
Optional fitting/display requirements are stated in the individual articles.
QQ temporary-file memory controls, marginal-rank Monte Carlo limits, ordinal
custom cutpoints, undefined response moments and explicit unsupported cases
are explained. The development version remains the verified 0.11.2-49.

See the detailed prediction, effects and spatial coverage notes and
release-validation notes for independent numerical comparisons and plot checks.
The validation summary records the final build and regression results.
