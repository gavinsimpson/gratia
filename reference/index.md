# Package index

## Plot models and effects

Visualise fitted models with
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) and
customise individual plots with
[`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md);
see [Customizing
plots](https://gavinsimpson.github.io/gratia/articles/custom-plotting.md).

- [`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) :

  Generic plotting via `ggplot2`

- [`draw(`*`<gam>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  [`draw(`*`<gamm>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  [`draw(`*`<scam>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  [`draw(`*`<scasm>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)
  : Plot smooth and parametric terms from a fitted GAM

- [`draw(`*`<gamlss>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.gamlss.md)
  [`assemble(`*`<gamlss>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.gamlss.md)
  :

  Plot smooths of a GAMLSS model estimated by
  [`GJRM::gamlss`](https://rdrr.io/pkg/GJRM/man/gamlss.html)

- [`qq_plot(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`worm_plot(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`appraise(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`parametric_terms(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`parametric_effects(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`smooths(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`get_smooths_by_id(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`smooth_estimates(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`smooth_coefs(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`assemble(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  [`draw(`*`<glmmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/glmmTMB-methods.md)
  : Effects and diagnostics for glmmTMB models

- [`smooths(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  [`get_smooths_by_id(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  [`smooth_coefs(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  [`smooth_estimates(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  [`parametric_terms(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  [`parametric_effects(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  [`assemble(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  [`draw(`*`<sdmTMB>`*`)`](https://gavinsimpson.github.io/gratia/reference/sdmTMB-methods.md)
  : Formula effects for sdmTMB models

- [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
  :

  Prepare plots via `ggplot2` and assembles them as a list

- [`assemble(`*`<gam>`*`)`](https://gavinsimpson.github.io/gratia/reference/assemble.gam.md)
  [`assemble(`*`<gamm>`*`)`](https://gavinsimpson.github.io/gratia/reference/assemble.gam.md)
  [`assemble(`*`<scam>`*`)`](https://gavinsimpson.github.io/gratia/reference/assemble.gam.md)
  : Plot smooth and parametric terms from a fitted GAM

- [`scale_fill_partial_effect()`](https://gavinsimpson.github.io/gratia/reference/scale_fill_partial_effect.md)
  : Default diverging red-blue colour palette for partial effects

- [`gratia_theme_get()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  [`gratia_theme_set()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  [`gratia_theme_update()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  : Native themes for gratia plots

- [`gratia_palette()`](https://gavinsimpson.github.io/gratia/reference/gratia_palette.md)
  : Discrete palettes for gratia plots

## Estimate smooths and parametric effects

Estimate individual model terms, add confidence intervals and partial
residuals, and transform effects for interpretation.

- [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
  : Evaluate smooths at covariate values

- [`draw(`*`<smooth_estimates>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
  :

  Plot the result of a call to
  [`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)

- [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md)
  : Estimated values for parametric model terms

- [`draw(`*`<parametric_effects>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.parametric_effects.md)
  : Plot estimated effects for model parametric terms

- [`ale()`](https://gavinsimpson.github.io/gratia/reference/ale.md) :
  Accumulated local effects of a model covariate

- [`draw(`*`<ale>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.ale.md)
  : Plot accumulated local effects

- [`add_confint()`](https://gavinsimpson.github.io/gratia/reference/add_confint.md)
  : Add a confidence interval to an existing object

- [`confint(`*`<gam>`*`)`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  [`confint(`*`<gamm>`*`)`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  [`confint(`*`<gamm4>`*`)`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  [`confint(`*`<scam>`*`)`](https://gavinsimpson.github.io/gratia/reference/confint.gam.md)
  : Point-wise and simultaneous confidence intervals for smooths

- [`simultaneous_bands`](https://gavinsimpson.github.io/gratia/reference/simultaneous_bands.md)
  : Simultaneous bands on an evaluation grid or a continuous interval

- [`shape_intervals()`](https://gavinsimpson.github.io/gratia/reference/shape_intervals.md)
  : Enforce named shapes on estimates and interval endpoints

- [`shape_confidence_set()`](https://gavinsimpson.github.io/gratia/reference/shape_confidence_set.md)
  [`in_shape_set()`](https://gavinsimpson.github.io/gratia/reference/shape_confidence_set.md)
  : Intersect a band with linear restrictions on coefficients

- [`add_constant()`](https://gavinsimpson.github.io/gratia/reference/add_constant.md)
  : Add a constant to estimated values

- [`transform_fun()`](https://gavinsimpson.github.io/gratia/reference/transform_fun.md)
  : Transform estimated values and confidence intervals by applying a
  function

- [`partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/partial_residuals.md)
  : Partial residuals

- [`add_partial_residuals()`](https://gavinsimpson.github.io/gratia/reference/add_partial_residuals.md)
  : Add partial residuals

## Predict responses

Predict from the whole model at observed or chosen covariate values,
combining model terms rather than estimating individual term effects.

- [`fitted_values()`](https://gavinsimpson.github.io/gratia/reference/fitted_values.md)
  : Generate fitted values from a estimated GAM
- [`fitted_moments()`](https://gavinsimpson.github.io/gratia/reference/fitted_moments.md)
  : Estimate moments of the conditional response distribution
- [`conditional_values()`](https://gavinsimpson.github.io/gratia/reference/conditional_values.md)
  : Conditional predictions from a GAM
- [`conditional_moments()`](https://gavinsimpson.github.io/gratia/reference/conditional_moments.md)
  : Conditional moments of a GAM response distribution
- [`draw(`*`<conditional_moments>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_moments.md)
  : Draw conditional response moments
- [`draw(`*`<conditional_values>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_values.md)
  : Plot conditional predictions
- [`add_fitted()`](https://gavinsimpson.github.io/gratia/reference/add_fitted.md)
  : Add fitted values from a model to a data frame
- [`add_fitted(`*`<gam>`*`)`](https://gavinsimpson.github.io/gratia/reference/add_fitted.gam.md)
  : Add fitted values from a GAM to a data frame

## Compare smooths and predictions

Use
[`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
for differences between factor-by smooths,
[`conditional_differences()`](https://gavinsimpson.github.io/gratia/reference/conditional_differences.md)
for contrasts of whole-model predictions, and
[`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
to compare smooth estimates across models.

- [`difference_smooths()`](https://gavinsimpson.github.io/gratia/reference/difference_smooths.md)
  : Differences of factor smooth interactions
- [`draw(`*`<difference_smooth>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.difference_smooth.md)
  : Plot differences of smooths
- [`conditional_differences()`](https://gavinsimpson.github.io/gratia/reference/conditional_differences.md)
  : Conditional differences of fitted values from a GAM
- [`draw(`*`<conditional_differences>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_differences.md)
  : Plot conditional differences
- [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
  : Compare smooths across models
- [`draw(`*`<compare_smooths>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.compare_smooths.md)
  : Plot comparisons of smooths

## Estimate derivatives and identify change

Estimate rates of change in smooths or response predictions and identify
regions of change; use
[derivative_samples()](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
for posterior draws of derivatives.

- [`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
  : Derivatives of estimated smooths via finite differences
- [`partial_derivatives()`](https://gavinsimpson.github.io/gratia/reference/partial_derivatives.md)
  : Partial derivatives of estimated multivariate smooths via finite
  differences
- [`draw(`*`<derivatives>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.derivatives.md)
  [`draw(`*`<partial_derivatives>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.derivatives.md)
  : Plot derivatives of smooths
- [`response_derivatives()`](https://gavinsimpson.github.io/gratia/reference/response_derivatives.md)
  : Derivatives on the response scale from an estimated GAM
- [`add_sizer()`](https://gavinsimpson.github.io/gratia/reference/add_sizer.md)
  : Add indicators of significant change after SiZeR

## Check model fit

Assess model adequacy with
[`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
individual diagnostic plots, residuals, and measures of concurvity.

- [`diagnostic_support`](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md)
  : Model diagnostic support and conventions
- [`family_diagnostics`](https://gavinsimpson.github.io/gratia/reference/family_diagnostics.md)
  : Extend diagnostics for an mgcv-compatible family
- [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  : Model diagnostic plots
- [`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
  : Quantile-quantile plot of model residuals
- [`worm_plot()`](https://gavinsimpson.github.io/gratia/reference/worm_plot.md)
  : Worm plot of model residuals
- [`rootogram()`](https://gavinsimpson.github.io/gratia/reference/rootogram.md)
  : Rootograms to assess goodness of model fit
- [`draw(`*`<rootogram>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.rootogram.md)
  : Draw a rootogram
- [`observed_fitted_plot()`](https://gavinsimpson.github.io/gratia/reference/observed_fitted_plot.md)
  : Plot of fitted against observed response values
- [`posterior_predictive_plot()`](https://gavinsimpson.github.io/gratia/reference/posterior_predictive_plot.md)
  : Posterior predictive check of the response distribution
- [`pp_check.gam()`](https://gavinsimpson.github.io/gratia/reference/pp_check.md)
  [`pp_check.bam()`](https://gavinsimpson.github.io/gratia/reference/pp_check.md)
  [`pp_check.gamm()`](https://gavinsimpson.github.io/gratia/reference/pp_check.md)
  [`pp_check.gamm4()`](https://gavinsimpson.github.io/gratia/reference/pp_check.md)
  [`pp_check.scam()`](https://gavinsimpson.github.io/gratia/reference/pp_check.md)
  : Posterior checks with bayesplot
- [`log_lik()`](https://gavinsimpson.github.io/gratia/reference/log_lik.md)
  : Posterior draws of pointwise log likelihoods
- [`loo.gam()`](https://gavinsimpson.github.io/gratia/reference/loo.gam.md)
  [`loo.bam()`](https://gavinsimpson.github.io/gratia/reference/loo.gam.md)
  [`waic.gam()`](https://gavinsimpson.github.io/gratia/reference/loo.gam.md)
  [`waic.bam()`](https://gavinsimpson.github.io/gratia/reference/loo.gam.md)
  [`loo.scam()`](https://gavinsimpson.github.io/gratia/reference/loo.gam.md)
  [`waic.scam()`](https://gavinsimpson.github.io/gratia/reference/loo.gam.md)
  : Leave-one-out validation and WAIC for GAMs
- [`scalable_qq_plot()`](https://gavinsimpson.github.io/gratia/reference/scalable_qq_plot.md)
  [`scalable_worm_plot()`](https://gavinsimpson.github.io/gratia/reference/scalable_qq_plot.md)
  [`scalable_residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/scalable_qq_plot.md)
  [`scalable_observed_fitted_plot()`](https://gavinsimpson.github.io/gratia/reference/scalable_qq_plot.md)
  [`scalable_residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/scalable_qq_plot.md)
  : Scalable model diagnostic plots
- [`residuals_hist_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_hist_plot.md)
  : Histogram of model residuals
- [`residuals_linpred_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_linpred_plot.md)
  : Plot of residuals versus linear predictor values
- [`residuals_covariate_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_covariate_plot.md)
  [`scalable_residuals_covariate_plot()`](https://gavinsimpson.github.io/gratia/reference/residuals_covariate_plot.md)
  : Plot residuals against covariates
- [`quantile_residuals()`](https://gavinsimpson.github.io/gratia/reference/quantile_residuals.md)
  : PIT and randomised quantile residuals
- [`surrogate_residuals()`](https://gavinsimpson.github.io/gratia/reference/surrogate_residuals.md)
  : Surrogate residuals for categorical GAMs
- [`dharma_residuals()`](https://gavinsimpson.github.io/gratia/reference/dharma_residuals.md)
  : Simulation-based PIT residuals
- [`residual_correlation()`](https://gavinsimpson.github.io/gratia/reference/residual_correlation.md)
  [`residual_correlation_plot()`](https://gavinsimpson.github.io/gratia/reference/residual_correlation.md)
  : Ordered residual correlation diagnostics
- [`add_residuals()`](https://gavinsimpson.github.io/gratia/reference/add_residuals.md)
  : Add residuals from a model to a data frame
- [`add_residuals(`*`<gam>`*`)`](https://gavinsimpson.github.io/gratia/reference/add_residuals.gam.md)
  : Add residuals from a GAM to a data frame
- [`model_concurvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md)
  [`concrvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md)
  : Concurvity of an estimated GAM
- [`draw(`*`<pairwise_concurvity>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.pairwise_concurvity.md)
  [`draw(`*`<overall_concurvity>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.pairwise_concurvity.md)
  : Plot concurvity measures

## Prepare data for prediction and evaluation

Choose covariate values at which to evaluate a model and restrict
evaluation to regions supported by the data; see [Data
slices](https://gavinsimpson.github.io/gratia/articles/data-slices.md).

- [`data_slice()`](https://gavinsimpson.github.io/gratia/reference/data_slice.md)
  : Prepare a data slice through model covariates

- [`data_combos()`](https://gavinsimpson.github.io/gratia/reference/data_combos.md)
  : All combinations of factor levels plus typical values of continuous
  variables

- [`factor_combos()`](https://gavinsimpson.github.io/gratia/reference/factor_combos.md)
  : All combinations of factor levels

- [`smooth_data()`](https://gavinsimpson.github.io/gratia/reference/smooth_data.md)
  : Generate regular data over the covariates of a smooth

- [`evenly()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  [`all_levels()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  [`seq_min_max()`](https://gavinsimpson.github.io/gratia/reference/evenly.md)
  : Create a sequence of evenly-spaced values

- [`typical_values()`](https://gavinsimpson.github.io/gratia/reference/typical_values.md)
  : Typical values of model covariates

- [`ref_level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  [`level()`](https://gavinsimpson.github.io/gratia/reference/ref_level.md)
  : Return the reference or specific level of a factor

- [`too_far()`](https://gavinsimpson.github.io/gratia/reference/too_far.md)
  : Exclude values that lie too far from the support of data

- [`too_far_to_na()`](https://gavinsimpson.github.io/gratia/reference/too_far_to_na.md)
  :

  Set rows of data to `NA` if the lie too far from a reference set of
  values

## Sample from fitted models

Draw individual smooths with
[`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
or expected responses with
[`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md);
[`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
generates new responses including coefficient uncertainty, while
[`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
simulates responses conditional on the fitted coefficients. Examine
chains with
[as_mcmc_draws()](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
and
[sampling_diagnostics()](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md).
For coefficient draws, use
[post_draws()](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
in the advanced helpers. See [Posterior
Simulation](https://gavinsimpson.github.io/gratia/articles/posterior-simulation.md)
and the [sampling migration
guide](https://gavinsimpson.github.io/gratia/articles/sampling-migration.md).

- [`posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/posterior_samples.md)
  : Draw samples from the posterior distribution of an estimated model
- [`smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/smooth_samples.md)
  : Posterior draws for individual smooths
- [`draw(`*`<smooth_samples>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_samples.md)
  : Plot posterior smooths
- [`fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/fitted_samples.md)
  : Draw fitted values from the posterior distribution
- [`predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/predicted_samples.md)
  : Draw new response values from the conditional distribution of the
  response
- [`derivative_samples()`](https://gavinsimpson.github.io/gratia/reference/derivative_samples.md)
  : Posterior expectations of derivatives from an estimated model
- [`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
  : Convert posterior samples for MCMC diagnostics
- [`sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md)
  [`print(`*`<sampling_diagnostics>`*`)`](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md)
  : Extract coefficient-sampler diagnostics
- [`draw(`*`<sampling_diagnostics>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.sampling_diagnostics.md)
  : Plot coefficient-chain diagnostics
- [`simulate(`*`<gamm4>`*`)`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  [`simulate(`*`<gamlss>`*`)`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  [`simulate(`*`<gam>`*`)`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  [`simulate(`*`<gamm>`*`)`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  [`simulate(`*`<scam>`*`)`](https://gavinsimpson.github.io/gratia/reference/simulate.md)
  : Simulate responses from a fitted model
- [`add_fitted_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md)
  [`add_predicted_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md)
  [`add_posterior_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md)
  [`add_smooth_samples()`](https://gavinsimpson.github.io/gratia/reference/add_fitted_samples.md)
  : Add posterior draws from a model to a data object

## Inspect models and smooths

Discover a fitted model’s structure and extract its parameters, terms,
smooth properties, families, and links.

### Model summaries and parameters

Summarise model complexity and extract coefficients and other
parameters.

- [`overview()`](https://gavinsimpson.github.io/gratia/reference/overview.md)
  : Provides an overview of a model and the terms in that model

- [`gamm4_support`](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
  : Support for models fitted by gamm4

- [`edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  [`model_edf()`](https://gavinsimpson.github.io/gratia/reference/edf.md)
  : Effective degrees of freedom for smooths and GAMs

- [`variance_comp()`](https://gavinsimpson.github.io/gratia/reference/variance_comp.md)
  : Variance components of smooths from smoothness estimates

- [`fixed_effects()`](https://gavinsimpson.github.io/gratia/reference/fixed_effects.gam.md)
  : Extract fixed effects estimates from a fitted model

- [`coef(`*`<scam>`*`)`](https://gavinsimpson.github.io/gratia/reference/coef.scam.md)
  :

  Extract coefficients from a fitted `scam` model.

- [`get_vcov()`](https://gavinsimpson.github.io/gratia/reference/get_vcov.md)
  : Extract the covariance matrix of the model parameters or a subset of
  these for a particular model term

- [`model_constant()`](https://gavinsimpson.github.io/gratia/reference/model_constant.md)
  **\[experimental\]** : Extract the model constant term

- [`dispersion()`](https://gavinsimpson.github.io/gratia/reference/dispersion.md)
  **\[experimental\]** : Dispersion parameter for fitted model

- [`theta()`](https://gavinsimpson.github.io/gratia/reference/theta.md)
  : General extractor for additional parameters in mgcv models

- [`nb_theta()`](https://gavinsimpson.github.io/gratia/reference/nb_theta.md)
  : Negative binomial parameter theta

- [`null_deviance()`](https://gavinsimpson.github.io/gratia/reference/null_deviance.md)
  : Extract the null deviance of a fitted model

### Terms and variables

Identify the terms and covariates used in a fitted model.

- [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md)
  : Names of smooths in a GAM
- [`n_smooths()`](https://gavinsimpson.github.io/gratia/reference/n_smooths.md)
  : How many smooths in a fitted model
- [`which_smooths()`](https://gavinsimpson.github.io/gratia/reference/which_smooths.md)
  : Identify a smooth term by its label
- [`parametric_terms()`](https://gavinsimpson.github.io/gratia/reference/parametric_terms.md)
  : Names of any parametric terms in a GAM
- [`model_terms()`](https://gavinsimpson.github.io/gratia/reference/model_terms.md)
  : Find the names of model terms
- [`model_vars()`](https://gavinsimpson.github.io/gratia/reference/model_vars.md)
  : List the variables involved in a model fitted with a formula
- [`term_names()`](https://gavinsimpson.github.io/gratia/reference/term_names.md)
  : Extract names of all variables needed to fit a GAM or a smooth
- [`term_variables()`](https://gavinsimpson.github.io/gratia/reference/term_variables.md)
  : Names of variables involved in a specified model term

### Smooth objects and properties

Extract smooth objects and inspect their structure and coefficients.

- [`get_smooth()`](https://gavinsimpson.github.io/gratia/reference/get_smooth.md)
  : Extract an mgcv smooth by name
- [`get_by_smooth()`](https://gavinsimpson.github.io/gratia/reference/get_by_smooth.md)
  : Extract an factor-by smooth by name
- [`get_smooths_by_id()`](https://gavinsimpson.github.io/gratia/reference/get_smooths_by_id.md)
  : Extract an mgcv smooth given its position in the model object
- [`basis_size()`](https://gavinsimpson.github.io/gratia/reference/basis_size.md)
  : Extract basis dimension of a smooth
- [`smooth_dim()`](https://gavinsimpson.github.io/gratia/reference/smooth_dim.md)
  : Dimension of a smooth
- [`smooth_label()`](https://gavinsimpson.github.io/gratia/reference/smooth_label.md)
  : Extract the label for a smooth used by 'mgcv'
- [`smooth_type()`](https://gavinsimpson.github.io/gratia/reference/smooth_type.md)
  : Determine the type of smooth and return it in a human readable form
- [`smooth_terms()`](https://gavinsimpson.github.io/gratia/reference/smooth_terms.md)
  **\[experimental\]** : List the variables involved in smooths
- [`smooth_coefs()`](https://gavinsimpson.github.io/gratia/reference/smooth_coefs.md)
  : Coefficients for a particular smooth
- [`smooth_coef_indices()`](https://gavinsimpson.github.io/gratia/reference/smooth_coef_indices.md)
  : Indices of the parametric terms for a particular smooth
- [`is_by_smooth()`](https://gavinsimpson.github.io/gratia/reference/is_by_smooth.md)
  [`is_factor_by_smooth()`](https://gavinsimpson.github.io/gratia/reference/is_by_smooth.md)
  [`is_continuous_by_smooth()`](https://gavinsimpson.github.io/gratia/reference/is_by_smooth.md)
  [`by_variable()`](https://gavinsimpson.github.io/gratia/reference/is_by_smooth.md)
  [`by_level()`](https://gavinsimpson.github.io/gratia/reference/is_by_smooth.md)
  : Tests for by variable smooths

### Families and links

Inspect response distributions, link functions, and linear predictors.

- [`family(`*`<gam>`*`)`](https://gavinsimpson.github.io/gratia/reference/family.gam.md)
  [`family(`*`<gamm>`*`)`](https://gavinsimpson.github.io/gratia/reference/family.gam.md)
  [`family(`*`<bam>`*`)`](https://gavinsimpson.github.io/gratia/reference/family.gam.md)
  [`family(`*`<gamm4>`*`)`](https://gavinsimpson.github.io/gratia/reference/family.gam.md)
  : Extract family objects from models
- [`family_name()`](https://gavinsimpson.github.io/gratia/reference/family_name.md)
  : Name of family used to fit model
- [`family_type()`](https://gavinsimpson.github.io/gratia/reference/family_type.md)
  : Extracts the type of family in a consistent way
- [`link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  [`inv_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  [`extract_link()`](https://gavinsimpson.github.io/gratia/reference/link.md)
  : Extract link and inverse link functions from models
- [`has_theta()`](https://gavinsimpson.github.io/gratia/reference/has_theta.md)
  : Are additional parameters available for a GAM?
- [`n_eta()`](https://gavinsimpson.github.io/gratia/reference/n_eta.md)
  **\[experimental\]** : The Number of linear predictors in model

## Explore spline bases and penalties

Inspect and visualise the basis functions and penalties that define
smooths.

- [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md)
  : Basis expansions for smooths

- [`draw(`*`<basis>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.basis.md)
  : Plot basis functions

- [`tidy_basis()`](https://gavinsimpson.github.io/gratia/reference/tidy_basis.md)
  : A tidy basis representation of a smooth object

- [`draw(`*`<mgcv_smooth>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.mgcv_smooth.md)
  : Plot basis functions

- [`penalty()`](https://gavinsimpson.github.io/gratia/reference/penalty.md)
  : Extract and tidy penalty matrices

- [`draw(`*`<penalty_df>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.penalty_df.md)
  :

  Display penalty matrices of smooths using `ggplot`

- [`boundary()`](https://gavinsimpson.github.io/gratia/reference/boundary.md)
  **\[experimental\]** : Extract the boundary of a soap film smooth

## Example data and simulation

Generate example data or use bundled data sets to learn and demonstrate
GAM workflows.

- [`data_sim()`](https://gavinsimpson.github.io/gratia/reference/data_sim.md)
  : Simulate example data for fitting GAMs
- [`data_sim_families`](https://gavinsimpson.github.io/gratia/reference/data_sim_families.md)
  : Generating responses with mgcv family objects
- [`data_sim_shapes`](https://gavinsimpson.github.io/gratia/reference/data_sim_shapes.md)
  : Reference functions for shape-constrained GAMs
- [`gw_f0()`](https://gavinsimpson.github.io/gratia/reference/gw_functions.md)
  [`gw_f1()`](https://gavinsimpson.github.io/gratia/reference/gw_functions.md)
  [`gw_f2()`](https://gavinsimpson.github.io/gratia/reference/gw_functions.md)
  [`gw_f3()`](https://gavinsimpson.github.io/gratia/reference/gw_functions.md)
  : Gu and Wahba test functions
- [`rtw()`](https://gavinsimpson.github.io/gratia/reference/rtw.md) :
  Simulator for tweedie LSS models
- [`bird_move`](https://gavinsimpson.github.io/gratia/reference/bird_move.md)
  : Simulated bird migration data
- [`gss_vocab`](https://gavinsimpson.github.io/gratia/reference/gss_vocab.md)
  : Data from the General Social Survey (GSS) from the National Opinion
  Research Center of the University of Chicago
- [`ref_sims`](https://gavinsimpson.github.io/gratia/reference/ref_sims.md)
  : Reference simulation data
- [`smallAges`](https://gavinsimpson.github.io/gratia/reference/smallAges.md)
  : Lead-210 age-depth measurements for Small Water
- [`zooplankton`](https://gavinsimpson.github.io/gratia/reference/zooplankton.md)
  : Madison lakes zooplankton data

## Advanced programming helpers

Build custom computations using lower-level evaluation, sampling, and
validation tools.

### Model evaluation and posterior sampling

- [`lp_matrix()`](https://gavinsimpson.github.io/gratia/reference/lp_matrix.md)
  [`model.matrix(`*`<scam>`*`)`](https://gavinsimpson.github.io/gratia/reference/lp_matrix.md)
  : Return the linear prediction matrix of a fitted GAM
- [`eval_smooth()`](https://gavinsimpson.github.io/gratia/reference/eval_smooth.md)
  : S3 methods to evaluate individual smooths
- [`spline_values()`](https://gavinsimpson.github.io/gratia/reference/spline_values.md)
  : Evaluate a spline at provided covariate values
- [`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
  [`generate_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
  : Low-level Functions to generate draws from the posterior
  distribution of model coefficients
- [`gaussian_draws()`](https://gavinsimpson.github.io/gratia/reference/gaussian_draws.md)
  : Posterior samples using a Gaussian approximation
- [`mh_draws()`](https://gavinsimpson.github.io/gratia/reference/mh_draws.md)
  : Posterior samples using Metropolis Hastings
- [`user_draws()`](https://gavinsimpson.github.io/gratia/reference/user_draws.md)
  : Handle user-supplied posterior draws

### Validation and data utilities

- [`check_user_select_smooths()`](https://gavinsimpson.github.io/gratia/reference/check_user_select_smooths.md)
  : Select smooths based on user's choices

- [`is_factor_term()`](https://gavinsimpson.github.io/gratia/reference/is_factor_term.md)
  : Is a model term a factor (categorical)?

- [`is_mgcv_family()`](https://gavinsimpson.github.io/gratia/reference/is_mgcv_family.md)
  : Is an object one of mgcv's family objects?

- [`is_mgcv_smooth()`](https://gavinsimpson.github.io/gratia/reference/is_mgcv_smooth.md)
  [`stop_if_not_mgcv_smooth()`](https://gavinsimpson.github.io/gratia/reference/is_mgcv_smooth.md)
  [`check_is_mgcv_smooth()`](https://gavinsimpson.github.io/gratia/reference/is_mgcv_smooth.md)
  [`is_mrf_smooth()`](https://gavinsimpson.github.io/gratia/reference/is_mgcv_smooth.md)
  : Check if objects are smooths or are a particular type of smooth

- [`is_multivariate_y()`](https://gavinsimpson.github.io/gratia/reference/is_multivariate_y.md)
  : Is a model multivariate?

- [`is_offset()`](https://gavinsimpson.github.io/gratia/reference/is_offset.md)
  : Is a model term an offset?

- [`fix_offset()`](https://gavinsimpson.github.io/gratia/reference/fix_offset.md)
  : Fix the names of a data frame containing an offset variable.

- [`seq_min_max_eps()`](https://gavinsimpson.github.io/gratia/reference/seq_min_max_eps.md)
  : Create a sequence of evenly-spaced values adjusted to accommodate a
  small adjustment

- [`shift_values()`](https://gavinsimpson.github.io/gratia/reference/shift_values.md)
  :

  Shift numeric values in a data frame by an amount `eps`

- [`rep_first_factor_value()`](https://gavinsimpson.github.io/gratia/reference/rep_first_factor_value.md)
  : Repeat the first level of a factor n times

- [`to_na()`](https://gavinsimpson.github.io/gratia/reference/to_na.md)
  :

  Sets the elements of vector to `NA`

- [`vars_from_label()`](https://gavinsimpson.github.io/gratia/reference/vars_from_label.md)
  : Returns names of variables from a smooth label

- [`load_mgcv()`](https://gavinsimpson.github.io/gratia/reference/load_mgcv.md)
  : Load mgcv quietly

## Deprecated and legacy interfaces

Reference for older code; use
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
[`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md),
and
[`derivatives()`](https://gavinsimpson.github.io/gratia/reference/derivatives.md)
for new analyses.

- [`evaluate_smooth()`](https://gavinsimpson.github.io/gratia/reference/evaluate_smooth.md)
  **\[deprecated\]** : Evaluate a smooth
- [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md)
  **\[deprecated\]** : Evaluate parametric model terms
- [`draw(`*`<evaluated_parametric_term>`*`)`](https://gavinsimpson.github.io/gratia/reference/draw.evaluated_parametric_term.md)
  **\[deprecated\]** : Plot estimated parametric effects
- [`confint(`*`<fderiv>`*`)`](https://gavinsimpson.github.io/gratia/reference/confint.fderiv.md)
  : Point-wise and simultaneous confidence intervals for derivatives of
  smooths
