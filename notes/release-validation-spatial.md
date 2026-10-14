# Spatial article validation

All fitting, executable rendering, and numerical checks ran with `sandbox_permissions="require_escalated"`, using `pkgload::load_all(".")` to load this branch. The installed gratia package is older and was not used for the checks. R 4.6, mgcv 1.9-4, sdmTMB 1.1.0.

## Fit and independent calculations

- Full Pacific cod survey: 2143 tows, nine survey years, all observations retained. Delta-Gamma, year factor plus s(depth, k = 6), mesh cutoff 10 km, persistent spatial and independent year-specific fields for both components. Fitting took about 13 seconds on this machine.
- Native `sdmTMB::sanity()` passed all checks: optimizer convergence, positive-definite Hessian, gradients, standard errors, variance parameters and spatial ranges.
- Spatial ranges estimated at about 35.7 km for encounter and 12.3 km for positive density; spatial SDs about 1.79 and 0.626. Year-field SDs about 0.766 and 1.28. The positive range is close enough to the mesh cutoff that the article explicitly requires mesh refinement before a substantive analysis; refinement is not claimed to have been performed.
- Reconstructed each component's predictor without fields from native tidy fixed coefficients plus gratia smooth contribution: maximum differences 2.22e-15 and 8.88e-16.
- Independent basis calculation using public `get_smooths_by_id()`, `smooth_coefs()` and `mgcv::PredictMat()` reproduced both gratia smooth estimates exactly at the 20 checked covariate rows.
- Native full delta response mean compared with `plogis(est1) * exp(est2)`: maximum absolute difference 1.14e-13 across all fitting observations.
- Independent positive-component Gamma CDF from native `sdmTMB::tidy(model = 2, effects = "ran_pars")` shape and native `predict()` mean, transformed with `qnorm()`, matched gratia `quantile_residuals(component = "positive", field_method = "mle-eb")` at the retained positive rows to 2.75e-14. Zero tows correctly have NA entries in this component's restored row layout.
- Smooth standard errors are not asserted to equal standard errors from predictions in the reference year: default smooth uncertainty includes the average predictor mean using observed design-column means, whereas a reference-year full prediction has a different uncertainty design. This expected distinction was checked in implementation; no bug was inferred from it.

## Executable article and figures

The complete article rendered successfully twice with rmarkdown, including all four residual field choices, full delta mixture predictions, component QQ/PPC and joint parameter draws using conditional fitted fields and marginal new fields. The only render warning was pandoc's existing `--mathjax` deprecation. The latest full render is `/private/tmp/gratia-spatial-render/sdmTMB-support.html`; after that render only descriptive text and the observed map's colour transformation changed. The revised map alone was executed and visually checked. Parent full pkgdown build will render the final source.

Figures inspected: observed survey map, depth effects, year contrasts, full predicted density maps, default conditional QQ, integrated QQ, marginal QQ, positive-component QQ, occurrence PPC, residual maps. Scientific units were added to depth and year-effect axes; component subtitles distinguish encounter and positive density. Observed-density colour uses `log1p` to show smaller catches without dropping zeros or extreme catches. Maps use equal coordinate scales. Native diagnostic subtitles remain visible and identify conditioning and reference calculations. Long integrated subtitles consume space but remain legible at the seven-inch figure height.

The default conditional QQ shows a positive upper-tail departure (largest normal residual around 5.8); the positive-component QQ locates that departure in positive density. Fitted-effect integration narrows residual variation, while new-field marginal ranks lie much closer to their diagonal. The narrative explains why these answer different questions, does not select a favourable mode to certify the fit, and suggests checking a delta-lognormal alternative rather than treating altered conditioning as a model repair.

Observed zero frequency = 0.5380; observed 95th density percentile = 156.996 kg/km². With 25 joint posterior response draws (seed 1405):

| Effects | Summary | Median | 5th percentile | 95th percentile |
| --- | --- | --- | --- | --- |
| Uncertain fitted fields | Zero proportion | 0.543 | 0.524 | 0.557 |
| Uncertain fitted fields | 95th density percentile | 160 | 147 | 185 |
| New fields | Zero proportion | 0.494 | 0.412 | 0.614 |
| New fields | 95th density percentile | 180 | 149 | 241 |

The article explains that reproducing these aggregate summaries does not establish adequate representation of the extreme tail or spatial structure. Simulation counts of 49 reference responses and 25 PPC replicates keep the article manageable; their limited tail resolution and Monte Carlo accuracy are explicit.

## Implementation audit

Claims were checked against `R/sdmtmb.R`, `R/sdmtmb-diagnostics.R`, `R/diagnostic-posterior.R`, `R/smooth-intervals.R` and existing sdmTMB adapter/dependence tests. Default mle-mvn draws all fitted latent effects together once and uses the same state for observed/reference responses. Mle-eb retains estimates. Integrated mode uses native joint simulations with repeated fitted-effect draws (including penalised smooth coefficients), fixed maximum-likelihood coefficients and covariance parameters, and simulation ranks. Marginal references generate new native spatial/temporal/group effects, keep fitted smooths and use joint dependence-preserving intervals. Posterior predictive draws jointly include coefficients, nuisance parameters and effects; marginal PPCs replace fields at each parameter draw while retaining posterior smooth draws. Unsupported cases are explicit in the article.

No implementation changes or new regression tests were required. This article does not claim numerical validation of every supported family, backend or latent-effect structure; those are listed as scope limits in the spatial coverage notes.

## Tensor extension

A worked extension fits all 2143 encounter indicators with a Bernoulli logit response, `t2(depth, year_centered, k = c(5,4))`, the existing 10 km mesh, persistent spatial field and independent survey-year spatial fields. It explicitly adapts the official basic introduction's quadratic depth encounter model to ask about temporal changes in the depth relationship. Fitting took about five seconds. Native optimizer convergence, positive-definite Hessian, gradients, fixed standard errors and field parameters pass sanity checks. Native `sanity()` warns of large `ln_smooth_sigma` standard errors: two tensor penalty SDs shrink near zero (about 3.1e-5 and 0.0017), while the third is about 50.1. The warning is retained in executable output and explained as a reason for caution and considering the simpler model. An exploratory REML refit produced the same boundary behavior; no prior was silently introduced.

The tensor has three penalties and nineteen coefficients; the article demonstrates this with public `get_smooths_by_id()` and `smooth_coefs()`. The 30 by 30 surface was inspected; nearly vertical contours, a stable intermediate-depth peak, and masks at sparsely supported deep depth-year combinations are discussed. Axes show metres and actual survey years despite the centred fitting coordinate. It displays a link-scale contribution, not a probability or uncertainty map.

For the ML fit, native prediction without fields at 50 real survey rows matches the intercept plus gratia tensor contribution to 7.03e-9. The native link prediction standard errors match the default gratia tensor standard errors to 4.51e-10. An independent public-basis calculation using `mgcv::PredictMat()` and `smooth_coefs()` matches all checked contribution estimates exactly. The complete new section executes and renders independently; the parent's full pkgdown build should render the final integrated article. No implementation issue was found.
