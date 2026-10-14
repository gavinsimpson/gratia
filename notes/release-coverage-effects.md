# Coverage of effect and spatial smooth articles

These rows reconcile the delegated feature audit with the completed articles.
The latest CRAN baseline and history are recorded in release-article-coverage.md.
Descriptions below distinguish executed worked examples from support that is
explained without fitting another model.

| Feature | Public APIs | Existing documentation | New article section | Worked example and scope |
|---|---|---|---|---|
| glmmTMB penalised spline extraction and plots | smooths(), smooth_coefs(), smooth_estimates(), draw() | glmmTMB-methods | glmmTMB-support: river flow and smooth uncertainty | Official glmmTMB Nile spline adapted to year/flow; Gaussian REML fit compared with identical mgcv spline |
| Pointwise credible alias and simultaneous smooth bands | smooth_estimates(), draw(), assemble(), confint() | smooth_estimates, draw.gam, confint.gam | glmmTMB-support: extract trend and read uncertainty | Executed credible estimates and simultaneous Nile band; scope per fitted smooth over finite grid explained |
| Frequentist covariance selection and public extraction | smooth_estimates(), get_vcov() | smooth_estimates, get_vcov | glmmTMB-support: mgcv covariance choices | mgcv Nile effects at 1880 and 1920; Bayesian/frequentist SE table; custom covariance extraction; explicitly unavailable for glmmTMB |
| Parametric effects included by default and intercept covariance | draw(), assemble(), parametric_effects() | draw.gam, parametric_effects | glmmTMB-support: repeated measurements | CO2 Gamma spline with Type * Treatment and plant effects; exact term-only/default distinction explained |
| Native random intercept and slope panels/lines | draw(ranef_lines) | glmmTMB-methods | glmmTMB-support: individual plant responses | Actual penalised CO2 Gamma fit; separate fitted-coefficient QQ plots and random-contribution lines; unsupported line structures retain QQ |
| Numeric, factor and higher-order parametric interactions | parametric_effects(), draw() | parametric_effects, draw.gam | smooth-effects: weather associations and reconstruction | Airquality log-ozone model Temp * Wind * month, solar smooth; numeric surfaces, monthly facets and numeric-factor curves; factor-factor CO2 interaction in main guide |
| Plot surface resolution and vector tiles | smooth_estimates(n_2d), parametric_effects(n_2d), draw(surface), assemble() | draw.gam, smooth_estimates | smooth-effects/spatial-smooths: surfaces | 30-point temperature/wind grid; 25-point monthly facets; 35-point soap plots; numeric/factor conditioning limits explained |
| Optional 3D surfaces | draw(surface = '3d', surface_3d), assemble() | draw.gam | smooth-effects: choose display | Executed temperature/wind contribution with ggcube yaw and orthographic view; package optional and flat analysis complete |
| Random-effect tensor continuous/factor curves and selected levels | smooth_estimates(), draw(re_levels), assemble() | draw.gam; tensor plotting tests | smooth-effects: different plant curve shapes | CO2 Gamma mgcv common curve plus full t2(log_conc,Plant) deviations; selected named plants and recovered legend |
| Higher-dimensional factor margins and two-factor categorical tensor display | smooth_estimates(), draw(), assemble() | draw.gam; test-tensor-re-plots and test-draw-tensor-factors | smooth-effects: tensor display rules | Explained; no newly fitted factor-only tensor or 3/4-dimensional tensor example in these articles. Existing tests exercise constructors and display forms; do not count as new worked analyses |
| Full/reduced-rank MRF area estimates and interval/QQ displays | smooth_estimates(), draw(mrf_plot) | smooth_estimates, draw.gam; mrf tests | spatial-smooths: crime, income and district effects | Official mgcv Columbus 49 districts, full and reduced spatial-only fits plus income-adjusted fit; all area effects retained; descriptive QQ warning |
| Soap boundary and wiggly components | smooth_estimates(), draw(), boundary(), penalty() | draw.gam, boundary; soap tests | spatial-smooths: peninsula and boundary/interior variation | Official mgcv curved-domain simulation, independent Gaussian noise, REML sf + sw with original boundary/knots; correct component types, numeric selection, clipping, shared fill scales |
| Geographic cells, orthographic clipping, date-line/poles | draw(), smooth_estimates() | draw.gam; spherical rendering tests | spatial-smooths: global field recovery | Adapted official sphere simulation with direct area-uniform sampling and continuous polar truth, spherical spline, sf orthographic plot; seam numerical check |
| Factor-by spherical facets | draw(grouped_by) | draw.gam and tests | spatial-smooths: global field interpretation | Explained as support; no separate fitted survey-by-location example in these articles |
| Tile rendering of penalty matrices | penalty(), draw(geom = 'tile') | penalty, draw.penalty_df | spatial-smooths: neighbourhood penalty | Executed reduced MRF penalty with explanation that fitted basis rows are not districts |
| Variance components when intervals absent or no components | variance_comp() | variance_comp; tests | smooth-effects: plant variance components | Executed CO2 REML tensor components; GCV/no-component behaviour explained rather than a separate fitted analysis |
| Plot axes for transformed expressions | draw(back_transform), smooth_estimates(), parametric_effects() | transformed-covariates vignette | smooth-effects: display coordinates | Centred log concentration used in both plant examples; links to existing expanded transformed-covariates vignette |
| Multiple glmmTMB model parts, smooth labels and support limits | smooths(), smooth_estimates(), draw(), parametric_effects() | glmmTMB-methods | glmmTMB-support: final support guide | Conditional smooths worked; zero-inflation/dispersion smooth selection and subtitles explained, not separately fitted in articles. Focused regression covers parametric effects in conditional/dispersion parts |
| Native and calibrated diagnostic references | appraise(), residual_correlation_plot() | diagnostic_support; residual_correlation | glmmTMB-support: distribution and serial structure | Nile native Pearson QQ with conditional simulated reference and correlation plot; CO2 conditional quantile residual panels. Broader conditioning/PPC covered in model-checking |

## Explicit limits and remaining example gaps

Three- and four-dimensional random-effect tensors, factor-only tensors,
factor-by spherical maps, glmmTMB zero-inflation/dispersion smooths and soap
space-by-time tensors are explained and linked to reference documentation, but
have no newly fitted worked analysis here. This is distinct from their
implementation/regression coverage. Unsupported glmmTMB covariance choices,
partial-residual overlays, smooth structures with unsupported multiple
penalties, fixed/shared joint parameters and rank-deficient smooth models are
retained explicitly. Model geometry does not establish response independence.
