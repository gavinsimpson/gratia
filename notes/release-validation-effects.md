# Validation of effect and spatial smooth articles

Validation used this checkout through pkgload::load_all(), rather than the
older installed gratia. All R validation, rendering and package tests were
run outside the sandbox, as required by AGENTS.md.

## Fitting and numerical checks

R 4.6.1, mgcv 1.9-4, glmmTMB 1.1.15.9000. sf, geometry and ggcube were
installed, so both optional spherical and 3D figures were actually rendered.
Nile and CO2 glmmTMB fits returned convergence code zero and pdHess TRUE.

Independent checks used fitting-package predictions and covariance matrices,
not gratia's plotting calculations as their reference:

| Calculation | Independent reference | Maximum absolute discrepancy |
|---|---|---|
| Nile glmmTMB complete mean (smooth plus intercept) | predict.glmmTMB() | 6.15e-9 |
| Nile glmmTMB complete mean standard error | predict.glmmTMB(se.fit = TRUE) | 4.32e-9 |
| Nile glmmTMB versus mgcv fitted mean | predict.gam() using identical Gaussian REML spline | 9.29e-5 |
| CO2 glmmTMB Gamma fitted response mean | predict.glmmTMB(type = 'response') | 0 |
| CO2 mgcv tensor plant effects and term-only standard errors | predict.gam(type = 'terms', se.fit = TRUE), named tensor column | 0 |
| Airquality Temp:Wind, Temp:month, Temp:Wind:month effects | Coefficient blocks of model.matrix(pterms) times coef() | 0 |
| Airquality interaction SE including intercept covariance | sqrt(rowSums((X V) * X)), with term/intercept columns only | <= 8.89e-16 |
| Columbus reduced MRF adjusted area effects and term-only SE | predict.gam(type = 'terms', se.fit = TRUE) | 0 |
| Public frequentist covariance extraction | vcov.gam(freq = TRUE) | 0 |
| Soap sf/sw component effects and term-only SE | predict.gam(type = 'terms', se.fit = TRUE), positions 1 and 2 | 0 |
| Spherical smooth effects and term-only SE | predict.gam(type = 'terms', se.fit = TRUE) | 0 |
| Spherical predictions across -180/180 degrees longitude | Direct fitted-package predictions at latitude -90, -45, 0, 45, 90 | <= 5.56e-16 |

The official soap simulation yielded 402 retained observations. RMSE of the
fitted complete field relative to known truth was 0.0852 at those locations;
estimated Gaussian SD 0.3048 versus generated SD 0.3. The new Nile correlation
API reported lag-one Pearson correlation 0.2350 across 99 pairs and lag-two
correlation 0.0809 across 98 pairs, informing the serial-dependence discussion.
These checks do not make the examples held-out validation or validate their
working independence assumptions on real data.

The final polar-continuous spherical simulation had mean-field RMSE 0.0426
at its 400 observed locations. Its effect and SE checks were repeated after
the simulation adaptation and matched the fitting package exactly.

The calculation script and fit objects used for local checks are in
/private/tmp/gratia-effects-validate.R, gratia-effects-fits.rds and
gratia-smooths-fits.rds. They are validation scratch files, not article
dependencies. The central notes/validate-release-articles.R script provides
the shared release checks.

## Implementation problem fixed

Default draw() of the CO2 glmmTMB model initially failed while building its
Type:Treatment parametric panel: the renderer tried a numeric rug using a
nonexistent .value column. R/mixed-smooths.R had concatenated named
per-predictor lists of factor/term/axis metadata, prefixing keys with cond.
while effect rows retained ordinary labels. Removing the names of the outer
lists preserves the correct fitted term keys across model parts. This also
restores transformed observation coordinates and factor level metadata.

A focused regression in test-glmmtmb-effects.R builds the saved effects and
all default model panels for conditional/dispersion interactions and transformed
covariates. It verifies categorical geoms, axis labels, metadata keys and
actual rug coordinates, so constructing an unevaluated ggplot cannot hide the
original failure. Affected glmmTMB effects and shared parametric interaction
test files pass with no failures or warnings. No full suite was repeated.

Final visual inspection also exposed an existing shared-scale problem:
assemble() formed per-smooth ranges with missing soap-boundary values, so
scales = 'fixed' supplied nonfinite limits and the components reverted to
separate colour scales. R/assemble.R now calculates ranges using finite
effect/interval values and similarly filters residual ranges. A focused
test-soap-components.R regression checks identical finite sf/sw fill limits
against the independently calculated shared effect/interval range. Existing
draw-gam, prepare-surface-plot and soap-components tests pass with no failures
or warnings after the fix. The final soap plot omits contours and uses common
fill limits so tiny interior variation is not visually exaggerated.

An early article attempted simulated QQ references with calibrated quantile
residuals. The implementation correctly rejects that combination: calibrated
scores have a fixed standard-normal reference. The article now uses native
Pearson residuals for conditional simulated QQ references and distinguishes
individual-score distribution checks in the CO2 quantile panels. This was an
example correction, not an implementation defect.

## Rendering and visual inspection

glmmTMB-support.Rmd, smooth-effects.Rmd and spatial-smooths.Rmd rendered
successfully using rmarkdown::render() outside the sandbox. There were no
R warnings from the successful renders. Pandoc reports its existing MathJax
command-line deprecation warning, unrelated to these articles.

Local self-contained HTML outputs are under
/private/tmp/gratia-effects-rendered. Their embedded PNGs were extracted for
visual inspection under /private/tmp/gratia-effects-figures. Inspected figures
include simultaneous Nile bands, all CO2 default panels and random lines,
selected tensor plant curves, numeric and month-faceted interactions, 3D
weather surfaces, district intervals/QQ/penalties, soap boundary/interior
components and the orthographic spherical map. The following improvements
were made after inspection:

- Removed a patchwork ncol setting that had shrunk one already-faceted monthly
  interaction to a narrow strip; monthly facets now use the figure width.
- Applied meaningful weather units through patchwork labs() and restored a
  legend identifying the three selected plant curves.
- Stacked the soap components with shared fill scales, so automatic scales
  cannot visually exaggerate the almost-zero wiggly component.
- Corrected the interpretation of reduced-basis penalty rows: they are
  coefficient directions, not district labels.
- Stated that interaction masking pools weather support across months;
  a visible cell can still extrapolate for an individual month.

The parent task performs the pkgdown site build and browser inspection, and
reconciles final article coverage and remaining example gaps.
