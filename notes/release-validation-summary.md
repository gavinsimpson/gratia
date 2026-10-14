# Release article validation summary

Validated on 14 October 2026, using R 4.6.1, mgcv 1.9-4,
glmmTMB 1.1.15.9000 and sdmTMB 1.1.0. This branch starts at the
verified main commit 419930d19, with the merged diagnostics changes and
DESCRIPTION version 0.11.2-49. No release version or merge was assumed.

## Articles and site

Eight new articles are under vignettes/articles. The four primary workflows
cover diagnostics, gfam, glmmTMB and sdmTMB; companion workflows cover
prediction comparisons, smooth effects, spatial smooths and response families.
The coverage table was prepared before writing and reconciled after execution
and independent review. Guide-only families and remaining example gaps are
listed explicitly in release-article-coverage.md and the detailed coverage notes.

- Every new article executed and rendered successfully with current source,
  outside the sandbox. Optional sf and ggcube figures were actually rendered.
- The entire pkgdown site built successfully with examples = FALSE, lazy = FALSE
  and preview = FALSE. pkgdown installed this checkout into a temporary library,
  rather than using the older installed gratia. Reference-page examples were
  not executed; every article, including existing articles and the changed
  transformed-covariates vignette, was executed.
- pkgdown::check_pkgdown() reported no problems. New workflow groups and
  diagnostic reference links are in pkgdown/_pkgdown.yml. Website dependencies
  are recorded in DESCRIPTION's Config/Needs/website.
- Final refinements and source fixes were followed by affected article rebuilds
  using pkgload::load_all() and new_process = FALSE. NEWS was regenerated.
- The local browser showed the new article index, version badge, navigation,
  diagnostics article and spatial survey article with readable content and
  section navigation. Figures were visually inspected through rendered PNGs
  and the browser. Units, component labels, conditioning subtitles, colour
  ranges, legends and facet widths were corrected where needed.
- notes/check-release-article-links.py checks local files, anchors and image
  paths in the eight new pages and article index; all 530 checks passed.
- Pandoc emits existing MathML/MathJax command-line deprecation messages.
  Successful builds have no execution errors. The sdmTMB tensor model's large
  smoothing-parameter SE is displayed and interpreted; it is not suppressed
  or treated as evidence that the richer model is suitable.

## Independent numerical checks

See release-validation-{diagnostics,prediction,effects,spatial}.md for the
calculations and limits. Highlights:

- Prediction means match native mgcv exactly; contrast calculations agree
  within 3e-14, response derivatives within 3e-10, and raw-coordinate chain-rule
  calculations within 2e-12.
- Grouped-family means match direct inverse links and native mgcv within 2e-16;
  component response draws match independent binomial/Poisson generators exactly.
  Weighted, count, binomial and censored PITs match independent R CDFs.
- Gaussian AR1 response simulations preserve rho^lag correlations and series
  boundaries. Working-AR1 BAM references omit independence bands and reject
  joint response simulation and PPC, as documented.
- Nile glmmTMB effects plus intercepts and SEs match native predictions within
  7e-9. The equivalent mgcv and glmmTMB fitted means differ by at most 9.3e-5.
- Spatial delta response means and Gamma residual calculations match independent
  definitions within 2e-13. The depth/year tensor matches native predictions
  and SEs within 1e-8 and independent basis reconstruction exactly.
- Interaction, MRF, soap and spherical effects/SEs match independent fitting-
  package calculations. The global surface is continuous across the date line
  within 2e-15. These calculations verify implementation; they do not establish
  causal conclusions, out-of-sample performance or calibrated post-fit tests.

notes/validate-release-articles.R preserves the shared independent calculations.
The user-facing articles contain fitting and interpretation rather than testing
machinery.

## Implementation defects and affected tests

Two plot defects were found by executing and inspecting the examples:

1. Mixed-model parametric term metadata gained predictor-name prefixes, so
   factor interactions could be rendered as numeric effects with invalid rugs.
   R/mixed-smooths.R now retains the original term keys. A focused regression
   builds actual glmmTMB panels across conditional/dispersion components and
   verifies interaction geometry, axes and observation coordinates.
2. Missing soap boundary rows produced nonfinite shared limits in assemble(),
   making fixed scales revert to separate component scales. R/assemble.R now
   uses finite estimates, interval bounds and residual values. A focused soap
   regression compares both rendered scales with an independently calculated
   range. The final sf/sw example shares [-4.231753, 4.413976], correctly showing
   the very small wiggly contribution without exaggeration.

Affected test files passed outside the sandbox: test-glmmtmb-effects.R,
test-parametric-effects-interactions.R, test-sdmtmb.R, test-draw-gam.R,
test-prepare-surface-plot.R and test-soap-components.R. One sdmTMB visual snapshot
test was skipped under its On CRAN condition; the executable spatial articles
and their figures were independently rendered and inspected. The full package
test suite and R CMD check were not run for this documentation task.

git diff --check passed. Temporary PDFs created by these checks were removed;
no unrelated working-tree changes existed at the start. The work is committed
on gls/release-workflow-articles; main is not merged and no CRAN submission or
publication has been performed.
