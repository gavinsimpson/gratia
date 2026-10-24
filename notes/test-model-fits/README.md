# Audit of model fitting in the tests

The goal is to avoid paying for a new fit when a suitable model has already
been fitted. The audit covers every R file under `tests/`, including helpers,
shared fixtures and optional integration tests. The HGAM paper code was read
without running it.

The audit inspected 277 current R files and recorded 1,140 potential fitting
call sites in 177 files. The accompanying [inventory](inventory.md) lists each potential fitting call
with its file, line, enclosing test, formula and family. It also lists files
without a direct fitting call. These are source locations, not counts of fits
performed: loops, helper calls, caching and skips change the number performed.
Calls through fitting aliases and `do.call()` are included; `update()` can
update a formula rather than refit a model.

## Decisions

Use an existing setup model when its class, family, fitted terms, data and
retained fitting information cover the assertion. The same formula text is
not enough to establish that two fits are interchangeable. In particular,
missing responses, missing predictors, subsets, likelihood weights, offsets,
contrast coding and smoothing-parameter estimation can be the property being
tested.

When a specialized model is needed repeatedly, construct it on first use and
reuse it through the shared setup environment. Keep optional families and
expensive structures out of unconditional setup. Cache different links,
fitting methods, bases, missing-row conventions and other fitting controls
separately.

Keep a separate fit when the test checks fitting itself, compares independent
fitters on exactly the same observations, needs a different fitted design, or
needs fresh mutable native state. A small dedicated reference can also be
cheaper overall than reusing a much larger model for hundreds of posterior
draws. Reducing the number of fitting calls is useful, but elapsed test time
remains the performance measure that matters.

## Changes made

The shared cache and builders live in `tests/testthat/setup.R`. The cache
belongs to one test setup environment. Under parallel testing each worker has
its own setup and cache; it is not a cache of files or fitted models between
R sessions. The new tensor-product GAMM and factor-smooth mixed models are
also constructed on first access.

| Tests | Decision and replacement |
| --- | --- |
| `test-variance-components-terms.R` | Reuse `su_m_bivar_te`, `m_fs`, `su_gamm_bivar_te`, `m_fs_gamm`, `gamm4_surface` and `m_fs_gamm4` for the multi-penalty checks. Include the existing surface model's native random intercept in its expected term contributions. |
| `test-variance-components-gamm.R`, `test-variance-components-gamm4.R` | Reuse the shared tensor, factor-smooth and factor-by fits for penalty mapping. Use `gamm4_gaussian` for Gaussian profile and profile-failure checks and existing mixed models for argument validation. |
| `test-variance-components-totals.R`, `test-variance-components-proportions.R` | Share the exact simulated GAM/BAM reference fits through `variance_total_models()` and the complete-data random-slope fits through `variance_mixed_fixture()`. Their numerical references remain separate. |
| `test-variance-components-terms.R`, `test-variance-components-term-intervals.R` | Share the exact shuffled, subsetted, missing-row mixed-model fits through `variance_mixed_fixture(missing = TRUE)`. |
| Term, interval, proportion, total-selection and variance plotting tests | Share the constrained fit as `variance_scam_model` and the intercept-only fit as `variance_intercept_model`. General validation uses existing setup models. |
| `test-plot-themes.R` | Use `m_1_smooth`, `su_m_quick_eg1` and `su_m_bivar`. Keep the first style test checking several returned panels. |
| `test-prepare-smooth-plot.R` | Use `su_m_cont_by` and the factor-smooth term from `m_fs`; update the expected labels to those models' actual covariate names. |
| `test-prepare-surface-plot.R` | Reuse the shared GAMM tensor and `gamm4_surface`. Add a BAM surface builder using existing `su_eg2` data, on demand. |
| `test-draw-label-control.R` | Use `su_m_bivar`, `b_pois` and `m_gamm` for the generic surface, rootogram and GAMM checks. Keep the dedicated models whose factor levels contain the long labels under test. |
| `test-assemble-parametric-selection.R` | Use `gamm4_gaussian$gam` for a smooth plus two parametric terms. Use `m_1_smooth` and `sampling_linear_model` for smooth-only and parametric-only checks. |
| `test-sampling-interface.R`, `test-sampling-review.R`, `test-sampling-likelihood-integration.R` | Share the compact four-coefficient `sampling_interface_model` and the two-coefficient `sampling_linear_model`. These retain the coefficient counts and small prediction data used throughout the sampling checks. |
| `test-sampling-mh-adjustment.R` | Construct `adjustment_fixture()` once. Keep the separate BAM fit because the assertion checks BAM's corrected covariance. |
| Conditional-difference, simultaneous-difference and fitted-parameter tests | Cache `conditional_difference_fixture()` by family and link. Its interaction, factor order and varying offset are required by the independent calculations. |
| `test-conditional-surfaces.R` | Cache `conditional_surface_fixture()` by family and link. Keep fits made after changing the observed combinations or distributional predictors. |
| `test-conditional-values-complete.R` | Share `conditional_combos_fixture()` through setup. Keep the numeric-only comparison model because excluding unmodelled categorical predictors is the assertion. |
| `test-residuals-covariate-plot.R` | Cache `covariate_plot_fixture()` by sample size. Keep the deliberately misspecified, transformed, subsetted, multivariate and native-backend models. |
| Smoothing-sampler tests | Cache `smoothing_research_fixture()` and `smoothing_family_fixture()` using all their configuration arguments. Keep independent fixed-parameter refits used as numerical references. |
| `test-mcmc-chains.R`, `test-family-mh.R` | Cache the small weighted, offset-bearing sampling models by family, link, engine and discrete setting. Keep their separate data designs and independent posterior reference fits. |
| `test-random-effect-samples.R` | Cache repeated fits by family, link, factor-smooth setting, engine and all extra fitting arguments. Preserve the existing data generation and seed handling. |
| `test-tube-intervals.R` | Cache `tube_fixture()` by basis and grouping. Keep the additional models that deliberately change the basis, domain or by-variable structure. |
| SCASM sampling, interval, integration and shape-interval tests | Move the repeated builders into shared setup and cache their required variants. Use `m_1_smooth` where only an ordinary unconstrained GAM is required. Bootstrap models remain on demand. |
| `test-cyclic-polar.R` | Cache `cyclic_polar_model()` by cyclic basis. Its explicit fitted cycle boundaries cannot be replaced by ordinary smooths. |
| `test-surrogate-residuals.R` | Share the default ordinal fit. Requests with changed data or a supplied family remain separate fits. |
| `test-multinom-surrogate.R` | Cache by sample size, category count, missing-row flag and NA action. The observation encoding and offsets differ from the older setup multinomial model. |
| `test-response-derivatives-delta.R` | Cache `response_delta_fixture()` by family and link, retaining the simple design used for exact gradients. |
| `test-qq-plot.R`, `test-worm-plot.R` | Share the identical binomial data and model. Request the model lazily rather than fitting it while sourcing both files. |
| `test-simulate-methods.R` | Remove the duplicate `smallAges` SCAM fit; `sw` and its data already exist in setup. |

## Fits to retain

| Files or topic | Why a generic setup model does not cover the case |
| --- | --- |
| Missing-row, discrete-offset, fitting-offset and diagnostic-weight tests | The fitted omission record, response encoding, weights or offset location is what must be recovered. A complete-data model would remove the regression case. |
| Model-context, expression-wrapper, transformed-prediction and relevant parallel-worker tests | They deliberately fit functions defined in local environments, nonstandard predictor classes, stored transformations or temporary prediction methods. Reusing a model fitted in setup would change the environment being tested. |
| Parametric interaction, overall-uncertainty and model-label tests | Designs distinguish contrast codings, polynomial blocks, shared coefficients, reserved names, absent intercepts and repeated labels across predictors. Ordinary additive models cannot stand in for those structures. |
| Variance-component format and smoothing-sampler numerical references | ML, REML, fREML, known scale, fixed or shared smoothing parameters, `paraPen` and alternative parameter mappings must be fitted as requested. Some comparisons need both fits to use exactly the same observations and penalties. |
| Mixed-model covariance tests | Random slopes, nested groups, separate blocks, residual correlation and residual variance weights change the joint covariance. An existing random-intercept model would omit those contributions. |
| Surface, basis and transformed-coordinate regressions | Marginal basis order, two-variable margins, different tensor constructors, factor conditioning, prescribed boundaries and holes in the observed support can determine the result. Keep those particular designs; existing local caches already share many of them. |
| Response-family, likelihood and posterior-law tests | Nondefault links, bounds, nuisance parameters, censoring, hurdle mixtures, zero trials and family hooks are meaningful differences even when formula text is identical. |
| `test-draw-sampling-diagnostics.R` | Keep its compact four-coefficient fit with corrected covariance. Larger existing models would increase the cost of every draw and derived object, and the small interface model does not supply the same corrected covariance. |
| SCAM constructor, plateau, cyclic-margin and matrix-valued-by tests | The particular fitted basis and unrestricted parameterization are needed. Existing monotone fits do not cover all these structures. Existing cached constructor and prediction fixtures remain cached. |
| SCASM documented-recipe tests and constraint replacement tests | The specified recipe must actually fit, and equality constraints, replacement inequalities and bootstrap storage are deliberately varied. Keep these distinct from the shared basic constrained fits. |
| `test-sdmtmb-dependence.R` | These objects use `do_fit = FALSE` and then modify native parameter state. They are not repeated optimizer fits. Sharing the mutable native environment could couple tests. |
| Other native sdmTMB and glmmTMB covariance/conditioning cases | Native field, random-effect, zero-inflation, dispersion and temporal structures are not represented by the ordinary GAM setup models. Preserve fresh objects when tests mutate native state. Existing cached threshold, time-varying and assembly fixtures remain on demand. |
| Cached GJRM joint, selection, diagnostic-family and assembly fixtures | Their parent equations and family-specific parameterizations are required; a GAM component cannot replace the native GJRM fit. Keep their existing caches. |
| `fixtures/secondary-models.R` | Already fitted once on demand behind the CRAN guard. Do not move these fits into unconditional setup. |
| HGAM paper and optional `tests/integration` workflows | Keep the full documented workflows separate. Moving their model fits into setup would add work to routine CRAN checks. |

## Remaining audit candidates implemented

All five remaining groups identified by the audit now use shared setup models.
The specialized glmmTMB model is fitted only when requested, so installing an
optional backend does not add an unconditional fit to setup.

| Location | Decision |
| --- | --- |
| Three `glm(count ~ spray, ...)` calls in `test-dharma-residuals.R` | Reuse the on-demand `dharma_poisson_model` for `InsectSprays`. Family-hook edits are made to a test's local copy. The existing Gaussian `m_glm` does not supply the required Poisson GLM class and family. |
| `test-data-combos-complete.R` | Reuse `conditional_combos_fixture()` and its data. Both fixtures have the same three observed factor combinations; unequal replication is already represented by the shared fixture. |
| `test-parametric-effects-constant.R` | Use `gamm4_gaussian$gam` and its stored model frame. Select `terms = "z"` and check the selected term explicitly before drawing the numeric effect. |
| Unsupported-control checks in `test-sampling-interface.R` and `test-sampling-review.R` | Use `m_lm`; these checks reject the control arguments before needing particular covariate names or observations. |
| Native residual and visual snapshot cases in `test-glmmtmb.R` | Reuse `glmmtmb_gaussian_fixture()` with the identical complete data, smooth and random intercept. Keep fresh fits for covariance comparisons and tests that modify native state. The visual snapshot case is skipped on CRAN, so sharing that fit reduces work only when it is enabled. |

## Validation and measurement

The focused comparison uses the same 236 test cases before and after the
refactor, in a serial R session. It traces `mgcv::gam()`, `mgcv::bam()` and
`mgcv::gamm()`, including calls made inside other functions. It does not count
all native backend fitters or treat the counts as a timing estimate.

| Measurement | Before | After |
| --- | ---: | ---: |
| `gam()` calls | 353 | 267 |
| `bam()` calls | 46 | 45 |
| `gamm()` calls | 38 | 31 |
| Total traced calls | 437 | 343 |
| Elapsed seconds | 110.535 | 110.361 |
| Failing tests / warnings | 0 / 0 | 0 / 0 |

That is 94 fewer traced calls (21.5%). The elapsed times are effectively
unchanged in this comparison. Several checked operations spend their time on
posterior draws, interval calculations and rendering; reducing fits alone
does not establish a comparable reduction in CRAN elapsed time. The comparison
also predates the final caching changes for multinomial, response-derivative
and random-effect fixtures, which lie outside its selected groups, and the
five remaining audit groups implemented subsequently.

After implementing all five remaining groups, the complete CRAN selection
passed again with `make test TEST_CPUS=4`, with no failures or warnings and
201 expected skips.
`NOT_CRAN=false` skips tests explicitly reserved for the broader suite;
`GRATIA_RUN_HGAM=false` keeps the paper checks disabled. The runner installs
into a temporary library, so skipped-snapshot cleanup applies to installed
test copies rather than the repository's reference images.

The broader selection then passed with `make test-full TEST_CPUS=4`, with
visual snapshots enabled and no failures or warnings. Its only skip was the
HGAM paper suite because `GRATIA_RUN_HGAM=false`; `NOT_CRAN=true` and
`VDIFFR_RUN_TESTS=true` enabled the other tests and visual comparisons. The
existing snapshots matched without updates. The HGAM test file and all source
snapshot files remain unchanged; unused HGAM snapshots were removed only from
the temporary installed test copy.

To refresh the source inventory without running or fitting anything, run the
following command from the repository root.

```sh
Rscript --vanilla notes/test-model-fits/audit.R
```
