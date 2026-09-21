# CRAN test and model-fixture review

Review date: 2026-09-21. Implemented on `codex/cran-test-review`. The original audit below is retained as historical context; this status section supersedes its proposed treatments.

## Implemented split

- CRAN retains the numerical regression suite and all eight family-quantile integrations. Tweedie integration fixtures use 12 observations.
- The three large drawing files, AR drawing examples and extended soap-film examples are secondary. Nine small tests in `test-draw-core.R` preserve representative numerical and plot-build coverage on CRAN.
- Selected 4D, ordered-factor, tensor random-effect, distributed-lag, Box–Cox gamma, AR, bivariate factor and extended soap-film models load on demand through a guarded, cached fixture environment. The bivariate factor data remain in core setup because `data_slice()` tests need them.
- Removed unused and overwritten fits, reused `m_gaulss` for two structural checks previously using `m_accel`, and reduced the classification-only Duchon fixture.
- CI explicitly runs the broader numerical suite and a separate CRAN-core check. A dedicated macOS job runs visual comparisons; paper-sized HGAM tests run locally and in weekly/manual CI.
- Fixed five existing variance-component assertions that checked an unrelated `df` instead of the computed `vc` result. Removing the unrelated global data exposed this mistake.
- No production R code or image baselines changed. The drawing message snapshot only changes its displayed fixture expression.

See `tests/README.md` for commands and fixture conventions.

### Validation

- CRAN mode: **4,347 passing expectations, zero failures/errors/warnings**, 166 skipped records; **49.840 seconds elapsed** (44.940 user, 2.912 system).
- Ordinary CI mode with image comparisons disabled: **4,743 passing expectations, zero failures/errors/warnings**, 179 skipped records; **76.304 seconds elapsed**.
- The CRAN suite is about **61% faster** than the original 128.645-second run. These are single-machine observations, not a CRAN runtime guarantee; final validation processes ran concurrently. The original sandbox-limited baseline also omitted parallel assertions.
- `R CMD check --no-manual`: **zero errors, zero warnings, one NOTE** for differences from saved example output. Examples, installed-package tests and vignette rebuilding all passed. No production code or example sources were changed.
- Visual CI mode on this macOS machine: **4,983 passing expectations, zero failures/errors/warnings**, 12 skipped records. All enabled SVG comparisons passed without updating image baselines. The remote GitHub Actions jobs have not yet run.
- Weekly HGAM mode: all 13 model cases executed with zero failures/errors/warnings; nine numerical expectations passed and 12 image comparisons skipped as configured. The two slowest test bodies were bird-movement model 2 (**91.225 seconds**) and the complex `gfam` bird-movement model (**85.141 seconds**). These local times are not CI predictions. The workflow now publishes per-test timings and a CSV artifact retained for 90 days.

Keep a broad, inexpensive numerical core on CRAN. Move costly integration matrices, visual permutations and large model/grid examples to the secondary suite. Do not blanket-skip the new regression files: most are fast and directly protect recent bug fixes.

CRAN permits long-running tests to be optional, while asking that retained checks exercise all package features. Its policy also limits concurrent threads/cores to two. There is no universal test-suite rejection threshold stated there. [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html).

## Applied quantile-test optimisation

The final family-quantile integration test now uses 12 observations for each Tweedie fit, while retaining the original 120-observation fixtures for the other six families. All eight families, all applicable residual types, both public plot entry points and fitted Tweedie parameter checks remain on CRAN. Unasserted standard-family residual-reference calculations were removed, and the general-family reference quantiles are computed once per model instead of once per residual type. Direct distribution, endpoint and tail tests are unchanged.

Validation with `NOT_CRAN=false`: all **189 expectations across six tests passed**. The formerly slow test retained all **78 assertions** and took **3.624s**, compared with the baseline **49.638s** (about **93% less time**). The sum of this file's test-body times dropped from **50.991s to 4.943s**. The complete targeted runner took 16.693s including shared setup/runner overhead. These are single runs on the same machine, not a full-suite remeasurement.

This supersedes the original recommendation to move the two Tweedie integration cases off CRAN. The next priority is the visual test matrix and unnecessary fixtures. No production code changed.

## Original baseline measurements

One local run with `NOT_CRAN=false`, `VDIFFR_RUN_TESTS=false`, R 4.6.1 on Apple Silicon/macOS with Accelerate BLAS:

- Whole `testthat::test_local()` call: **128.645 seconds elapsed**, 122.670 user CPU seconds and 4.488 system CPU seconds.
- Sum of recorded test-body times: **111.454 seconds**. These do not include shared setup, top-level file code, package loading or all runner overhead.
- Separately timing each expression in `setup.R`: **11.440 seconds elapsed** in total. This is a separate diagnostic run, not an additive component measured inside the first run.
- **4,324 passing expectations, zero failed expectations, one error, 289 skipped test records.** The error was `mirai::daemons()` failing to open a socket in this sandbox; the three parallel tests consequently did not run. This is not a clean full-suite pass.
- A skipped record can contain passing assertions and substantial computation before its skip. “Skipped” does not mean “free”.
- These are single-machine measurements, not CRAN timings or a full `R CMD check`; examples and vignettes need a separate budget.

| Cost centre | Elapsed seconds | Recommendation |
|---|---:|---|
| `family-quantiles`: “uniform QQ and worm plots use the extended quantile helpers” | 49.638 | Now retained on CRAN with the smaller fixtures described above |
| `draw-methods` | 10.764 | Retain small functional checks; skip visual permutations before constructing plots |
| `draw-gam` | 8.606 | Retain small drawing smoke tests; secondary model/style matrix |
| `draw-smooth-estimates` | 4.249 | Secondary high-dimensional/visual variants |
| `missing-rows` | 3.559 | Keep: substantial correctness coverage for modest cost |
| `fix-family`: “fix family funs work for tw” | 2.643 | Currently computes before a snapshot skip; move/split this roundtrip example |
| `transformed-smooths` (whole file) | 0.846 | Keep, including the cross-fitter matrix |
| `transformed-model-variants` (whole file) | 0.714 | Keep; no timing case for excluding all specialised models |
| `confint-simultaneous-uncertainty` | 0.183 | Keep the independent statistical reference |

A targeted rerun of that final family-quantile test isolated **24.238s for `mgcv::tw()`** and **24.310s for `mgcv::Tweedie(p = 1.5)`**. The other six family iterations together took about 0.41s. `Rprof()` attributed **98.5% of sampled time to `tweedie::qtweedie()`**, mostly through `ptweedie()` / `ptweedie_inversion()` during numerical inversion. This is quantile evaluation, not `rtw()` or `rtwlss()`; no `twlss()` model is in this test. Its initial Tweedie responses use `mgcv::rTweedie()`. These targeted timings include profiler overhead and are a separate run.

The 49.6-second integration test alone is about 39% of the whole run. The three drawing files add 23.6 seconds. This is a much stronger first target than removing fast mathematical regressions. Those figures are an upper bound on removable cost: replacement smoke tests still cost time. No reduced-suite timing is claimed.

## Shared models suitable for secondary tests

References below were checked against parsed test expressions and source searches. Move dependent data preparation with the fits. A test-body skip cannot prevent fitting in `setup.R`.

| Models | Current use | Proposed treatment |
|---|---|---|
| `su_m_quadvar`, `su_m_quadvar_te`, `su_m_quadvar_t2` | Only `draw-gam` / `draw-smooth-estimates` variants | Strong secondary candidates. Keep a tiny 4D smoke case if this is otherwise the only 4D evaluation coverage. Fits alone total about 1.00s here; avoiding plot grids saves more. |
| `m_re_interaction`, `m_re_two_interaction` | Only `draw-methods`, issue #358 | Secondary full examples (about 0.97s fitting). Extract a small supported-path build test and unsupported-path assertion first. |
| `m_ordered_by` | Only plotting/grouped-by/label variants | Secondary CO2 example; keep a small ordered-factor label/grouping assertion. Numerical ordered-factor coverage already exists in transformed-model-variants. |
| `dlnm_m` and `su_dlnm` | Only one `draw-gam` example | Secondary full distributed-lag example. Keep a small matrix-covariate drawing smoke test; transformed-model-variants already checks the summation convention numerically. |
| `m_ar1`, `m_ar1_by` and AR simulation/covariance setup | Only `bam-ar` and `draw-gam` | Secondary full single-series/factor-by AR examples. Retain one cheap AR plotting/build smoke test on CRAN. |
| `su_m_bivar_by_fac` and doubled `su_eg2_by` | Only bivariate `difference-smooths` plotting variants | Secondary 4,000-row example; numerical alignment is covered in difference-smooths-alignment. Preserve small bivariate difference coverage. |
| `m_soap_bndry`, `m_soap_nested`, corresponding boundaries/knots/data | Only `soap-films` | Secondary known/nested-boundary integration examples. Keep `m_soap` or a smaller standard soap fixture for core boundary/clipping coverage. All current soap tests do some work before skipping. |
| `m_bcg` / `sim_bcg()` | One simulation failure test, already CRAN-skipped at its start | Fit only after the secondary skip. No current CRAN assertion requires this fixture. |
| `m_betar`, `m_gammals`, `m_gumbls` | Only fitted roundtrip examples in `fix-family` | Secondary full fitted examples; core can use family constructors/explicit parameters and small fitted-parameter checks. The current first snapshot aborts these tests before their later numerical assertion on CRAN. |
| `m_gevlss` | One numerical fitted-family roundtrip in `fix-family` | Secondary large fit only after retaining a small fitted-parameter case. Direct GEV support/tail/shape checks stay in family-quantiles. |
| `su_m_trivar`, `su_m_trivar_te`, `su_m_trivar_t2`; file-local `m_t2` in smooth-estimates | Large-grid evaluation, plotting, dimension/type helpers | Secondary original 1,000-row / 10,000-grid examples. Keep small 3D tensor/dimension/deprecation checks; do not simply omit fixtures while leaving references active. |
| `m_sz_2f` | Two-factor sz evaluation and drawing | Secondary exhaustive example; retain small sz coverage and a two-factor smoke case. The fit is not a dominant hotspot here. |

The first three specialised plotting pairs/groups are useful separations even where their fit is quick: they stop CRAN from spending time on large grids and repeated drawing operations.

## Fixtures to remove or simplify rather than merely skip

- **Unused:** `su_m_factor_by_re_decomp`, `rm_gamm4`, `i_sz`, `i_fs` have no references outside setup in the test sources. Their four fits cost about **1.29s** in the setup profile. Remove them unless intended coverage is added; keeping them only on CI would still test nothing.
- **Overwritten:** the first `su_m_factor_by_re` fit at setup line 94 is overwritten at line 104 before tests execute. The second is the one used by the #285 difference-smooths regression. Remove the first fit, not the regression.
- **Over-specified for their assertions:** `m_accel` is used only for `model_constant()` length and `n_eta()`. Both can use the existing two-predictor `m_gaulss`. If adaptive smooth behaviour needs coverage, give it its own secondary numerical test.
- **Largest individual setup fit:** `su_m_bivar_ds` took 0.96s but is used only by smooth-type/isotropic checks. Keep the Duchon feature in core using a much smaller fit or a constructed smooth object.
- **Tiny extraction-only fixtures:** `su_m_bivar_ti` and some basis variants are used for classification. Prefer small constructed smooths where fitting adds no coverage.

Do **not** immediately move `m_twlss`, `m_mvn`, `m_multinom`, `m_ziplss`, `m_gaulss`, all `gamm4` models, or all `scam` models off CRAN. They have active numerical prediction, parameter-ordering, interval, multivariate or adapter assertions. `m_twlss`, for example, protects a distinct three-parameter interval transformation. Smaller replacements are appropriate; removing every representative of these paths is not.

A useful core fixture set retains a small ordinary GAM, GAMM, BAM, gamm4 and scam; numeric/factor-by and random-effect smooths; univariate and bivariate tensor smooths; parametric/factor/offset models; Gaussian, count and a representative multi-parameter/multivariate model. Many newer tests already construct suitably small local fixtures. Keep their self-contained data and explicit seeds.

## File-by-file split

There are two execution tiers. “Split” below means the existing file contains tests for both tiers, not a third execution mode. “Core” refers to functional assertions: printed/image snapshots remain secondary. Times are current CRAN-mode test-body totals, not complete file runtimes. Near-zero values can indicate existing skips, not fast secondary tests.

| File (`tests/testthat/test-….R`) | Proposed allocation | Current seconds | Rationale / retained coverage |
|---|---|---:|---|
| `add-functions` | Split | 1.054 | Keep numeric add_* contracts; secondary sample-output snapshots, with skips before sample generation. |
| `assemble-parametric-selection` | Core | 0.478 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `assemble` | Secondary | 0.005 | Printed assembly snapshots; retain functional coverage in assemble-parametric-selection. |
| `bam-ar` | Secondary | 0.342 | AR(1) plotting variants; keep one small AR(1) construction/build smoke test on CRAN before moving both fixtures. |
| `basis-size` | Core | 0.012 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `basis` | Split | 0.745 | Keep numerical basis, selection and argument checks; secondary rendering/printed snapshots. Restore cheap argument checks currently CRAN-skipped. |
| `by-variables` | Split | 0.437 | Keep by-variable identification; secondary repeated draw variants. |
| `censored-families` | Core | 0.341 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `compare-smooths-unconditional` | Core | 0.314 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `compare-smooths` | Split | 0.822 | Keep numerical comparison and selection; secondary visual comparisons. |
| `concurvity` | Split | 0.451 | Keep numeric extraction; secondary plot snapshots. |
| `conditional-values` | Split | 0.248 | Keep numerical condition/grid and summary-function checks; separate printed/visual snapshots and remove CI exclusions from numeric assertions. |
| `confint-methods` | Split | 0.960 | Keep pointwise, small simultaneous and endpoint/selection regressions; secondary legacy platform-sensitive fderiv example. |
| `confint-simultaneous-uncertainty` | Core | 0.183 | Independent covariance/critical-value reference; 500 draws on 15 rows, inexpensive. |
| `data-combos-complete` | Core | 0.034 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `data-sim-ocat` | Core | 0.113 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `data-sim` | Split | 0.032 | Keep data shape, values and RNG contracts; secondary reference-output snapshots and broad gfam example. |
| `data-slice` | Split | 0.258 | Keep all data/selection/offset contracts; secondary formatting snapshots only. |
| `datagen` | Split | 0.087 | Keep deprecation/validation and small 1D/2D checks; secondary snapshot and large 3D fixture (replace with a small 3D rejection case). |
| `derivative-step` | Core | 1.109 | Analytical derivative accuracy, scaling and numerical stability; keep. |
| `derivatives-numeric-by` | Core | 0.429 | Independent finite-difference reference, zero/scaled multipliers and both orders; inexpensive. |
| `derivatives` | Split | 1.133 | Keep orders, stencils, numeric/factor by, selections and a small simultaneous check; secondary visual regressions and redundant large grids. |
| `diagnostic-plots` | Split | 1.947 | Keep argument validation and small appraise/QQ/worm construction/build checks; secondary family x method visual matrix. |
| `diagnostic-weights` | Core | 0.104 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `difference-smooths-alignment` | Core | 0.703 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `difference-smooths` | Split | 1.368 | Keep numerical differences, selection and validation; secondary repeated GAM/GAMM/gamm4/BAM and bivariate plotting variants. |
| `distribution-helpers` | Core | 0.039 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `draw-gam` | Split | 8.606 | Keep a small 1D/2D/factor-by/parametric construction/build smoke set and error contracts; secondary full visual matrix. |
| `draw-methods` | Split | 10.764 | Keep bad-select, partial-match, parametric contracts and representative draw-method build checks; secondary styling, rotation, contour and repeated model variants. |
| `draw-parametric-effects` | Split | 0.892 | Keep selected terms and factor-level regression assertions; secondary visual variants and downloaded rats example. Prefer a bundled small fixture for CI. |
| `draw-smooth-estimates` | Split | 4.249 | Keep one small draw/build regression per distinct path (including grouped_by and change indicators); secondary large 3D/4D, CRS and visual combinations. |
| `edf` | Split | 0.171 | Keep numeric EDF/penalty/selection checks; secondary printed snapshots. |
| `effects` | Core | 0.030 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `evaluate-parametric-terms` | Core | 0.030 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `evenly` | Core | 0.045 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `family-quantiles` | Core | 50.991 | Keep all six tests. Applied smaller Tweedie fits and removal of redundant reference work reduces this file to 4.943s of test-body time. |
| `family-utils` | Core | 1.137 | Large file but cheap family-object and method checks; retain. |
| `fderiv` | Split | 0.041 | Keep deprecation, errors and a small legacy-path smoke test; secondary output snapshots. |
| `fitted-values` | Split | 0.868 | Keep numeric predictions, row restoration, multi-parameter intervals and ordering; secondary printed snapshots. Smaller special-family fixtures are possible, but do not remove their numerical coverage. |
| `fix-family` | Split | 2.776 | Keep null/dispatch, analytic CDF/quantile and fitted-parameter contracts; secondary printed snapshots and fitted-family roundtrip matrix. Move assertions before snapshot skips or split tests. |
| `fixed-effects-smooths` | Core | 0.128 | Independent summary.gam reference and coefficient-index regression; retain. |
| `gamm-forwarding` | Core | 0.314 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `gfam` | Core | 0.212 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `gjrm-methods` | Secondary | 0.002 | Full GJRM plotting example; retain the small transformed GJRM adapter regression. |
| `hgam-paper` | Secondary | 0.017 | Paper-sized CO2, bird movement and zooplankton examples; already CRAN-skipped. Remove CI exclusions only in a suitably provisioned integration job. |
| `lp-matrix` | Core | 0.069 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `missing-rows` | Core | 3.559 | Keep all for now: only 3.56s for broad alignment, weights, RNG and NA correctness. If needed, trim repeated ggplot_build combinations, not row contracts. |
| `model-context` | Core | 0.202 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `model-utilities` | Core | 0.031 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `overview-parametric` | Core | 0.162 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `overview` | Split | 0.135 | Extract basic class/content/selection assertions for CRAN; secondary print snapshots. |
| `parallel` | Secondary | unmeasured¹ | Daemon startup and parallel equivalence/environment forwarding; skip before starting daemons. Keep serial paths in core. |
| `parametric-effects-constant` | Core | 0.253 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `parametric-effects` | Split | 0.166 | Keep numeric and factor-level contracts, including #284; secondary visual regression. |
| `partial-derivatives-focal` | Core | 0.153 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `partial-derivatives` | Split | 0.201 | Keep focal/selection, single-point, exclusion and small factor-by checks; secondary complex top-level random-effect tensor fixture and snapshots. Move that fixture behind the skip. |
| `penalty-geoms` | Core | 0.595 | Numerical geometry/scale assertions, not image snapshots; retain. |
| `penalty` | Split | 0.091 | Keep penalty extraction/numeric properties; secondary visual matrix. |
| `posterior-samples` | Split | 0.644 | Keep low-draw shape, RNG, offset and prediction contracts; secondary printed snapshots and larger multi-family examples. Avoid default large draws for seed-only checks. |
| `qq-plot` | Split | 1.153 | Keep validation plus a small test for each algorithm; secondary residual-type/visual combinations. |
| `quantile-residuals-missing` | Core | 0.155 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `quantile-residuals` | Split | 0.069 | Keep numerical ziplss mass/seed tests and unsupported-family error; secondary numeric-output snapshots. |
| `residual-tails` | Core | 0.217 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `residuals` | Core | 0.240 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `response-derivatives` | Secondary | 0.004 | Platform-sensitive numerical snapshots; retain analytical response-derivative coverage in derivative-step. |
| `rfoo` | Secondary | 0.008 | Tweedie RNG snapshots; retain numerical contracts in rtw-vector and residual-tails. Extract cheap argument-error assertions from snapshots. |
| `rootograms-gaussian` | Core | 0.142 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `rootograms-labels` | Core | 0.479 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `rootograms` | Split | 0.099 | Keep one Poisson numeric/build smoke test and unsupported-family error; secondary visual variants (Gaussian regressions are in rootograms-gaussian). |
| `rtw-vector` | Core | 0.208 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `samplers` | Core | 0.616 | Keep Gaussian and small MH tests: all 12 together cost only 0.62s. |
| `scales` | Secondary | 0.096 | Visual colour-scale snapshot; retain a cheap scale construction assertion in core. |
| `scam-methods` | Core | 0.131 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `setup-data-recovery` | Core | 0.210 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `simulate-methods` | Split | 0.163 | Keep simulation/RNG/shape/error contracts; secondary larger multivariate examples, preserving a small multivariate smoke case. |
| `smooth-coefs` | Split | 0.096 | Keep coefficient and method contracts; secondary printed snapshot. |
| `smooth-estimates-discrete` | Core | 0.334 | Keep all for now, including diamonds reproducer: measured cost is small. A smaller reproducer is optional, not a priority. |
| `smooth-estimates` | Split | 0.838 | Keep small 1D, 2D tensor, by-factor, random-effect and adapter contracts; secondary 10,000-row 3D grids and exhaustive two-factor sz integration. Keep a small 3D/sz assertion before moving fixtures. |
| `smooth-type` | Core | 0.105 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `soap-films` | Split | 0.596 | Keep a small numeric boundary/clipping and draw/build smoke case; secondary 100x100 grids, nested/known-boundary variants and snapshots. Existing skips occur AFTER expensive work. |
| `subsetting` | Core | 0.038 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `transformed-derivatives` | Core | 0.115 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `transformed-model-variants` | Core | 0.714 | Keep all for now: only 0.71s, including small specialised adapter regressions. |
| `transformed-prediction` | Core | 0.195 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `transformed-smooths` | Core | 0.846 | Keep all for now: the cross-fitter numerical matrix is only 0.85s. |
| `utilities` | Split | 0.670 | Keep cheap numerical/type/extraction tests; secondary RNG/print snapshots. Replace m_accel with m_gaulss for n_eta and keep an inexpensive Duchon classification fixture. |
| `variance-components-formats` | Core | 0.235 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `variance-components` | Secondary | 0.044 | Printed output snapshots; retain numerical/return-format checks in variance-components-formats. |
| `worm-plot-seed` | Core | 0.190 | Retain focused numerical, validation, selection or regression contracts; no material runtime hotspot found. |
| `worm-plot` | Split | 1.166 | Keep validation plus a small test for each algorithm; secondary residual-type/visual combinations. |

¹ The parallel file failed at daemon setup before its test bodies ran.

## Applying the split safely

1. Use `testthat::skip_on_cran()` at the **start** of secondary tests, before fitting, data generation, sampling or plotting. For wholly secondary files, put it at file scope before any setup. [testthat skip documentation](https://testthat.r-lib.org/reference/skip.html).
2. Move secondary fixtures from unconditional `setup.R` into their secondary files or cached fixture functions called after the skip. Merely moving them to another `setup-*.R` file does not avoid evaluation. Preserve explicit seeds, and keep shared dependencies available to every core consumer.
3. Separate numeric assertions from `expect_snapshot()` / `expect_doppelganger()`. A snapshot skip aborts the rest of the enclosing test, so useful later expectations can currently go untested on CRAN. `fix-family` is a concrete example. Keep actual `ggplot_build()` checks for a compact drawing smoke set because plot construction alone does not exercise all rendering failures.
4. Explicitly set `NOT_CRAN: "true"` in ordinary R-CMD-check and coverage CI jobs, and use `NOT_CRAN=false` for a separate CRAN-core check. Standard testthat/devtools workflows enable non-CRAN tests locally; direct `R CMD check` needs `NOT_CRAN=true` to run the secondary tier. Do not change the variable inside package tests.
5. Existing CI does **not** run all snapshots: both checked workflows set `VDIFFR_RUN_TESTS: "false"`. Several tests also explicitly call `skip_on_ci()` (HGAM, response derivatives, conditional values, GJRM, among others). A secondary CI promise requires addressing these exclusions. Keep numerical secondary checks on ordinary CI, and establish a stable macOS visual job for visual checks; retain platform restrictions only where justified. Move numerical assertions before visual-only skips, or split them into separate tests. The HGAM examples can use a dedicated scheduled/manual integration workflow if too large for every PR.
6. `test-gjrm-methods.R` loads GJRM at file scope before checking availability or CRAN status. Move that load after the guards. `test-parallel.R` needs its guard before `mirai::daemons()`; retain reliable worker cleanup.
7. Re-profile both tiers after changes, and run the built package with `R CMD check` in CRAN mode. Targeting comfortably below a minute locally is a useful engineering goal, not a CRAN acceptance guarantee. Measure examples and vignettes separately.

The quantile integration cost has now been reduced without skips. Next prioritise early visual skips with retained drawing smoke coverage and unused/secondary fixtures. Keep the inexpensive recent regression suites. Reassess further pruning after measuring the resulting full suite.
