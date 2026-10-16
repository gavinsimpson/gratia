# Intermittent GJRM NBII test failure

Investigation on `gls/posterior-chains`, 15 October 2026, using R 4.6.1,
GJRM 0.2.6.9 and gamlss.dist 6.1.11. All fitting, tests and package checks
run outside the sandbox.

The failure has two upstream causes and a reproducibility problem in gratia's
test fixture. Gratia's response-distribution adapter already calculates the
correct negative-binomial probabilities from the final parameters. It does not
use the broken upstream probability functions.

## Why the test varied between runs

The old family test set a seed once, then alternated response generation and
model fitting. GJRM's `gamlss()` calls `overall.svG()` to initialise the second
predictor. That function calls `set.seed(1)` and then removes `.Random.seed`
from the global environment. This happens in the first normal-family fit,
before the next response vector is generated. Later fixtures therefore depend
on a newly initialised random stream, rather than the test's seed.

Repeating the old test five times with the same explicit seed produced five
different NBII fits. A direct normal-family fit confirms that the seed has
been removed, without loading gratia. This explains why a full package check
could fail while an isolated rerun passed.

## Why some NBII fits warn and contain incorrect probabilities

NBII has mean `mu`, variance `mu * (1 + sigma)` and negative-binomial size
`mu / sigma`. Its Poisson limit is `sigma -> 0`.

In gamlss.dist 6.1.11, `dNBII()` contains this assignment:

```r
fy[sigma <= 1e-4] <- dpois(x, lambda = mu, log = log)
```

Only the left side is subsetted. With varying dispersion, the full right side
is recycled into the selected rows. The reported probabilities can therefore
belong to different observations. This also produces replacement-length warnings
in GJRM's likelihood and numerical derivatives. A three-observation call to
GJRM's own likelihood evaluator gives a probability of 0.1353353 where its
negative-binomial mass is 0.3678978, before any gratia code is involved.

`pNBII()` has the same indexing error and additionally switches to Poisson for
`sigma > 1e-4`, the wrong end of the dispersion range. These are the problems
reported in [Joran's issue #10](https://github.com/gamlss-dev/gamlss.dist/issues/10),
[his PR #11](https://github.com/gamlss-dev/gamlss.dist/pull/11) and
[Gavin's issue #12](https://github.com/gamlss-dev/gamlss.dist/issues/12).
All three were open when checked. Achim's comments recommend removing the
negative-binomial fallbacks, since base R already handles the Poisson limit.

A deterministic GJRM boundary fit produces 260 replacement-length warnings during
optimisation. Its final density happens to agree with the analytic mass; a final
agreement alone does not imply that earlier likelihood evaluations were correct.
The small direct evaluator example above demonstrates the actual corruption.

## Changes in gratia

The family test now generates every response vector before fitting. Each fit
preserves the caller's RNG state, and assertions require both silent fitting
and restoration of that state. The original seven families, formulas,
generating distributions, analytic checks and tolerances are retained. A comparison
of the parsed old and new test files confirms that all 28 original assertion
expressions remain unchanged. The fixed NBII fixture has fitted dispersion between 0.11686461 and 1.0647029 and its stored
likelihood masses agree exactly with the analytic negative-binomial masses.

A separate test supplies known final parameters for NBI and NBII, including
mixed dispersion values above, at and below the upstream cutoff, down to `1e-8`.
It deliberately supplies incorrect stored Poisson probabilities. It checks
means, variances, both sides of the discrete CDF, upper log tails, quantiles,
randomised residuals, seeded simulations and caller RNG state against base R.
This covers gratia's behaviour near the boundary without asking a broken
upstream optimiser to construct the fixture.

The new full-vector CDF-difference identity uses the standard `1e-8` numerical
tolerance: at sizes as large as `5e8`, base R's own CDF subtraction and mass
can differ by about `1.5e-10`. A separate identity for probability mass at zero uses `1e-12`.
The original fitted-density assertion retains its `1e-6` tolerance unchanged.
No test is skipped, no fitting warning is suppressed in the regression tests,
and no installed dependency or production sampler is patched.

This fixes the intermittent gratia test. External GJRM fits that reach the broken
upstream likelihood branch still require corrected upstream distribution
routines; recalculating diagnostic probabilities cannot repair those fits.

## Reproduction and validation

Run from the repository root:

```sh
Rscript notes/posterior-chains/gjrm-nbii-investigation.R
```

The script records upstream warnings for investigation, without loading gratia
or changing namespace bindings. Its output is in
[gjrm-nbii-investigation.txt](gjrm-nbii-investigation.txt).

The affected `scalable-gjrm`, `diagnostic-posterior` and `dharma-residuals` tests
pass: 569 assertions, no warnings, failures or skips. The GJRM file accounts for
182 assertions. Three repeats under each of Mersenne-Twister and L'Ecuyer-CMRG
produce identical response fixtures and NBII fits; all original family
assertions pass in every repeat.

The full package check completed with **Status: OK**. Its full test suite
has 18,761 passes, zero failures, zero warnings and 197 skipped visual
comparisons. Installation, namespace checks, documentation checks, ordinary
and `--run-donttest` examples, and vignette checks and rebuilding all pass.
This resolves the GJRM failure recorded in the earlier
[MCMC plot review](mcmc-plots-review-validation.txt).

Chosen validation options:

* `R CMD build` built the vignettes. `R CMD check --as-cran --no-manual`
  omitted the PDF reference manual; it did not disable tests.
* `NOT_CRAN=true` enabled functional tests normally skipped on CRAN.
* `VDIFFR_RUN_TESTS=false` skipped 197 visual snapshot comparisons. Functional
  plot construction and rendering checks still ran.
* `_R_CHECK_CRAN_INCOMING_REMOTE_=false` disabled remote incoming checks.

These options produced no package-check warnings, notes or errors. The omitted
manual, visual comparisons and remote incoming checks were not validated.

`pkgdown::check_pkgdown()` validates the configuration and reference coverage.
It passed, with existing nonfatal Pandoc warnings about the deprecated
`--katex` option supplied by pkgdown. No help topic or reference entry changed.

The package inputs match the frozen check inputs. Existing modified and
untracked smoothing-reference work retains its original hashes. Changes are
limited to the GJRM test and these internal investigation files; no production
code, public documentation or release notes changed.

Validation logs:

* `/private/tmp/gratia-nbii-affected.log`
* `/private/tmp/gratia-nbii-repeat-fixed.log`
* `/private/tmp/gratia-nbii-build.log`
* `/private/tmp/gratia-nbii-check.log`
* `/private/tmp/gratia-nbii-check/gratia.Rcheck/tests/test-all.Rout`
* `/private/tmp/gratia-nbii-pkgdown.log`

## Review before committing, 16 October 2026

No regressions or unintended package edits were found. All 28 original
assertion expressions remain unchanged. The only review edits clarify the
investigation descriptions and remove trailing whitespace from its transcript.

The affected tests were rerun outside the sandbox: 569 passes, no failures,
warnings or skips. The investigation script reproduces its saved results
exactly after ignoring console line-end spaces.

Built all reference pages, the reference index and the sampling migration
article. Validated 149 public topics and 203 index labels; every topic and
local reference link resolves. No public topic was added. Reference examples
used `examples = FALSE` because both ordinary and donttest examples passed in
the package check. Pandoc reports nonfatal deprecation warnings for the
`--katex` and `--mathjax` options supplied by pkgdown.

The R source, tests, documentation and other package inputs still match the
successful full-check inputs recorded above, so the full suite and package
check were not repeated during review. The targeted run regenerated the
ignored `tests/testthat/Rplots.pdf`; this file is excluded by `.Rbuildignore`
and is not part of the package or commit.

Existing smoothing-uncertainty files retain their original hashes. The commit
includes only the GJRM test and the three investigation files.

Review logs:

* `/private/tmp/gratia-nbii-review-tests.log`
* `/private/tmp/gratia-nbii-review-reference.log`
* `/private/tmp/gratia-nbii-review-investigation.txt`
