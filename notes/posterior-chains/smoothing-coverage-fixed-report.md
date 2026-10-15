# Partly fixed fREML dispersion: correction and coverage

The internal smoothing mixture now supports Gaussian identity, non-discrete
`bam(method = "fREML")` fits with partly fixed smoothing parameters and
estimated dispersion. All 200 previously rejected fits in the affected
coverage case now return intervals. The prototype remains internal.

## Source of the disagreement

In mgcv 1.9-4, `bam.fit()` stores the profiled Gaussian REML dispersion in
`sig2`. It calls `Sl.postproc()` with the original scale argument. When that
scale was estimated, `Sl.postproc()` uses

```
Pearson dispersion = sum(weights * (response - offset - X %*% beta)^2) /
  (number of fitting rows - sum(edf))
```

to construct `Vp`. With partly fixed smoothing parameters the two estimates
need not agree. In coverage replicate 1, reported REML dispersion is 0.2207051
and Pearson dispersion is 0.2277405. Across the 200 datasets the Pearson/REML
ratio ranges from 1.031 to 1.032. This explains the previous covariance
reconstruction failure; the coefficient mode and penalty mapping were correct.

The prototype now calculates Pearson dispersion from the retained response,
weights, offsets, coefficient predictions and effective degrees of freedom
for this fitting configuration. It accumulates residual sums of squares in
the existing model-matrix blocks, without allocating another full design.
It holds this dispersion fixed as smoothing values
change, matching the native conditional coefficient distribution at the fit.
Other configurations retain their existing dispersion calculation.

The audit records `dispersion`, `reported_dispersion`, `dispersion_source` and
`dispersion_sampled`. It retains the original `V.sp` smoothing covariance and
does not rescale it, draw dispersion or reinterpret it as a covariance
conditional on fixed dispersion. The result propagates approximate estimation
uncertainty in smoothing parameters, rather than integrating a joint Bayesian
posterior.

The coefficient and covariance reconstruction checks still apply. An arbitrary
scalar change to a stored `Vp` remains an error; the prototype does not estimate
a convenient scale from that matrix. Known-dispersion fits keep their supplied
scale. The source distinction is verified by the [independent Gaussian
calculation](smoothing-coverage-fixed-check.R) and its
[results](smoothing-coverage-fixed-check.csv).

## Repeated-simulation results

The affected case was rerun on all 200 original datasets for gam ML/REML and
non-discrete bam ML/REML/fREML: 1,000 fits and 3,000 interval sets. It retains
the original unequal precisions, fitting offsets, spline truth, seeds and
256-component integration. Every fit and every interval calculation succeeds.
All results for the four unaffected configurations are exactly identical to
the original checkpoints, excluding elapsed times. Native conditional and
unconditional intervals and smoothing estimates for bam fREML are also
unchanged.

For bam fREML, interior coverage percentages and Monte Carlo standard errors
in percentage points are:

| Quantity | Conditional | Unconditional | Mixture |
| --- | ---: | ---: | ---: |
| Linear predictor and expected response | 95.39 (0.67) | 95.39 (0.67) | 95.50 (0.65) |
| First constrained smooth | 95.17 (0.58) | 95.17 (0.58) | 95.11 (0.58) |
| Derivative of first smooth | 94.89 (0.66) | 94.89 (0.66) | 94.94 (0.66) |

The paired mixture-minus-conditional difference is 0.11 percentage points
(SE 0.11) for the linear predictor and 0.06 points (SE 0.06) for its smooth
derivative. These results support the corrected calculation in this case;
they do not change the earlier undercoverage and numerical failures for weak
smooths and rare binary outcomes.

[Summary](smoothing-coverage-fixed-summary.csv),
[paired comparisons](smoothing-coverage-fixed-paired.csv) and
[availability](smoothing-coverage-fixed-availability.csv) retain all five
configurations, both grid strata and all four quantities. The original study
summaries remain unchanged as a record of the original implementation.
Replacing only the affected case raises overall mixture availability from
15,488 to 15,688 of 16,000 attempted fits. The five native fit failures and
307 other failed smoothing components in the original study are unaffected.

The first 20 datasets were also rerun with 1,024 integration components for
all five configurations. Every calculation returned intervals; average
interior coverage did not change. The largest group-specific 95th percentile
endpoint change was 0.040% of the higher-resolution interval width.
[Resolution results](smoothing-coverage-fixed-resolution.csv) are retained.

For the first dataset, gam REML and bam fREML were checked against 10,000
actual coefficient draws for each of the three distributions. The reference
mixture uses 4,096 integration components. All six checks completed; the
largest absolute empirical CDF error was 0.0146.
[Draw-check results](smoothing-coverage-fixed-draw-check.csv) are retained.
Across all 200 bam fREML fits, the largest relative coefficient reconstruction
error is 7.6e-15 and covariance error 7.8e-15; see the
[dispersion and reconstruction results](smoothing-coverage-fixed-dispersion.csv).

## Validation and reproduction

Regression tests cover independent fixed-smoothing Gaussian predictions and
joint covariance at fitted and changed smoothing values, the distinct REML
scale, numerical smoothing derivatives, unchanged smoothing covariance,
fixed scale, weights, missing rows, call-level offsets, unavailable original
data expressions, reproducibility, caller RNG state, zero smoothing covariance
and corrupt fitted covariance. Ten unaffected fits, including shared smoothing
parameters, retain exactly the original study's setups and 100 seeded draws after
excluding the two added audit fields. Public coefficient samplers, chain
identifiers and response-generation code are unchanged.

The targeted suite passes 1,020 assertions and the standalone research tests
pass 99. The full installed-package local suite passes without failures or
warnings; 197 visual comparisons are skipped by `VDIFFR_RUN_TESTS=false`.
The pkgdown reference index builds successfully.

`R CMD check --as-cran --no-manual --run-donttest` completes with no errors or
warnings and one expected NOTE: `--run-donttest` prints five example blocks
absent from the saved ordinary-example output. Ordinary examples, optional
examples and vignette building/rebuilding pass. `--no-manual` omits the PDF
manual, and `_R_CHECK_CRAN_INCOMING_REMOTE_=false` disables remote incoming
checks. The CRAN-style run skips 198 gated tests; the separate full local run
uses `NOT_CRAN=true` to cover their non-visual assertions. Pandoc reports
deprecation warnings for pkgdown's `--katex` option during the reference build.
See the [validation record](smoothing-coverage-fixed-validation.txt) for logs,
source hashes and numerical results.

The pre-commit review reproduced all saved original and corrected summary,
paired, availability and failure CSVs from the raw checkpoints. Extra checks
of partly fixed tensor penalties, shared parameters and smoothing parameters
fixed through the bam call agree with independent native Gaussian fits in
the original coefficient coordinates. The checked package implementation and
regression tests are unchanged, so the preceding full-suite and package-check
results remain applicable. A fresh pkgdown build validates all 148 public
topics and 201 index labels, sampling aliases and local reference links.

The review pinned the seeded-draw comparison to the original study's recorded
commit, so later commits cannot silently change its baseline. The draw-check
runner now requires a fresh output directory and rejects existing results
without changing any archive files. Its rerun gives exactly the same
draw-check summary. The new raw suffix `fixed-review-draws` preserves that
rerun alongside `fixed-draws`. These changes affect research scripts only.

Use the corrected branch installed in an isolated library, and run outside
the sandbox from the repository root:

```sh
Rscript notes/posterior-chains/smoothing-coverage-run.R /path/to/fixed-corrected 200 256 4 normal_fixed
Rscript notes/posterior-chains/smoothing-coverage-summary.R /path/to/fixed-corrected /path/to/fixed-corrected/summaries
Rscript notes/posterior-chains/smoothing-coverage-run.R /path/to/fixed-resolution 20 1024 2 normal_fixed
Rscript notes/posterior-chains/smoothing-coverage-resolution.R /path/to/fixed-corrected /path/to/fixed-resolution notes/posterior-chains/smoothing-coverage-fixed-resolution.csv
Rscript notes/posterior-chains/smoothing-coverage-draws.R /path/to/fixed-draws 2 normal_fixed
Rscript notes/posterior-chains/smoothing-coverage-fixed-validation.R /path/to/original-main /path/to/fixed-corrected /path/to/fixed-draws
```

Raw checkpoints remain in `/Users/gavin/work/git/gratia/research-archives/`
under the prefix `smoothing-coverage-2026-10-15-`, with new suffixes
`fixed-corrected`, `fixed-resolution` and `fixed-draws`. Each new protocol records
source hashes and library information. The original archive is preserved.
Use mirai's `L'Ecuyer-CMRG`, `Inversion`, `Rejection` generator for direct
reproductions. The standalone validation checks both archives and rejects any
change in the unaffected results.
