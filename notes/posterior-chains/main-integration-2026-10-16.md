# Integrating current main with posterior sampling

Integration on 16 October 2026, starting from posterior branch commit
`018912cb3` and main commit `89fcdd84b`. Main contributed 13 newer commits.
All fitting, tests and package checks ran outside the sandbox.

## Merge and scope

The merge retains main's development version 0.11.2-52, new features,
optional bayesplot/loo registrations, documentation topics and CI workflows.
It also retains Rcpp, posterior and coda dependencies and the posterior
sampling interfaces from this branch. Four files required conflict resolution:
DESCRIPTION, NEWS.md, the saved example output and the GJRM tests.

Both GJRM checks are retained: the actual fitted NBII masses remain compared
with independent negative-binomial masses, and main's separate moderate-
dispersion parameterisation test is included. The seeded fixtures and
mixed-dispersion boundary tests from the reviewed fix remain intact.

The likelihood and external diagnostic wrappers now use `n_draws` for the total
retained draws. Likelihood and LOO/WAIC wrappers retain their 4,000-draw default;
bayesplot retains 50 draws for ordinary checks and 4,000 for LOO plots.
Supplied coefficient draws determine their own count and chains. Explicit
counts must agree. The obsolete `n` and inactive sampler controls produce
errors. Existing coefficient and `*_samples()` generators still require an
explicit draw count when generating samples.

`log_lik()` accepts chain and diagnostic controls directly. LOO, WAIC and
bayesplot accept sampler controls through `sampling_args`, separately from
arguments for the external calculations and plots. Each MH backend and its
appropriate controls are supported. Defaults for a different sampler are not
forwarded into user-draw or MH calls.

## Chain identities and diagnostics

Likelihood matrices retain their source sampling metadata, including draw,
chain and iteration identities. `as_mcmc_draws()` converts their observation-
specific log densities for external diagnostics. LOO and WAIC results and
bayesplot objects also retain coefficient sampling metadata where available.

LOO estimates relative efficiency using the stored chains. If rows were
reordered, it restores temporal order only for the efficiency calculation.
The log likelihoods, PSIS weights and response draws remain in the same
original row order. Explicit chain IDs must agree with stored identities.
An unlabelled user matrix still requires explicit chain IDs or relative
efficiency; Gaussian-generated draws continue to use relative efficiency one.

Bayesplot checks accept supplied matrices with non-sequential draw IDs and
preserve their row order. LOO response draws use the exact coefficient matrix
used for the weights. Once those coefficients have been sampled, response
simulation receives user-draw controls rather than inactive MH or Gaussian
controls. Multivariate-normal checks use each draw's joint precision matrix.

Two integration problems were corrected during testing and review:

* Main's bayesplot code applied a Gumbel mean correction already supplied by
  this branch's prediction adapter. The duplicate correction was removed;
  explicit custom family means are still retained.
* The diagnostics accessor treated every numeric matrix as coefficients.
  Likelihood matrices now identify their quantity, so native plots use retained
  coefficient draws when `diagnostics = TRUE`, rather than plotting log
  densities as coefficients. Without retained coefficients the native plot
  gives the same informative error as other transformed sampling outputs.
  `as_mcmc_draws()` continues to convert the log densities themselves.

Tests exercise independent Gaussian densities, generated and supplied draws,
all MH backends, serial/parallel agreement, chain conversion, coefficient
summaries and plots, reversed iteration order, non-sequential draw IDs,
LOO weights and efficiency, WAIC, bayesplot responses and expected values,
and migration and inactive-control errors. Existing family likelihood and
external integration assertions are retained; only obsolete call arguments
and their matching error text were migrated.

## Validation completed before the full check

The first integrated focused run passed 627 assertions without failures,
warnings or skips. Review then found the likelihood-matrix plotting issue.
After that correction, 682 focused assertions passed, including the existing
native MCMC plots and chain-conversion tests. The new integration test file
contains 45 passing assertions.

All 14 optional bayesplot/loo/rstantools availability and loading-order
scenarios pass on the final source. No optional package was made mandatory.

The independent refit validation covers Gaussian, Poisson and binomial models.
Its pointwise RMSE against refits holding smoothing and dispersion fixed is
0.00233, 0.00303 and 0.00751 respectively; maximum Pareto-k is at most 0.258.
Refits that re-estimate smoothing and dispersion are recorded separately.

The three difficult benchmark targets and all nine seeded MH results agree
exactly with the pre-integration installation, including coefficients,
smoothing parameters, response data, weights, offsets, design matrices,
fitted covariance, draw identities and acceptance information. The existing
longer benchmarks therefore remain applicable; their timing runs were not
repeated for this interface integration.

Pkgdown builds all reference pages, the reference index, the sampling migration
article and the bayesplot/LOO article. All 157 public topics and 218 index labels
resolve, as do local reference links. The bayesplot/LOO article evaluates its
examples. Reference-page examples use `examples = FALSE` because the package
check executes ordinary and donttest examples. Pandoc emits nonfatal
warnings for the deprecated `--katex` and `--mathjax` options supplied by
pkgdown. Documentation regeneration also reports pre-existing internal
plot-method export notices; unrelated regenerated help edits were excluded.

## Full check and preservation

The final full package check completed with **Status: OK**: 21,341 passes,
zero failures, zero warnings and zero skips. Visual comparisons ran. Namespace,
compiled-code and documentation checks, ordinary and donttest examples,
and vignette checks and rebuilding all passed. The first integrated snapshot
also completed with Status: OK and 21,337 passes before the final diagnostics
correction and its four additional assertions. The final check uses
`R CMD build` and `R CMD check --as-cran --no-manual`, with `NOT_CRAN=true`
and `VDIFFR_RUN_TESTS=true`: functional tests and visual comparisons are enabled.
The PDF reference manual is omitted by `--no-manual`; remote incoming checks
are disabled by `_R_CHECK_CRAN_INCOMING_REMOTE_=false`.

The pre-existing smoothing research consists of 29 files. Their hashes and
copies were saved before merging. Only the two tracked note edits were
stashed; the untracked scripts and results were left intact. None of this
research is part of the integration changes or public API. Both tracked note
edits were restored from the backup, and all 29 research files match their
original hashes. Their modifications and untracked files remain uncommitted.

The final R source, documentation, tests and package metadata match the frozen
check inputs. The staged changes exclude every preserved research file.
The diff against current main retains its features and adds only the intended
posterior work and these integration adaptations. No Linux or Windows CI was
launched from this local task; the merged CI workflows retain those jobs.

Validation logs:

* `/private/tmp/gratia-main-candidate-tests-final.log`
* `/private/tmp/gratia-integrated-main-diagnostic-regression.log`
* `/private/tmp/gratia-integrated-final-install.log`
* `/private/tmp/gratia-integrated-final-build.log`
* `/private/tmp/gratia-integrated-final-check.log`
* `/private/tmp/gratia-integrated-final-optional.log`
* `/private/tmp/gratia-integrated-final-pkgdown.log`
* `/private/tmp/gratia-integrated-main-loo-refits.log`
* `/private/tmp/gratia-integrated-main-parity-before.log`
* `/private/tmp/gratia-integrated-main-parity-after.log`
