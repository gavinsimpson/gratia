# Native inlaws support for gratia diagnostics

gratia now accepts `family$gratia_diagnostics(mu, wt, scale, y, eta)`. The
factory returns a named list of response moments and functions that capture
the fitted parameters. The complete contract is documented in
`?gratia::family_diagnostics`. Existing `$rd`, `$cdf` and `$qf` functions remain
unchanged. This interface requires no dependency between the two packages.

The native implementation is on inlaws branch `gls/native-gratia-diagnostics`,
created from main (`aba60178a01b31b703603c47db6ed185b2592ad3`). Every constructor
now creates its factory, including constructors used during fitted-family
reconstruction. The optional tests use native constructors and fitted families
without attaching adapters. The prototype helper has been removed.

The ordinary gratia test suite exercises the interface with custom families and
does not require inlaws. Run the optional native suite with an installed copy of
the new inlaws revision using `Rscript tools/test-inlaws-diagnostics.R`.

### Publish inlaws before enabling CI

The previous CI pin, `aba60178a01b31b703603c47db6ed185b2592ad3`, lacks the native
factories. A new inlaws commit must first be published to `gam-mafia/inlaws`.
Then set the gratia repository variable `INLAWS_DIAGNOSTICS_REF` to that published
commit SHA or tag. The Linux native integration job installs exactly that ref.
Until the variable is set, the optional job is skipped; gratia's ordinary tests
continue to run. The runner rejects an older inlaws installation explicitly.
Do not set the variable to a local or unpublished commit.

## Family additions

| Families | Native diagnostic information |
| --- | --- |
| `nbls`, `cmp`, `cmpls`, `gpoisson`, `pig`, `pigls` | Native factories supply `cdf_left(q) = F(ceiling(q) - 1)` and response means and variances, including fitted nuisance parameters. |
| `zinb`, `zanb` | Count bounds and complete response moments. For ZINB, mean is `p * mu`. For ZANB, mean is `p * mu / (1 - q0)`, where `q0` is the underlying NB zero probability. |
| `cumulative_link` | Probability functions bind to `eta`; CDF left limits randomise categories. Mean and variance describe the category value, with mean `1 + sum(P(Y > k))`, over interior category thresholds. |
| `ordbeta` | Supplies `cdf_left` at zero and one, including `F(0-) = 0` and `F(1-) = 1 - p1`. Mean `p1 + pI * mu` and variance include both endpoint masses and the interior beta component. |
| `cgamma`, `cgammals`, `clognormal`, `clognormalls` | Decodes the response's `censor` attribute or two-column matrix into lower and upper endpoints. The distribution callbacks describe uncensored responses; interval probabilities determine the residuals. |
| `gpd` | Declares a continuous response. Mean is `scale / (1 - shape)` for shape below one; variance is `scale^2 / ((1 - shape)^2 * (1 - 2 * shape))` for shape below one-half. Moments are unavailable outside these conditions. |
| `log_logistic`, `log_logisticls` | Declares continuous responses, retains the arithmetic mean, and marks variance unavailable when log-response logistic scale is at least one-half. |
| `betabinomial` | Native factories bind fitted probability, precision and retained trial totals to count CDFs, left limits, quantiles, moments and simulation. Trial totals are recorded after subsetting and missing-row removal. |
| `dirmult` | Native factories transform log ratios and concentration to expected category counts and variances, supply marginal beta-binomial CDFs and left limits, and draw all response columns jointly conditional on each retained row total. There is no joint inverse CDF. |
| `dirichlet` | Native factories transform log ratios and concentration to response means and variances, supply marginal beta CDFs, and retain joint Dirichlet draws with `independent = FALSE`. There is no joint inverse CDF. Deviance residuals remain undefined. |

For a new grouped-family component, gratia requires supplied `bounds`,
`cdf_left`, or an explicit `continuous = TRUE` declaration. Merely providing
a mean does not establish the probabilities over which to randomise.

## Censoring and reference simulations

An observed interval does not identify the observation process that produced
it. For traditional QQ and worm references, set `reference_available = FALSE`
with an explanation when that process is unknown. PIT/quantile plots can still
use the interval probabilities.

The inlaws censored constructors accept `observe`. The helper
`inlaws::censoring_process(lower, upper)` supports scalar limits shared by all
observations. It therefore survives subsetting, missing-row removal and grouped
component preparation without storing a vector of training thresholds. Even
realised exact observations do not establish an unknown censoring rule. For a
known uncensored positive response, use `censoring_process(0, Inf)`.

When thresholds and the censoring rule are known, supply `observe(y)` to apply
that rule to each generated response before calculating its reference residual.
For example, left censoring at known threshold `limit` can be encoded as:

```r
observe <- function(y) {
  structure(pmax(y, limit), censor = ifelse(y < limit, -Inf, y))
}
```

The thresholds must align with the factory's retained fitting rows and, for
`gfam()`, its component rows. Do not recover thresholds for exact observations
from their realised values. Native custom `observe` functions must apply a
row-independent rule. Vector
thresholds are not supported by the native interface; it has no retained-row
identity argument with which to align them safely.

This interface applies the observation process to traditional diagnostic
references. It does not change public response sampling or posterior predictive
sampling. Censored posterior predictive checks additionally need that process
applied at every parameter draw; the fitted-parameter factory does not establish
support for those checks.

## Preserve fitted parameter state

Attach factories inside constructors so `preinitialize` functions that rebuild
a family also rebuild its diagnostics. Factories that need nuisance parameters
must read the same fitted `getTheta()` closure as the distribution functions,
rather than capture initial parameter values. Ensure serialization and family
reconstruction preserve that shared state, including `gfam()` components.

Return replacement `cdf`, `cdf_left`, and `qf` functions with the argument names
`log_p` and `lower_tail`, supporting both tails and log probabilities directly.
For older native callbacks, gratia still adapts `logp`, `log.p`, and `lower.tail`.
The native count factories preserve older callbacks. For families whose native
CDF lacks an upper-tail argument, the left-limit function complements its log
lower-tail CDF; this retains the existing extreme-tail precision limitations.

Keep inlaws tests for fitting, parameter transformations, support and moments.
Add tests that the factory survives fitting, cloning, nuisance-parameter updates,
missing-response removal and supported `gfam()` fits. The native two-package
tests now cover constructor and serialized factories,
fitted ordinary and distributional models, reordered subsets and missing rows,
known censoring processes, and supported grouped fits. Independent calculations
check mixture probabilities, beta-binomial margins, transformed multivariate
moments, and undefined heavy-tail moments. inlaws also tests joint covariance
against the analytic Dirichlet and Dirichlet-multinomial covariance.

Real integration required two generic gratia corrections. Grouped-family
parameter reconstruction clones the complete closure graph before restoring
fitted theta, so nested factories share the restored state. Plots use supplied
matrix response means to retain component columns; for unfamiliar multivariate
families, residual panels use each component's response mean on the horizontal
axis rather than mislabelling log ratios or concentration as component links.
Existing mgcv multivariate plots keep their presentation. Custom-family
regressions cover both changes without depending on inlaws.

## Remaining limits

Beta-binomial CDFs and quantiles sum probabilities over count support; very large
trial totals can make them expensive. Dirichlet deviance remains undefined.
The componentwise Dirichlet-multinomial factory offers PIT, quantile, response
and Pearson residuals; its existing native row-level deviance is still available
through `stats::residuals()`. Multivariate PITs are marginal component checks,
while simulated traditional reference bands retain joint response dependence.
Row-specific censoring thresholds and censored posterior observation processes
are outside this fitted-parameter interface.


## Validation on 14 October 2026

Validation used R 4.6.1 and mgcv 1.9-4, with all tests and checks run outside
the sandbox. inlaws was built from the isolated feature checkout and installed
in `/private/tmp/inlaws-native-library` for the native integration run.

- `R CMD build --no-build-vignettes`, followed by
  `R CMD check --no-manual --no-vignettes`: all 39 inlaws test scripts passed,
  including `family-diagnostics.R`. The check recorded zero errors, zero notes
  and two warnings for the existing unbuilt `family-compatibility.qmd` vignette
  and missing `inst/doc` output.
- `R_LIBS=/private/tmp/inlaws-native-library Rscript
  tools/test-inlaws-diagnostics.R`: 13 native integration tests, 288 passing
  assertions, zero failures, errors, warnings or skips.
- `testthat::test_local()` with filter
  `family-diagnostics|response-distributions|gfam|diagnostic|residuals|qq|appraise|missing|scalable|simulate`:
  248 gratia regression tests, 3,035 passing assertions, zero failures, errors,
  warnings or skips. Both new generic regressions ran: matrix response layouts
  and nested diagnostic closures during grouped parameter reconstruction.

Validation used the native inlaws code in the sibling `inlaws-diagnostics`
checkout on `gls/native-gratia-diagnostics`, while preserving the original
inlaws vignette checkout. The gratia changes were developed on
`gls/extensible-family-diagnostics`.
