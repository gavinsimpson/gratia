# Inverse Gaussian probability diagnosis and implementation

Inverse Gaussian families now receive native CDF, quantile and random-deviate
helpers through `fix_family_cdf()`, `fix_family_qf()` and `fix_family_rd()`.
This enables PIT and normal quantile residuals and uniform QQ and worm plots,
and makes simulated diagnostics and response draws use the same weighted
distribution, without a `statmod` dependency.

The starting point was `main` at `0c5c2def97ffe2f57554ab1bb7fb15676fa76e0d`.
The implementation and tests are on `gls/inverse-gaussian-cdf-qf` in a separate
managed worktree. The original checkout was clean and remains on `main`.

## Problems in the original helpers

The mean and dispersion normalization was correct. Writing `x = q / mu` and
`d = phi * mu` gives `sqrt(x * d) = sqrt(q * phi)`, exactly as required by the
paper. The unused `phi_m <- scale * mu` assignment was misleading, but did not
affect the answer. The examples at `q = .001` and `.0001`, with `mu = 1.5` and
`phi = .7`, already agreed with the published results. The old comment about
small quantiles was insufficient to identify the actual failure.

The reproducible problems were:

| Problem | Consequence |
| --- | --- |
| The CDF family dispatch was commented out | PIT and quantile residuals were unavailable |
| No inverse Gaussian QF was implemented, including in mgcv 1.9-4 | Uniform QQ diagnostics fell back to simulation |
| Only the lower tail was implemented | Normal-score residuals could lose extreme upper-tail information |
| Negative responses, zero and infinity lacked explicit branches | The old CDF returned `NaN` at these support boundaries |
| `wt` was ignored | The helpers did not use observation dispersion `scale / wt` |
| `2 / d + log(pnorm(-v))` subtracted enormous terms | At `q = mu = 1.5`, `scale = 1e-20`, the CDF returned `1.5` |

For example, at `q = 1`, `mu = 1.5`, `scale = .7`, the old CDF was
`0.500902523669769` for weights `.25`, `1` and `4`. The correctly weighted
probabilities are approximately `.678027096`, `.500902524` and `.281756057`.

## Native CDF

The public internal interface is
`cdf_invgaussian(q, mu, wt, scale, log_p = FALSE, lower_tail = TRUE)`.
The previous internal name `pinvgaussian` remains an alias.

The implementation uses the two normal probability terms from Giner and Smyth,
combines probabilities on the log scale, and obtains a large tail by
complementing the smaller tail with `log1mexp()`. It computes a small survival
probability directly, rather than subtracting a rounded CDF from one.

Two refinements address cancellation beyond the basic formula:

* With `u = (q / mu - 1) / sqrt(q * phi)` and
  `v = (q / mu + 1) / sqrt(q * phi)`, the second term is represented as
  `dnorm(u) * R(v)`, where `R` is the normal Mills ratio. Its large-argument
  series avoids the subtraction of `2 / (phi * mu)` from a huge normal log tail.
* Survival is `dnorm(u) * (R(u) - R(v))`. When the arguments nearly coincide,
  their difference is evaluated with Mills derivatives or with differences
  of the asymptotic series. This retains small survival probabilities even
  when the two normal arguments round to the same double.

The Mills series is restricted to arguments at least 16. The close-argument
expansion uses 12 derivatives when the argument gap times `1 + abs(u)` is below
`.1`. Independent rescaled density integrals exercise both regimes and the
transition at 16. No Gamma approximation is substituted for the distribution.

## Native quantile function

The interface is
`qf_invgaussian(p, mu, wt, scale, log_p = FALSE, lower_tail = TRUE)`.
The existing QQ callers require no interface changes.

The solver starts at the mode for ordinary probabilities, following the paper's
monotonic Newton strategy. It iterates on `log(q / mu)` so positivity is retained
and very small deviations from the mean can be represented internally.

For extreme probabilities, a normal quantile supplies a bound by solving
`(x - 1) / sqrt(d * x) = z` as
`log(x) = 2 * asinh(z * sqrt(d) / 2)`. A stable logarithmic form avoids overflow.
This bound improves initialization without depending on another package.

Newton corrections compute the signed difference between target and actual
probabilities with `expm1()` and log densities. The target probability is never
required to be representable on the ordinary scale. Brackets safeguard rejected
steps; extreme upper tails also use Newton corrections to the log probability.
Corrections below floating point spacing terminate without restarting a distant
bisection. A 200-iteration limit warns and returns `NA` for unresolved values.

At `mu = 1.5`, `phi = .7`, the lower-tail log probability `-1000` gives a quantile
near `.00071648751688109`; its log CDF returns `-1000`. The upper-tail log
probability `-10000` gives a quantile near `31455.34724759`, also retaining the
requested log tail. This is stronger than delegating to the locally installed
`statmod` 1.5.2: its lower-tail `-1000` example round-tripped to `-998.354699`.

## Native random generator

`rd_invgaussian(mu, wt, scale)` uses the direct Michael-Schucany-Haas algorithm.
For `Z` drawn from a standard normal and observation dispersion `phi = scale/wt`,
the smaller root has ratio
`r = exp(-2 * asinh(abs(Z) * sqrt(phi * mu) / 2))` relative to the mean.
Return `mu * r` with probability `1 / (1 + r)` and `mu / r` otherwise.

The implementation computes the relative root and branch probability in logs,
reusing the stable `asinh()` helper. It avoids subtracting nearly equal numbers,
forming `mu^2`, or forming an overflowing `phi * mu`. Values near the mean use
relative multiplication to preserve precision. Infinite mean uses the inverse
chi-square generator directly; deterministic limits require no random draws.
Missing and invalid rows are excluded from random generation.

`fix_family_rd()` supplies this generator before mgcv adds its own unweighted
sampler. Existing supplied `$rd` functions are preserved, and fixing a family
twice is idempotent. Simulation, posterior response sampling and grouped-family
adapters already use this dispatch. Seeded draws are reproducible but differ
from those produced by the previous mgcv generator.

## Inputs and limits

The helpers recycle numeric arguments to the longest length, return an empty
vector if any numeric input is empty, and preserve attributes of the first
argument when it has the output length. Missing elements propagate separately
from invalid elements. Invalid means, negative weights or dispersions, and
invalid probabilities produce `NaN` with a warning.

For positive finite mean and dispersion, the support is `[0, Inf]`. Quantile
probability endpoints always return `0` and `Inf`, with their roles reversed
for upper-tail inputs. Interior probabilities with zero dispersion return the
mean. Infinite dispersion, including a zero weight with positive scale, uses
the limiting point mass at zero. Infinite mean with positive finite dispersion
uses the inverse chi-square distribution. Simultaneous zero scale and zero
weight is invalid.

Quantiles outside the representable positive range return `0` or `Inf`.
At vanishing dispersion, rounding the final quantile to a double can make a
perfect probability round trip impossible: neighboring representable responses
can have noticeably different CDFs. The solver preserves the internal relative
coordinate but cannot remove this output precision limit.

All three native helpers agree on `phi = scale / wt`. A caller-supplied `$rd`
remains under the caller's control, including one explicitly obtained earlier
from `mgcv::fix.family.rd()`.

## Validation and reproduction

`tests/testthat/test-inverse-gaussian.R` tests the family hooks, published tail
values, independent density integration, the paper's chi-square identity,
extreme log quantiles, prior weights, recycling, missing and invalid elements,
support and limiting cases, and weighted fitted-model diagnostics with excluded
rows. Random-generator tests additionally check theoretical means and variances,
CDF/quantile agreement, seeded weight equivalence, stable roots at extreme
parameter scales, and limiting and invalid inputs. Fitted-model tests verify
`simulate()`, simulated QQ diagnostics and posterior response draws against
weighted native draws. Grouped response dispatch is checked separately.
The tests require no `statmod` installation.

Run `Rscript notes/inverse-gaussian/evaluate.R` from the worktree root for the
diagnosis and numerical audit. It optionally compares against an installed
`statmod`, without installing it or using it as a runtime backend. The saved
`evaluation.txt` records the audit on this machine. The numerical and integration
suite passed 10,766 assertions with no failures or warnings and 183 configured
skips (`NOT_CRAN=true`, `VDIFFR_RUN_TESTS=false`, `CI=true`). The updated
simulation help file also passed `tools::checkRd()`.
All tests and audits ran outside the sandbox as required by `AGENTS.md`.

The independent chi-square identity is
`F(q1) + S(mu^2 / q1) = P(chisq_1 > (q1 - mu)^2 / (phi * mu^2 * q1))`.
Testing individual tails with density integrals is also necessary: round trips
alone can conceal a shared error in a CDF and its inverse.

## appraise performance

The [appraise performance evaluation](appraise-performance.md) records fitted
model timings, rendering costs and CPU profiles for 1,000, 10,000 and 50,000
observations. It separates the QF and RNG costs from graphics and identifies
unused quantile-name formatting as a simulation bottleneck. The resulting
`names = FALSE` optimization is implemented and checked for exact numerical
equivalence.

## Source

Giner, G. and Smyth, G. K. (2016).
[statmod: Probability Calculations for the Inverse Gaussian Distribution](https://journal.r-project.org/articles/RJ-2016-024/).
The R Journal 8(1), 339-351. The supplied PDF was used as mathematical reference;
its example commands were not treated as instructions. The implementation was
written from the formulas, without copying `statmod` source code.
