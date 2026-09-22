# Experimental Tweedie mixture quantiles and CDF

This branch implements an **internal, unexported experiment**, not a replacement
of the package's family factories. `appraise()`, `qq_plot()`, and `worm_plot()`
still default to simulation for Tweedie models. Nothing has been committed.

## Recommendation from the measurements

The mixture algorithm is promising for explicit uniform QQ diagnostics and for
CDF-based PIT/quantile residuals. Most of the speedup comes from changing the
CDF algorithm, even at strict tolerances. Relaxing accuracy gives an additional,
smaller improvement. The diagnostic profile below appears a reasonable candidate
for these uses; the loose profile offers little additional benefit and misses
our proposed normal-score error target in a small-dispersion example.

Use **normal-score error below 1e-4** as an initial engineering target, not a
universal statistical guarantee. Also examine deviance-residual differences and
QQ reference quantiles, because those transformations are model dependent. For
PIT ranks, probability errors should be small relative to 1/n; revisit the target
for extremely large samples or inference beyond graphical diagnostics.

## Implementation

`R/tweedie-quantile.R` supplies `qtweedie_mixture()` and `ptweedie_mixture()`.
Both accept scalar power strictly between 1 and 2, with scalar/common-length
recycling for the other parameters. They support missing inputs and endpoints;
the CDF additionally supports negative responses and direct log survival.

For N ~ Poisson(lambda), Y | N=k ~ Gamma(k*alpha, scale=beta), where

- lambda = mu^(2-power) / (phi * (2-power));
- alpha = (2-power) / (power-1);
- beta = phi * (power-1) * mu^(power-1).

The CDF is exp(-lambda) plus the weighted Gamma CDF sum over positive counts.
The compound Poisson interpretation is described in
[Smyth's Tweedie documentation](https://gksmyth.github.io/s/tweedie.html).
The implementation derives the CDF mixture directly by conditioning on N.

Poisson tail bounds select terms, without renormalising their weights. Their
omitted mass bounds **truncation error**, not all floating-point or root error.
Weights and shapes are cached during each solve. Calculations use the smaller
of positive lower-tail mass and survival, in log space. Root finding works on
log(y/mu), preserving relative scaling for very small/large responses. A failed
probability-residual check triggers one tighter solve using the same mixture.

The CDF uses the same mixture evaluator, refining its truncation for small tails.
This matters for normal-score residuals: a small absolute CDF error near one can
still produce a large error after `qnorm()`. The quantile algorithm also handles
the zero atom analytically and retains log quantiles when positive values
underflow as doubles. For an underflowed Gamma argument it uses the leading
incomplete-Gamma term only below the smallest normal double, where the neglected
relative correction is O(x).

Both functions now default to `fallback = FALSE`: no expensive upstream
inversion occurs automatically. A 10,000-term budget caps the sum to a contiguous
window around the Poisson mode. If the accuracy budget cannot be met, return the
available approximation and issue one warning per vector call. Quantile details
identify `approximate` or `unresolved` values and report retained terms, omitted
mass and the reason. If the retained mass cannot bracket a quantile, or numerical
failure leaves no estimate, return NA with the warning.

Partial CDF tails are returned literally, without renormalising the weights or
filling missing mass by complementing the other tail. Thus their lower and upper
probabilities need not sum to one when the budget is exhausted. Partial quantile
estimates invert the retained smaller tail; with substantial missing mass they
are not a guaranteed coherent, monotone quantile function. These are visibly
warned, potentially poor approximations, not promises of diagnostic accuracy.

The caller can increase `max_terms` or explicitly pass `fallback = TRUE` to allow
`tweedie::qtweedie()` / `tweedie::ptweedie()` for unresolved values only. The name
`fallback` applies consistently to quantiles and CDFs. Quantile fallback rejects
interior log probabilities that round to endpoints and warns/returns NA on an
upstream error. CDF fallback warns about ordinary-probability log-tail limitations.
Neither fallback inherits the mixture's tail-accuracy checks.

```r
# Bounded work, with a warning and the available partial answer if necessary
qtweedie_mixture(.9, mu = 1, power = 1.5, phi = 1, max_terms = 5)
ptweedie_mixture(1, mu = 1, power = 1.5, phi = 1, max_terms = 5)

# Explicit permission to use the expensive backend when the budget is insufficient
qtweedie_mixture(.9, 1, 1.5, 1, max_terms = 5, fallback = TRUE)
ptweedie_mixture(1, 1, 1.5, 1, max_terms = 5, fallback = TRUE)
```

## Accuracy settings

| Profile | Absolute CDF truncation budget | Relative tail/root probability tolerance | Log-quantile root tolerance |
|---|---:|---:|---:|
| strict (current experimental defaults) | 1e-12 | 1e-8 | 1e-10 |
| diagnostic | 1e-7 | 1e-5 | 1e-6 |
| loose | 1e-5 | 1e-3 | 1e-4 |

The truncation budget tightens further in small tails. It is **not** an upper
bound on the complete quantile's probability error. The diagnostic profile's
largest probability error in the four 60-observation cases was 1.26e-6, after
including root error; its largest normal-score error was 3.17e-6.

## Results on this machine

A real `tw()` GAM with 269 observations, varying fitted means, and ten uniform
QQ permutations gave:

| Method/profile | Seconds | Largest change in QQ reference residual quantiles |
|---|---:|---:|
| Existing `tweedie::qtweedie()` | 68.740 | reference |
| Mixture, strict | 0.279 | 3.62e-11 |
| Mixture, diagnostic | 0.234 | 2.99e-7 |
| Mixture, loose | 0.210 | 3.37e-5 |

Changing only the permutation seed produced RMS change 0.0131 and maximum change
0.0480. The diagnostic profile's numerical change is much smaller than this
randomisation variability. These timings cover QQ quantile/residual computation,
not model fitting or rendering all `appraise()` panels.

Warm 60-quantile timings across powers 1.05, 1.5 and 1.95 and dispersion 1 or
0.01 gave diagnostic-profile speedups of **22–985x** over one timed upstream call.
The maximum normal-score error ranged from 3.52e-7 to 3.17e-6. The loose profile
reached 2.98e-4 at dispersion 0.01, exceeding the provisional 1e-4 target.
These are exploratory local timings, not guarantees across machines/parameters;
mixture timings are medians of repeated warm batches, and upstream times are
single calls. Full per-case values are in the CSVs.

For 269 simulated responses per setting, diagnostic-profile CDF evaluation took
roughly 0.005–0.033 seconds. Relative to a tighter mixture calculation, maximum
PIT changes were about 5e-8 and normal-score residual changes below 6e-7. Zero
observations use identical fixed randomisation in this numerical benchmark so
atom handling contributes no difference. A separate integration test exercises
the existing `do_quantile_residuals()` path, including its current zero handling;
this experiment does not change the package's residual randomisation policy.

### Reference disagreement investigated

At y=1.7295884004568, mu=2.10911548192, power=1.5, phi=0.01:

- mixture CDF: 0.01209160530872;
- `tweedie::ptweedie_series()`: 0.01209160530872;
- deliberately over-wide direct Poisson/Gamma sum: 0.01209160530872;
- `tweedie::ptweedie()` inversion (3.1.0): 0.01215150161377.

The agreement of three series calculations suggests the 5.99e-5 disagreement is
an inversion limitation here, rather than mixture truncation. This is why the
evaluation does not treat the existing inversion as an infallible reference.
See `results/reference-crosschecks.csv` for this and the other settings.

### Difficult-case coverage

The stress scan contains 594 probability/parameter combinations per profile:
powers 1.01 through 1.9999, means 0.001/1/1000, dispersions 0.001/1/100,
central/tail probabilities down to 1e-12, log upper-tail probabilities near
-1e-30, and probabilities just above the zero atom.

| Profile | Atom | Finite positive mixture | Positive underflow | Work limit exceeded |
|---|---:|---:|---:|---:|
| strict | 122 | 392 | 15 | 65 |
| diagnostic | 122 | 397 | 15 | 60 |
| loose | 122 | 404 | 15 | 53 |

There were no remaining bracketing, convergence, or tail-residual failures in
this grid. The original stress scan records where the full accuracy budget is exceeded,
without running upstream inversion or partial sums there (`allow_partial = FALSE`).
These counts now indicate where the default warns and uses a partial answer,
or where an explicit opt-in can invoke inversion. The original timings above
used cases that did not exhaust the budget. Its success checks use the mixture's own residual,
so they are robustness checks, **not independent accuracy validation of all 594
points**. Independent inversion/series comparisons cover the ordinary parameter
cases and selected discrepancies; wider independent validation remains useful.

## Reproduce and review

From the repository root:

```r
# Focused correctness, integration, and regression checks
 devtools::test(filter =
   "tweedie-mixture|family-quantiles|diagnostic-method|quantile-residuals")
```

```sh
Rscript notes/tweedie-mixture/evaluate.R
```

The script records timings, errors, stress outcomes and R/package versions in
`results/`. It passes a copied family with an experimental qf into the real
`qq_uniform()` pipeline; it never changes the installed/default family factories.

Tests cover input contracts, endpoints/atoms, vector recycling, scaling,
monotonicity, convergence under tighter truncation, independent CDF/quantile
references, direct extreme log tails, underflow reporting, fallback dispatch,
normal-score accuracy at diagnostic tolerances, and residual integration. New tests explicitly prevent inversion on the default
path, verify literal capped sums and their roots, and check opt-in dispatch for
both functions.

Before promoting this into `make_qf_tw()` / `make_cdf_tw()`, review the accuracy
budget, fallback policy, and performance on representative user models. The new opt-in fallback policy prevents silently routing pathological tails
to an unchecked expensive backend. Broader
independent stress accuracy checks and profiling repeated-parameter caching are
worth considering; moving to compiled code is not necessary to establish the
benefit shown here.
