# Inverse Gaussian appraise performance

Default `appraise()` is practical with the native inverse Gaussian helpers:
the median time to construct and render all four plots was 0.322 seconds for
1,000 observations, 0.846 seconds for 10,000, and 2.998 seconds for 50,000.
Drawing points is the largest overall cost. Quantile inversion dominates the
computation before rendering; the random generator is much cheaper.

## Model and measurement

The benchmark fits weighted inverse Gaussian GAMs with a log link and four
smooths, each with `k = 10`, using `mgcv::gam(method = "REML")`. Data are
simulated with dispersion `.4`, precision weights between approximately `.67`
and `1.49`, and a nonlinear mean. The same 50,000-row simulated data set supplies
the three sample sizes. The 10,000-observation model has fitted means between
`.207` and `6.195`, estimated dispersion `.3935`, and total effective degrees of
freedom `22.31`.

Measurements ran on this arm64 Mac using R 4.6.1, mgcv 1.9-4, ggplot2 4.0.3,
and patchwork 1.3.2. Rendering uses a Cairo PNG device at 1200 by 1000 pixels
and 144 dpi. Each method is warmed once, then timed five times in interleaved
order. Garbage collection runs before each timed observation. Timings include
opening and closing the device, ggplot/patchwork building and layout, and
drawing all panels. They exclude package loading, data generation and fitting.
Interactive-device and other-machine times can differ.

`appraise()` returns a plot object; calling it without printing does not measure
the cost of generating the visible plots. The construction and rendering phases
are therefore recorded separately.

## Elapsed times

These baseline timings were collected before applying the unused-quantile-name
optimization. Default uniform timings are unaffected by that change; the
simulation column describes the earlier implementation. The integrated
comparison below measures the optimized production function.

Default method, in seconds:

| Observations | Construct plot object | Render plots | Total |
| ---: | ---: | ---: | ---: |
| 1,000 | 0.063 | 0.263 | 0.322 |
| 10,000 | 0.298 | 0.544 | 0.846 |
| 50,000 | 1.312 | 1.689 | 2.998 |

These are separate medians, so columns need not add exactly. Total ranges over
the five repetitions were `.306` to `.338`, `.811` to `.936`, and `2.976` to
`3.122` seconds respectively. The first default call after package loading and
fitting took `1.413` seconds at 1,000 observations, including `1.201` seconds
for first graphics use. The warm timings should not be read as first-call times.

Fitting the models took `.185`, `2.097`, and `11.117` seconds respectively.
Fitting is separate from the diagnostic costs above.

Comparison of rendered totals, in seconds:

| Observations | Default, 10 uniforms | One uniform | 50 simulations | Normal reference |
| ---: | ---: | ---: | ---: | ---: |
| 1,000 | 0.322 | 0.284 | 0.351 | 0.298 |
| 10,000 | 0.846 | 0.563 | 1.192 | 0.551 |
| 50,000 | 2.998 | 1.892 | 5.045 | 1.782 |

`n_uniform = 1` averages one permutation instead of ten. Simulation also
computes and draws reference bands. The normal reference is included to show
graphics costs with little distributional computation; it is a different
diagnostic, not a statistically equivalent speed replacement.

## Costs of the new helpers

Default inverse Gaussian `appraise()` uses ten QQ permutations and therefore
evaluates the QF on ten vectors of observation length. The CDF is evaluated
inside the solver. Ordinary deviance residuals in the other three panels do
not call the new CDF. The random generator is used by `method = "simulate"`.

Median seconds for one vector call on the fitted models:

| Observations | QF | CDF, both log tails | Random draws |
| ---: | ---: | ---: | ---: |
| 1,000 | 0.0048 | 0.0010 | 0.0002 |
| 10,000 | 0.0266 | 0.0080 | 0.0017 |
| 50,000 | 0.1310 | 0.0393 | 0.0082 |

Quantile residuals, which evaluate both CDF tails and transform through the
normal quantile, took `.002`, `.008` and `.039` seconds respectively. Those
costs are separate measurements, not extra charges added to default `appraise()`.

## Bottlenecks and improvement targets

CPU profiles were collected separately from elapsed timing runs with
`Rprof(interval = .001)` on the 10,000-observation model. Percentages below are
sampled CPU time, not a decomposition of wall time. Inclusive percentages for
nested functions must not be added together.

1. **Rendering points dominates the complete default call.** Grid graphics
   account for about 58% of sampled CPU time, and `drawDetails.points` for 55%.
   The measured graphics phase is about 64% of elapsed time at 10,000 rows and
   56% at 50,000. Three panels draw observation-level point clouds. For these
   sizes, making the QF arbitrarily fast would still leave substantial drawing
   time. Reducing or aggregating displayed points is a separate plotting-design
   choice; it was not applied in these measurements.

2. **QF inversion dominates construction.** About 92% of sampled default
   construction time is inside the QF. In the isolated QF profile, the Newton
   solver accounts for 95%, including 56% in its CDF evaluator. Normal CDF
   calls themselves account for only 13%; vector bookkeeping, indexing and
   control flow also matter. The Mills series accounts for approximately `.5%`,
   so weakening the extreme-tail safeguards would not address the main cost.
   Reducing repeated vector work or moving the numerical loop into compiled
   code are plausible optimization targets. The current cost is approximately
   linear in observation count and in `n_uniform` over this range.

3. **Simulation bands are more expensive than drawing the random responses.**
   `apply(sims, 1L, quantile, ...)` accounts for about 69% of sampled simulation
   construction time; the sampler accounts for about 20%. About 21% is spent
   formatting quantile names. Those percentage labels are discarded by the
   plotting code.

The third target has a measured inexpensive improvement. A private copy of
`qq_simulate()` with `names = FALSE` on its two quantile calls took a median
`.4135` seconds versus `.6040` for the original, over ten interleaved repetitions
at 10,000 observations. This saves approximately **32% of QQ simulation
construction time**. All theoretical quantiles, interval endpoints and residual
values were checked to be bitwise identical after removing names. This is an
initial isolated experiment. The optimization is now applied to the production
`qq_simulate()` function. A fresh integrated comparison measured `.3955`
seconds for production versus `.6015` seconds for a private legacy-naming
reference, approximately **34% less QQ simulation construction time**. Its
end-to-end rendering improvement was not separately measured.

Regression tests compare every numerical output with the former named-quantile
calculations using identical seeded draws. They cover weighted Gaussian, Poisson
and inverse Gaussian fits with excluded rows, all three residual types, two
interval levels, one or seven simulations, detrending, and multivariate models.
The comparisons require bitwise identity after discarding names. The full
package suite passes 10,766 assertions with no failures or warnings and 183
configured skips (`NOT_CRAN=true`, `VDIFFR_RUN_TESTS=false`, `CI=true`).

The unused quantile-name formatting has therefore been removed from the
simulation path. For default uniform diagnostics, the QF loop is
the numerical target, while rendering remains the larger overall cost. The
random generator is not the dominant bottleneck.

The principal QQ matrix occupies `8 * n * n_uniform` bytes in the default path
and `8 * n * n_simulate` bytes in the simulation path: about 4 MB and 20 MB
respectively at 50,000 rows with the default counts. This is matrix storage
alone, not measured peak memory; temporary arrays and graphics add to it.

## Reproduction and files

Run from the worktree root:

```r
source("notes/inverse-gaussian/benchmark-appraise.R")
source("notes/inverse-gaussian/benchmark-appraise-intervals.R")
```

Each script can also be run with `Rscript`. The scripts use the current local
gratia source and require no `statmod` dependency. Package benchmarks ran outside
the sandbox in accordance with `AGENTS.md`.

The [benchmark directory](appraise-benchmark/) contains raw timings, medians,
model fit summaries, session information and CPU profiles. In particular,
[medians.csv](appraise-benchmark/medians.csv),
[helper-medians.csv](appraise-benchmark/helper-medians.csv), and
[interval-names-medians.csv](appraise-benchmark/interval-names-medians.csv)
contain the baseline measurements above. The supplementary script now compares
production with a private copy that restores legacy quantile naming, saving its
current results to
[interval-names-integrated-medians.csv](appraise-benchmark/interval-names-integrated-medians.csv).

The rendered [1,000-row plots](appraise-benchmark/default-1000.png) and
[10,000-row plots](appraise-benchmark/default-10000.png) are saved for inspection.
