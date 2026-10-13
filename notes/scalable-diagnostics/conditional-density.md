# Conditional residual density rendering

Scalable residual panels now default to `density_method = "kde"` using
`KernSmooth::bkde2D()`. KernSmooth is an Imports dependency. It performs the
linear binning and FFT convolution; gratia does not implement a kernel estimator.
The existing rectangular conditional histogram remains available through
`density_method = "histogram"`. Count panels always use unsmoothed counts.

## Options considered

| Approach | Benefit | Limitation |
| --- | --- | --- |
| Rectangular conditional histogram | Exact cell counts; no bandwidth choice | Jagged discrete-response bands and sensitivity to bin edges |
| Residual-axis KDE within predictor bins | Could smooth response bands while keeping predictor bins independent | Would retain vertical bin seams and noisy estimates in sparse predictor bins |
| Two-dimensional binned KDE | Smooths both axes using KernSmooth; separates bandwidth from display resolution | Can blur narrow features; Gaussian smoothing does not make traditional count residuals normally distributed |
| Randomized quantile residuals | Removes discrete CDF jumps when distributional calculations are available | Changes residual semantics and depends on validated model/family CDF support |

The selected implementation uses two-dimensional KDE by default, following the
user's preference. It operates after the shared model adapter, so supported
model classes need no new density-specific methods. Quantile residuals remain
an independent choice; the smoother does not silently replace Pearson or
deviance residuals or add unsupported quantile semantics to model adapters.

## Calculation and controls

The estimator uses all finite retained observations with equal observation
weights, just as the existing conditional histogram does. Model prior weights
remain incorporated in the residual calculation. `bandwidth = c(h_x, h_r)` sets
Gaussian bandwidths in plotted units, including predictor ranks when those are
used. With `NULL`, the univariate normal-reference rule from `bw.nrd0()` is
adjusted from an n^(-1/5) rate to n^(-1/6) for two dimensions. Constant axes have
a positive plotting-range-based fallback. This is a default smoothing heuristic,
not a claim of optimal bandwidth selection for every distribution.

The predictor grid spans the observed range; the residual grid extends three
bandwidths past each observed extreme. KernSmooth is evaluated on a sufficiently
fine internal grid to avoid its zero-width-kernel warning. If the display grid
is coarser, estimates are averaged into display cells, rather than sampled
between narrow peaks. An internal grid exceeding 2^20 cells is rejected with
advice to increase bandwidth or reduce the requested grid size.

Each populated predictor slice is divided by the integral of its joint-density
estimate over the displayed residual cells, so its conditional density
integrates to one. Predictor cells with no observations remain blank rather
than extrapolating into unsupported gaps. A square-root colour transformation
reveals low-density regions; the legend retains density units. Users can
replace that scale using ordinary ggplot additions.

Gaussian kernels can extend beyond bounded residual supports, including PIT
residuals. The implementation does not apply boundary correction or compare
empirical density with a fitted-model reference density. The unsmoothed option
is available when preserving discrete bands or support boundaries matters.

Final plots retain only display-cell bounds, original observation counts, and
conditional density values, with at most the product of `density_bins` rows per
component. Count conservation, normalization, dependency agreement, coarse and
constant grids, missing rows, RNG preservation, and appraisal control forwarding
are covered by tests.

## Benchmark

`benchmark-density.R` isolates estimation and drawing from fitting and QQ
calculations. `density-benchmark-results.csv` records three runs per combination
at 10,000, 100,000 and 1,000,000 observations. A 50 by 50 display grid remains
bounded at 2,500 rows across all data sizes; timings are descriptive rather than
test thresholds.

## Sources

* [KernSmooth bkde2D documentation](https://stat.ethz.ch/R-manual/R-devel/library/KernSmooth/html/bkde2D.html)
* [mgcViz l_dens2D documentation](https://mfasiolo.github.io/mgcViz/reference/l_dens2D.html)
