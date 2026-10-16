# Plot coefficient-chain diagnostics

Native ggplot2 displays of retained coefficient chains. Trace plots show
movement over iterations, including repeated values after MH rejections.
Rank plots compare overlaid histogram outlines for the ranks of each
coefficient pooled across chains. Chains exploring the same distribution
should have approximately uniform rank histograms. These plots
complement the numerical summaries; similar-looking chains alone do not
establish convergence.

## Usage

``` r
# S3 method for class 'sampling_diagnostics'
draw(
  object,
  type = c("trace", "rank"),
  select = NULL,
  n_bins = 20L,
  scales = NULL,
  ncol = NULL,
  nrow = NULL,
  alpha = 0.7,
  linewidth = 0.4,
  discrete_colour = NULL,
  ...
)
```

## Arguments

- object:

  A
  [`sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md)
  object.

- type:

  Either `"trace"` or `"rank"`.

- select:

  Coefficients to plot, as exact names, positive integer positions, or a
  logical vector with one entry per coefficient. The default plots every
  available coefficient.

- n_bins:

  Positive integer; number of common pooled-rank bins. Used only for
  rank plots. Tied draws receive their average rank, without jitter.

- scales:

  Facet scales, either `"fixed"` or `"free"`. By default trace plots
  have separate coefficient scales and rank plots share a count scale.

- ncol, nrow:

  Number of columns or rows in the panel layout.

- alpha:

  Transparency of the chain lines.

- linewidth:

  Width of the chain lines.

- discrete_colour:

  An optional ggplot2 discrete colour scale. Defaults follow gratia's
  smooth plots: Okabe-Ito for up to nine chains, then hue.

- ...:

  Additional arguments passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

The trace x axis counts retained iterations within each chain, rather
than the globally unique draw number. Burn-in is discarded by the
samplers and cannot be displayed. With thinning, successive plotted
iterations represent successive retained draws, not successive MH
transitions.

Rank plots require at least two chains. Ranks are computed separately
for each selected coefficient, pooling all chains. A dashed line shows
the average count per bin. Independent continuous draws from the same
distribution should give approximately flat histograms. Autocorrelation
and ties affect the histogram variation; the reference line is not a
confidence bound.

Plots always describe coefficient draws, including when diagnostics are
extracted from response samples or smooths. For transformed quantities
use
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)
with external diagnostic tools. Sampling with `diagnostics = TRUE`
retains coefficient draws for plotting from those outputs. Coefficient
matrices can be plotted whenever their sampling metadata are available,
even without numerical diagnostic summaries.

## See also

[`sampling_diagnostics()`](https://gavinsimpson.github.io/gratia/reference/sampling_diagnostics.md),
[`as_mcmc_draws()`](https://gavinsimpson.github.io/gratia/reference/as_mcmc_draws.md)

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 100, seed = 42)
model <- gam(y ~ s(x0, k = 5), data = dat, method = "REML")
b <- post_draws(model, n_draws = 400, chains = 4, method = "mh",
  burnin = 100, seed = 281)
diagnostics <- sampling_diagnostics(b)
draw(diagnostics, select = 1:2)

draw(diagnostics, type = "rank", select = 1:2, n_bins = 10)
```
