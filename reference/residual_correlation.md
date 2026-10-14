# Ordered residual correlation diagnostics

Compute lag correlations within independent series. These are
descriptive checks of ordered residual structure, with no
independence-based confidence bands or significance tests.

## Usage

``` r
residual_correlation(
  model,
  type = NULL,
  lag_max = 20L,
  series = NULL,
  seed = NULL,
  ...
)

residual_correlation_plot(
  model,
  type = NULL,
  lag_max = 20L,
  series = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- model:

  a supported fitted model.

- type:

  residual type. For an AR1 BAM the default is `"standardized"`, using
  mgcv's stored `std.rsd`. Otherwise the default is `"quantile"`. BAM
  also supports `"working"`, using weighted residuals from the final
  iteratively reweighted least-squares fit, and
  `"working.standardized"`, adjusting those residuals for the fitted AR1
  correlation (whitening). Extended families are supported if they
  supply the expected deviance derivatives required for these working
  residuals.

- lag_max:

  positive integer; maximum observation lag.

- series:

  optional vector identifying series, in the order of observations used
  for fitting or of the original observations. BAM's `AR.start`
  boundaries are always retained, even if `series` is supplied. Other
  fitters need an explicit series and appropriately ordered fitting data
  when separate time series are present.

- seed:

  optional seed for randomized residuals.

- ...:

  arguments used to prepare the model for diagnostics, including
  `conditioning` and, for sdmTMB, `field_method`.

## Value

`residual_correlation()` returns a tibble with lag, correlation and the
number of finite observation pairs used at each lag.
`residual_correlation_plot()` returns a ggplot.

## Details

BAM's generalized estimating equations (GEE) AR1 correction acts on the
weighted working response used for fitting. Whitening these working
residuals can reveal remaining serial structure. The stored `std.rsd` in
current mgcv adjusts its final residuals for AR1 correlation, which need
not be the weighted working residuals for non-Gaussian models. Both
checks are approximate. They can reveal remaining serial structure, but
do not specify a joint response distribution, establish normality or
provide calibrated goodness-of-fit tests. Fitted parameters, leverage
and discrete likelihoods affect correlations.

Lags use observation positions within series, not elapsed time or
spatial distance. Means are removed within each series, and correlations
use only finite pairs at each lag; missing residuals and omitted fitting
rows do not close gaps. If series are interleaved, supply `series` for
all original observations so omitted rows can be assigned to the right
series. If `series` covers only observations used for fitting, an
omitted position is counted as a gap in every series with observations
on both sides of it. Observations assigned to another series never
create gaps. Significance bands that assume independent residuals are
omitted. Check spatial residual patterns with their coordinates and a
suitable distance diagnostic.

## See also

[diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md),
[`qq_plot()`](https://gavinsimpson.github.io/gratia/reference/qq_plot.md)
