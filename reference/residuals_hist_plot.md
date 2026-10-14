# Histogram of model residuals

Histogram of model residuals

## Usage

``` r
residuals_hist_plot(
  model,
  type = NULL,
  n_bins = c("sturges", "scott", "fd"),
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  seed = NULL,
  by_family = NULL,
  conditioning = NULL,
  ...
)
```

## Arguments

- model:

  a fitted model supported by
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- type:

  character or `NULL`; residual type: `"deviance"`, `"pearson"`,
  `"response"`, `"pit"`, or `"quantile"`. Grouped models automatically
  use quantile residuals when all active components are supported,
  otherwise all use deviance residuals with a message. Ordinary models
  retain deviance residuals by default. See
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  for distributional support.

- n_bins:

  character or numeric; either the number of bins or a string indicating
  how to calculate the number of bins.

- ylab:

  character or expression; the label for the y axis. If not supplied, a
  suitable label will be generated.

- xlab:

  character or expression; the label for the x axis. If not supplied, a
  suitable label will be generated.

- title:

  character or expression; the title for the plot. See
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- subtitle:

  character or expression; the subtitle for the plot. See
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- caption:

  character or expression; the plot caption. See
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- seed:

  integer; random seed to use for PIT or quantile residuals.

- by_family:

  logical or `NULL`; separate indexed grouped-family components into
  facets. The default pools quantile residuals with component colours
  and facets traditional residuals. `TRUE` and `FALSE` explicitly select
  the layout. In
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
  observed-versus-fitted values always use component facets. Standalone
  [`observed_fitted_plot()`](https://gavinsimpson.github.io/gratia/reference/observed_fitted_plot.md)
  facets by default and permits explicit pooling. Ignored for ordinary
  models.

- conditioning:

  character or `NULL`; `"conditional"` holds fitted random effects
  fixed, `"marginal"` averages over new effects, and `"gam_component"`
  uses the extracted GAM. Availability and defaults depend on the model;
  see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- ...:

  optional arguments passed to model preparation, such as the sdmTMB
  `field_method`.

## Details

Pooled grouped-family histograms use common bin edges and
component-specific densities, with faint group-coloured fills and opaque
stepped upper boundaries. Set `by_family = TRUE` for separate panels.
