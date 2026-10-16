# Plot of fitted against observed response values

Plot of fitted against observed response values

## Usage

``` r
observed_fitted_plot(
  model,
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  point_col = "grey20",
  point_alpha = 1,
  by_family = NULL,
  conditioning = NULL,
  ...
)
```

## Arguments

- model:

  a fitted model supported by
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

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

- point_col:

  Deprecated; use
  [`gratia_theme_get()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  theme settings instead. Constant point colour when components are not
  mapped to colour. Pooled grouped diagnostics instead colour points by
  component.

- point_alpha:

  Deprecated; use
  [`gratia_theme_get()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  theme settings instead. Numeric; alpha transparency for points in
  plots.

- by_family:

  logical or `NULL`; separate indexed grouped-family components into
  facets. The default pools quantile residuals with component colours
  and facets traditional residuals. `TRUE` and `FALSE` explicitly select
  the layout. In
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
  observed-versus-fitted values always use component facets. Standalone
  `observed_fitted_plot()` facets by default and permits explicit
  pooling. Ignored for ordinary models.

- conditioning:

  character or `NULL`; `"conditional"` holds fitted random effects
  fixed, `"marginal"` averages over new effects, and `"gam_component"`
  uses the extracted GAM. Availability and defaults depend on the model;
  see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- ...:

  optional arguments passed to model preparation, such as the sdmTMB
  `field_method`.
