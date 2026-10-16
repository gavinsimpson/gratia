# Plot of residuals versus linear predictor values

Plot of residuals versus linear predictor values

## Usage

``` r
residuals_linpred_plot(
  model,
  type = NULL,
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  point_col = "grey20",
  point_alpha = 1,
  line_col = "red",
  seed = NULL,
  by_family = NULL,
  rank = NULL,
  conditioning = NULL,
  ...,
  n_dharma = 250L
)
```

## Arguments

- model:

  a fitted model supported by
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- type:

  character or `NULL`; residual type: `"deviance"`, `"pearson"`,
  `"response"`, `"pit"`, `"quantile"`, `"dharma"`, or `"surrogate"` (for
  [`ocat()`](https://rdrr.io/pkg/mgcv/man/ocat.html) or
  [`multinom()`](https://rdrr.io/pkg/mgcv/man/multinom.html) GAMs).
  Grouped models automatically use quantile residuals when all active
  components are supported, otherwise all use deviance residuals with a
  message. Ordinary models retain deviance residuals by default. See
  [`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md)
  for distributional support.

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

- line_col:

  Deprecated; use
  [`gratia_theme_get()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  theme settings instead. Colour specification for 1:1 line.

- seed:

  integer; random seed to use for PIT, quantile, surrogate or
  DHARMa-style residuals.

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

- rank:

  logical or `NULL`; use within-component ranks on the horizontal axis
  of residual-versus-predictor plots. The default ranks pooled grouped
  plots and retains original predictors in facets. `TRUE` or `FALSE`
  overrides this independently of the residual type and layout. Only
  finite observations included in each component determine its ranks;
  average ties are used.

- conditioning:

  character or `NULL`; `"conditional"` holds fitted random effects
  fixed, `"marginal"` averages over new effects, and `"gam_component"`
  uses the extracted GAM. Availability and defaults depend on the model;
  see
  [diagnostic_support](https://gavinsimpson.github.io/gratia/reference/diagnostic_support.md).

- ...:

  optional arguments passed to model preparation, such as the sdmTMB
  `field_method`.

- n_dharma:

  positive integer; number of response simulations for
  `type = "dharma"`. This is separate from `n_simulate`, which controls
  traditional QQ reference calculations. Defaults to 250.

## Details

The default subtitle identifies the response family. Conditioning and
field choices appear in the caption where relevant. Supply `caption` to
replace this text, or `caption = ""` to omit it. The scalable version
uses the same labels.
