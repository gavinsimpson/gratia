# Plot of residuals versus linear predictor values

Plot of residuals versus linear predictor values

## Usage

``` r
residuals_linpred_plot(
  model,
  type = c("deviance", "pearson", "response", "pit", "quantile"),
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  point_col = "black",
  point_alpha = 1,
  line_col = "red",
  seed = NULL
)
```

## Arguments

- model:

  a fitted model. Currently only class `"gam"`.

- type:

  character; type of residuals to use. One of `"deviance"`,
  `"response"`, `"pearson"`, `"pit"`, and `"quantile"` residuals are
  allowed. `"pit"` uses probability integral transform (PIT) residuals,
  which, if the model is correct should be approximately uniformly
  distributed, while `"quantile"` transforms the PIT residuals through
  application of the inverse CDF of the standard normal, and therefore
  the quantile residuals should be approximately normally distributed
  (mean = 0, sd = 1) if the model is correct. PIT and quantile residuals
  are not yet available for most families that can be handled by
  `gam()`, but most standard families are supported, e.g. those used by
  [`glm()`](https://rdrr.io/r/stats/glm.html).

- ylab:

  character or expression; the label for the y axis. If not supplied, a
  suitable label will be generated.

- xlab:

  character or expression; the label for the y axis. If not supplied, a
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

  colour used to draw points in the plots. See
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html) section
  **Color Specification**. This is passed to the individual plotting
  functions, and therefore affects the points of all plots.

- point_alpha:

  numeric; alpha transparency for points in plots.

- line_col:

  colour specification for 1:1 line.

- seed:

  integer; random seed to use for PIT or quantile residuals.
