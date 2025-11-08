# Histogram of model residuals

Histogram of model residuals

## Usage

``` r
residuals_hist_plot(
  model,
  type = c("deviance", "pearson", "response", "pit", "quantile"),
  n_bins = c("sturges", "scott", "fd"),
  ylab = NULL,
  xlab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
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

- n_bins:

  character or numeric; either the number of bins or a string indicating
  how to calculate the number of bins.

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

- seed:

  integer; random seed to use for PIT or quantile residuals.
