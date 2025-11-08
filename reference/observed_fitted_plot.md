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
  point_col = "black",
  point_alpha = 1
)
```

## Arguments

- model:

  a fitted model. Currently only class `"gam"`.

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
