# Internal function to draw an individual parametric effect

Internal function to draw an individual parametric effect

## Usage

``` r
draw_parametric_effect(
  object,
  ci_level = 0.95,
  ci_col = "black",
  ci_alpha = 0.2,
  line_col = "black",
  constant = NULL,
  fun = NULL,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  rug = TRUE,
  position = "identity",
  ylim = NULL,
  angle = NULL,
  factor_levels = NULL,
  ...
)
```

## Arguments

- object:

  a fitted GAM, the result of a call to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

- ci_level:

  numeric between 0 and 1; the coverage of credible interval.

- ci_col:

  colour specification for the confidence/credible intervals band.
  Affects the fill of the interval.

- ci_alpha:

  numeric; alpha transparency for confidence or simultaneous interval.

- constant:

  numeric; a constant to add to the estimated values of the smooth.
  `constant`, if supplied, will be added to the estimated value before
  the confidence band is computed.

- fun:

  function; a function that will be applied to the estimated values and
  confidence interval before plotting. Can be a function or the name of
  a function. Function `fun` will be applied after adding any
  `constant`, if provided.

- xlab:

  character or expression; the label for the x axis. If not supplied, a
  suitable label will be generated from `object`.

- ylab:

  character or expression; the label for the y axis. If not supplied, a
  suitable label will be generated from `object`.

- title:

  character or expression; the title for the plot. See
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- subtitle:

  character or expression; the subtitle for the plot. See
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- caption:

  character or expression; the plot caption. See
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- rug:

  logical; draw a rug plot at the bottom of each plot for 1-D smooths or
  plot locations of data for higher dimensions.

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function.

- angle:

  numeric; the angle at which the x axis tick labels are to be drawn
  passed to the `angle` argument of
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html).

- factor_levels:

  list; a named list of factor levels

- ...:

  additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
