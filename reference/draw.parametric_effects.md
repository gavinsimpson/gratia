# Plot estimated effects for model parametric terms

Plot estimated effects for model parametric terms

## Usage

``` r
# S3 method for class 'parametric_effects'
draw(
  object,
  scales = c("free", "fixed"),
  ci_level = 0.95,
  ci_col = "black",
  ci_alpha = 0.2,
  line_col = "black",
  constant = NULL,
  fun = NULL,
  rug = TRUE,
  position = "identity",
  angle = NULL,
  ...,
  ncol = NULL,
  nrow = NULL,
  guides = "keep",
  contour = TRUE,
  contour_col = "black",
  n_contour = NULL,
  geom = c("raster", "tile"),
  continuous_fill = NULL,
  discrete_colour = NULL,
  discrete_fill = NULL
)
```

## Arguments

- object:

  A `parametric_effects` object, optionally nested.

- scales:

  Use a common effect-axis range for curves and points (`fixed`), or
  separate ranges (`free`). Surface covariate axes are not affected.

- ci_level:

  numeric between 0 and 1; the coverage of credible interval.

- ci_col:

  colour specification for the confidence/credible intervals band.
  Affects the fill of the interval.

- ci_alpha:

  numeric; alpha transparency for confidence or simultaneous interval.

- line_col:

  colour specification used for regression lines of linear continuous
  terms.

- constant:

  numeric; a constant to add to the estimated values of the smooth.
  `constant`, if supplied, will be added to the estimated value before
  the confidence band is computed.

- fun:

  function; a function that will be applied to the estimated values and
  confidence interval before plotting. Can be a function or the name of
  a function. Function `fun` will be applied after adding any
  `constant`, if provided.

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

- ...:

  additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

- ncol, nrow:

  numeric; the numbers of rows and columns over which to spread the
  plots

- guides:

  character; one of `"keep"` (the default), `"collect"`, or `"auto"`.
  Passed to
  [`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html)

- contour:

  logical; should contours be draw on the plot using
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- contour_col:

  colour specification for contour lines.

- n_contour:

  numeric; the number of contour bins. Will result in `n_contour - 1`
  contour lines being drawn. See
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- geom:

  character; either `"raster"` (the default) or `"tile"` for flat smooth
  surfaces, including faceted and soap-film plots. Raster rendering
  keeps large PDF plots compact; tiles are individual borderless
  rectangles that remain vector elements in PDF and SVG output,
  potentially increasing file size. Other plot types, including
  spherical smooths, are unchanged.

- continuous_fill:

  a suitable fill scale to be used when plotting continuous variables.

- discrete_colour:

  a suitable colour scale to be used when plotting discrete variables.

- discrete_fill:

  a suitable fill scale to be used when plotting discrete variables.
