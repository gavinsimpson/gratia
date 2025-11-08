# Plot basis functions

Plots basis functions using ggplot2

## Usage

``` r
# S3 method for class 'basis'
draw(
  object,
  legend = FALSE,
  labeller = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ncol = NULL,
  nrow = NULL,
  angle = NULL,
  guides = "keep",
  contour = FALSE,
  n_contour = 10,
  contour_col = "black",
  ...
)
```

## Arguments

- object:

  an object, the result of a call to
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md).

- legend:

  logical; should a legend by drawn to indicate basis functions?

- labeller:

  a labeller function with which to label facets. The default is to use
  [`ggplot2::label_both()`](https://ggplot2.tidyverse.org/reference/labellers.html).

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

- ncol, nrow:

  numeric; the numbers of rows and columns over which to spread the
  plots

- angle:

  numeric; the angle at which the x axis tick labels are to be drawn
  passed to the `angle` argument of
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html).

- guides:

  character; one of `"keep"` (the default), `"collect"`, or `"auto"`.
  Passed to
  [`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html)

- contour:

  logical; should contours be draw on the plot using
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- n_contour:

  numeric; the number of contour bins. Will result in `n_contour - 1`
  contour lines being drawn. See
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- contour_col:

  colour specification for contour lines.

- ...:

  arguments passed to other methods. Not used by this method.

## Value

A `patchwork` object.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 400, seed = 42)
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")

bf <- basis(m)
draw(bf)


bf <- basis(m, "s(x2)")
draw(bf)
```
