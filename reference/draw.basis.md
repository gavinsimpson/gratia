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
  contour = TRUE,
  n_contour = 10,
  contour_col = "black",
  ...,
  variables = NULL
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

  Deprecated; use
  [`gratia_theme_get()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  theme settings instead. Colour specification for contour lines.

- ...:

  arguments passed to other methods. Not used by this method.

- variables:

  Character vector giving every covariate of the selected smooths
  exactly once, in plotting order: x, y, then the conditioning covariate
  for 3D smooths, or facet rows and columns for 4D smooths. When
  supplied, selected smooths must use the same covariates. Use fitted
  coordinate names, such as `"log(x)"` for a transformed covariate. The
  default, `NULL`, favours a two-variable tensor margin for x and y,
  otherwise retaining fitted order. Grid resolution follows this order.
  Supplied `data` are evaluated unchanged. Drawing saved estimates can
  change the order but uses only points already evaluated; evaluate
  again to change grid resolution or conditioning values. Continuous
  covariates must precede factors. Spherical smooths retain their
  latitude/longitude convention and do not support reordering.

## Value

A `patchwork` object.

## Details

For smooths of three or four covariates, each basis function has its own
plot. The first two covariates define the surface axes. Three-covariate
smooths have wrapped facets for the third covariate; four-covariate
smooths have the third covariate in rows and the fourth in columns.
`ncol` and `nrow` control the outer arrangement of plots. Fill scales
share a range within each smooth. Use `variables` in
[`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md) to
choose the axes before grid generation, or here to rearrange existing
evaluations without resampling. Surface plots include contour lines by
default, matching
[`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md).
Use `contour = FALSE` to draw surfaces without contour lines.

Factor-by bases from smooth specifications are drawn once for the whole
factor-by set, using evaluations from all available levels. For fitted
models, `basis(model)` returns coefficient-weighted contributions, which
remain separate for each level. Use `basis(model, unweighted = TRUE)` to
draw the shared basis once. Subsetting the basis before drawing
restricts the plot to the retained evaluations and basis functions.

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


# A single shared basis for a factor-by set
df_by <- data_sim("eg4", n = 400, seed = 42)
m_by <- gam(y ~ fac + s(x2, by = fac), data = df_by)
draw(basis(m_by, unweighted = TRUE))


# A three-covariate basis, evaluated at two conditioning values
m3 <- gam(y ~ s(x0, x1, x2, k = 12), data = df)
bf3 <- basis(m3, variables = c("x1", "x2", "x0"),
  n_2d = 15, n_3d = 2)
draw(bf3, ncol = 3)


# Four covariates: conditioning values form facet rows and columns
m4 <- gam(y ~ s(x0, x1, x2, x3, k = 20), data = df)
bf4 <- basis(m4, n_2d = 15, n_4d = 2)
draw(bf4, ncol = 3)
```
