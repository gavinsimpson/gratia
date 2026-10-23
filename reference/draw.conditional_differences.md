# Plot conditional differences

Display conditional differences as curves with pointwise or simultaneous
intervals and a zero reference line, or as surfaces over two grouped
numeric covariates. Each comparison is shown in a separate facet.
Surfaces map the difference to fill and, in 3D, height, with a fill
range symmetric around zero. Additional conditions supply facets for
surfaces, or colour and facets for curves, as in
[`draw.conditional_values()`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_values.md).

## Usage

``` r
# S3 method for class 'conditional_differences'
draw(
  object,
  facet_scales = "fixed",
  discrete_colour = NULL,
  discrete_fill = NULL,
  xlab = NULL,
  ylab = NULL,
  ...,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  continuous_fill = NULL,
  contour = TRUE,
  n_contour = NULL,
  ci_3d = FALSE,
  zlab = NULL
)
```

## Arguments

- object:

  an object returned by
  [`conditional_differences()`](https://gavinsimpson.github.io/gratia/reference/conditional_differences.md).

- facet_scales:

  character; should facets have the same axis scales across facets? See
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  for details. Options are: `"fixed"` (default), `"free_x"`, `"free_y"`,
  and `"free"`.

- discrete_colour:

  a suitable colour scale to be used when plotting discrete variables.

- discrete_fill:

  a suitable fill scale to be used when plotting discrete variables.

- xlab:

  character; label for the x axis of the plot.

- ylab:

  character; label for the prediction axis of a curve, or the second
  covariate axis of a surface.

- ...:

  arguments passed to
  [`draw.conditional_values()`](https://gavinsimpson.github.io/gratia/reference/draw.conditional_values.md).

- surface:

  character; `"raster"` (default), `"tile"`, or `"3d"`. Applies to
  grouped numeric conditions. Three-dimensional surfaces require the
  optional ggcube package. Irregular flat grids use rectangles bounded
  by adjacent coordinate midpoints.

- surface_3d:

  named list of view options passed to
  [`ggcube::coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.html),
  for example `list(yaw = 45, persp = FALSE)`. The default view uses
  `ggcube::light(backface_scale = 1)`. A supplied `light` replaces that
  lighting specification. The same view applies to all panels; gratia
  retains its plot theme.

- continuous_fill:

  a suitable fill scale to be used when plotting continuous variables.

- contour:

  logical; draw contour lines on surfaces? Defaults to `TRUE`.

- n_contour:

  numeric; the number of contour bins. Will result in `n_contour - 1`
  contour lines being drawn. See
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- ci_3d:

  logical; add the returned lower and upper interval surfaces when
  `surface = "3d"`? Defaults to `FALSE`.

- zlab:

  character; label for the surface fill legend and 3D height axis.
  Defaults to the prediction label, or the difference and its scale for
  conditional differences.

## Label layout

Pass `label_control = draw_label_control(...)` to
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) to
wrap discrete axis labels and legend text, stagger discrete tick labels,
or adjust legend and colour-bar layout. Settings apply to each panel,
including unwrapped plot lists. See
[`draw_label_control()`](https://gavinsimpson.github.io/gratia/reference/draw_label_control.md)
for the available settings.

## Examples

``` r
load_mgcv()
df <- data_sim("eg4", seed = 2)
m <- gam(y ~ fac + s(x2, by = fac), data = df, method = "REML")
draw(conditional_differences(m, by = "fac", condition = "x2"))
```
