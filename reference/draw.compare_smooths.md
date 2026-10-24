# Plot comparisons of smooths

Plot comparisons of smooths

## Usage

``` r
# S3 method for class 'compare_smooths'
draw(
  object,
  ncol = NULL,
  nrow = NULL,
  guides = "collect",
  select = NULL,
  partial_match = FALSE,
  show = c("models", "difference"),
  pair = NULL,
  ci_level = 0.95,
  surface = c("raster", "tile"),
  contour = TRUE,
  n_contour = NULL,
  continuous_fill = NULL,
  ...
)
```

## Arguments

- object:

  of class `"compare_smooths"`, the result of a call to
  [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md).

- ncol, nrow:

  numeric; the numbers of rows and columns over which to spread the
  plots

- guides:

  character; guide handling for the combined plot, passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  The default is `"collect"`.

- select, partial_match:

  select smooths from the comparison, using the conventions of
  [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md).
  Numeric and logical selections follow the order of unique smooth
  labels in `object`.

- show:

  character; `"models"` (default) overlays one-variable smooths and
  draws two-variable smooths in side-by-side model facets with fixed
  axes and a shared fill scale. `"difference"` draws the estimated
  difference for selected two-variable smooths, using a fill scale
  centred on zero.

- pair:

  two model names or integer indices in input order. In difference mode,
  subtract the second model from the first. With exactly two models, the
  default uses their input order; with more models, supply `pair`.

- ci_level:

  numeric; one finite value strictly between 0 and 1, giving the
  coverage of the pointwise normal intervals for one-variable smooths.
  The default is `0.95`. Use `ci_level` to change the interval level;
  `coverage` and `level` are not accepted, even when `ci_level` is also
  supplied.

- surface:

  character; `"raster"` or `"tile"` for surface displays. Irregular
  numeric grids use rectangles bounded by coordinate midpoints.

- contour:

  logical; add contour lines? These require two numeric axes and at
  least one grid cell with four finite corner values that are not all
  equal.

- n_contour:

  numeric; the number of contour bins. Will result in `n_contour - 1`
  contour lines being drawn. See
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- continuous_fill:

  a suitable fill scale to be used when plotting continuous variables.

- ...:

  arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Details

Difference plots match both coordinates and any retained conditioning
covariates. Locations missing from either model, and missing estimates,
remain missing. Multiple conditioning values are drawn in separate
panels. No interval is calculated for the difference: estimates from
models fitted to the same observations can be correlated, and their
separate covariance matrices do not supply the covariance between
models. See
[`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md)
for the scale and centring of the compared smooth contributions.

## Label layout

Pass `label_control = draw_label_control(...)` to
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) to
wrap discrete axis labels and legend text, stagger discrete tick labels,
or adjust legend and colour-bar layout. Settings apply to each panel,
including unwrapped plot lists. See
[`draw_label_control()`](https://gavinsimpson.github.io/gratia/reference/draw_label_control.md)
for the available settings.
