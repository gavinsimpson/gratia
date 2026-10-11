# Plot the result of a call to `smooth_estimates()`

Plot the result of a call to
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)

## Usage

``` r
# S3 method for class 'smooth_estimates'
draw(
  object,
  constant = NULL,
  fun = NULL,
  contour = TRUE,
  grouped_by = FALSE,
  contour_col = "black",
  n_contour = NULL,
  ci_alpha = 0.2,
  ci_col = "black",
  smooth_col = "black",
  resid_col = "steelblue3",
  decrease_col = "#56B4E9",
  increase_col = "#E69F00",
  change_lwd = 1.75,
  partial_match = FALSE,
  discrete_colour = NULL,
  discrete_fill = NULL,
  continuous_colour = NULL,
  continuous_fill = NULL,
  angle = NULL,
  ylim = NULL,
  crs = NULL,
  default_crs = NULL,
  lims_method = "cross",
  caption = TRUE,
  surface = c("raster", "tile", "3d"),
  surface_3d = list(),
  back_transform = FALSE,
  ...
)
```

## Arguments

- object:

  a fitted GAM, the result of a call to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

- constant:

  numeric; a constant to add to the estimated values of the smooth.
  `constant`, if supplied, will be added to the estimated value before
  the confidence band is computed.

- fun:

  function; a function that will be applied to the estimated values and
  confidence interval before plotting. Can be a function or the name of
  a function. Function `fun` will be applied after adding any
  `constant`, if provided.

- contour:

  logical; should contours be draw on the plot using
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- grouped_by:

  logical; should factor by smooths be drawn as one panel per level of
  the factor (`FALSE`, the default), or should the individual smooths be
  combined into a single panel containing all levels (`TRUE`)?

- contour_col:

  colour specification for contour lines.

- n_contour:

  numeric; the number of contour bins. Will result in `n_contour - 1`
  contour lines being drawn. See
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- ci_alpha:

  numeric; alpha transparency for confidence or simultaneous interval.

- ci_col:

  colour specification for the confidence/credible intervals band.
  Affects the fill of the interval.

- smooth_col:

  colour specification for the smooth line.

- resid_col:

  colour specification for the partial residuals.

- decrease_col, increase_col:

  colour specifications to use for indicating periods of change.
  `col_change` is used when `change_type = "change"`, while
  `col_decrease` and `col_increase` are used when \`change_type =
  "sizer"“.

- change_lwd:

  numeric; the value to set the `linewidth` to in
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html),
  used to represent the periods of change.

- partial_match:

  logical; should smooths be selected by partial matches with `select`?
  If `TRUE`, `select` can only be a single string to match against.

- discrete_colour:

  a suitable colour scale to be used when plotting discrete variables.

- discrete_fill:

  a suitable fill scale to be used when plotting discrete variables.

- continuous_colour:

  a suitable colour scale to be used when plotting continuous variables.

- continuous_fill:

  a suitable fill scale to be used when plotting continuous variables.

- angle:

  numeric; the angle at which the x axis tick labels are to be drawn
  passed to the `angle` argument of
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html).

- ylim:

  numeric; vector of y axis limits to use all *all* panels drawn.

- crs:

  the coordinate reference system (CRS) to use for the plot. All data
  will be projected into this CRS. See
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  for details.

- default_crs:

  the coordinate reference system (CRS) to use for the non-sf layers in
  the plot. If left at the default `NULL`, the CRS used is 4326 (WGS84),
  which is appropriate for spline-on-the-sphere smooths, which are
  parameterized in terms of latitude and longitude as coordinates. See
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  for more details.

- lims_method:

  character; affects how the axis limits are determined. See
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).
  Be careful; in testing of some examples, changing this to
  `"orthogonal"` for example with the chlorophyll-a example from Simon
  Wood's GAM book quickly used up all the RAM in my test system and the
  OS killed R. This could be incorrect usage on my part; right now the
  grid of points at which SOS smooths are evaluated (if not supplied by
  the user) can produce invalid coordinates for the corners of tiles as
  the grid is generated for tile centres without respect to the spacing
  of those tiles.

- caption:

  logical; show the smooth type in the caption of each plot?

- surface:

  character; one of `"raster"` (the default), `"tile"`, or `"3d"`.
  Raster surfaces keep large PDF plots compact; tiles are borderless
  vector rectangles. Three-dimensional surfaces use the optional
  `ggcube` package, with the displayed partial effect as height and
  fill. Numeric smooth and parametric surfaces, including conditioning
  facets and soap films, are supported. Curves and factor estimates keep
  their usual display; spherical smooths retain their geographic
  rendering. Covariate scale changes that produce irregular spacing use
  rectangles bounded by adjacent coordinate midpoints for flat surfaces.

- surface_3d:

  named list of view options passed to
  [`ggcube::coord_3d()`](https://matthewkling.github.io/ggcube/reference/coord_3d.html),
  for example `list(yaw = 45, persp = FALSE)`. The default
  [`list()`](https://rdrr.io/r/base/list.html) uses the default view
  with `ggcube::light(backface_scale = 1)`, preserving equal horizontal
  units for isotropic smooths. Supply `light = ggcube::light(...)` in
  this list to override lighting; `ggcube::light(method = "none")`
  disables it. An explicitly supplied `light` replaces the entire
  default light. Only the coordinate system is used; ggcube's theme
  settings are not applied. These options affect all 3D panels in a
  call. For different views per term, modify individual plots returned
  by
  [`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md).
  The `dist` entry here controls perspective; the top-level `dist`
  argument controls masking.

- back_transform:

  logical; plot covariates on their original scale? Defaults to `FALSE`,
  using model coordinates such as `log(x)` for a term containing
  `log(x)`. With `TRUE`, recover the raw covariates, generate a grid on
  their original scale and evaluate the model expressions forwards.
  Applies to axes, rugs, partial-residual locations and conditioning
  facets, including tensor products and parametric interactions. Each
  smooth axis must depend on one distinct original model covariate.
  Functions such as `foo(x)` are supported when their raw inputs and
  functions can be recovered; no inverse function is needed. Missing raw
  data or unavailable functions produce an error; supply `data` or
  `envir` as appropriate. With supplied prediction `data`, unavailable
  raw fitting observations omit rugs and partial residuals; prediction
  rows are never used as observations. For precomputed smooth estimates,
  prepare raw grids with
  `smooth_estimates(model, back_transform = TRUE)` before drawing with
  `back_transform = TRUE`. Raw inputs supplied through `data` also
  retain their coordinates. Transformed-only estimates cannot be
  relabelled on the raw scale, including grids carrying typical values
  from `include_all = TRUE`. Transformed soap-film boundaries lack
  retained raw coordinates and must be drawn with
  `back_transform = FALSE`. Parametric estimates retain both scales;
  multi-column basis expansions retain raw axes on either setting. Does
  not transform estimated effects or their intervals; use `fun` for
  that.

- ...:

  additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Examples

``` r
load_mgcv()
# example data
df <- data_sim("eg1", seed = 21)
# fit GAM
m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = df, method = "REML")
# plot all of the estimated smooths
sm <- smooth_estimates(m)
draw(sm)

m_log <- gam(y ~ s(log(x1), k = 6), data = df)
draw(smooth_estimates(m_log, back_transform = TRUE), back_transform = TRUE)
#> Error in map(smooths, eval_smooth, model = object, n = n, n_2d = n_2d,     n_3d = n_3d, n_4d = n_4d, data = data, unconditional = unconditional,     frequentist = frequentist, overall_uncertainty = overall_uncertainty,     dist = dist, clip = clip, back_transform = back_transform): i In index: 1.
#> Caused by error in `recover_raw_data()`:
#> ! Cannot recover raw model covariates.
#> x The fitting data are unavailable.
#> i Supply reference `data` and, if needed, `envir`; stored transformed values
#>   cannot be inverted automatically.
#> Caused by error in `doTryCatch()`:
#> ! The fitting data are unavailable.
# evaluate smooth of `x2`
sm <- smooth_estimates(m, select = "s(x2)")
# plot it
draw(sm)


# customising some plot elements
draw(sm, ci_col = "steelblue", smooth_col = "forestgreen", ci_alpha = 0.3)


# Add a constant to the plotted smooth
draw(sm, constant = coef(m)[1])


# Adding change indicators to smooths based on derivatives of the smooth
d <- derivatives(m, n = 100) # n to match smooth_estimates()

smooth_estimates(m) |>
  add_sizer(derivatives = d, type = "sizer") |>
  draw()


# Render a bivariate smooth as vector tiles
m_surface <- gam(y ~ s(x0, x1), data = df, method = "REML")
draw(smooth_estimates(m_surface, n_2d = 20), surface = "tile")
```
