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
  term_info = NULL,
  contour = TRUE,
  contour_col = "black",
  n_contour = NULL,
  surface = "raster",
  surface_3d = list(),
  ci_3d = FALSE,
  ci_3d_alpha = 0.15,
  ci_3d_col = "grey60",
  continuous_fill = NULL,
  discrete_colour = NULL,
  discrete_fill = NULL,
  axis_info = NULL,
  from_raw = FALSE,
  back_transform = FALSE,
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
  a function. For simultaneous bands it must be monotone over the entire
  band; endpoint transformation alone does not preserve non-monotone
  band coverage. Function `fun` will be applied after adding any
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

- term_info:

  Internal per-term covariate and plotting metadata.

- contour:

  logical; should contours be draw on the plot using
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- contour_col:

  colour specification for contour lines.

- n_contour:

  numeric; the number of contour bins. Will result in `n_contour - 1`
  contour lines being drawn. See
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

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

- ci_3d:

  logical; add lower and upper interval surfaces to numeric smooth and
  parametric effect panels when `surface = "3d"`? Defaults to `FALSE`.
  Applies across supported model classes and conditioning facets. Other
  panel types, including standard errors, retain their usual display.
  Smooth bounds follow `interval`; parametric bounds remain pointwise.

- ci_3d_alpha:

  numeric between 0 and 1; opacity of both interval surfaces,
  independent of `ci_alpha`. Defaults to `0.15`.

- ci_3d_col:

  one R colour; fill of both interval surfaces, independent of `ci_col`
  and the estimate's continuous fill scale. Defaults to `"grey60"`.

- continuous_fill:

  a suitable fill scale to be used when plotting continuous variables.

- discrete_colour:

  a suitable colour scale to be used when plotting discrete variables.

- discrete_fill:

  a suitable fill scale to be used when plotting discrete variables.

- axis_info:

  Internal per-term mappings between model and raw coordinates.

- from_raw:

  Internal logical; are the supplied coordinates on the raw scale?

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
