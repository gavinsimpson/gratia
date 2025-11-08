# Plot derivatives of smooths

Plot derivatives of smooths

## Usage

``` r
# S3 method for class 'derivatives'
draw(
  object,
  select = NULL,
  scales = c("free", "fixed"),
  add_change = FALSE,
  change_type = c("change", "sizer"),
  alpha = 0.2,
  change_col = "black",
  decrease_col = "#56B4E9",
  increase_col = "#E69F00",
  lwd_change = 1.5,
  ncol = NULL,
  nrow = NULL,
  guides = "keep",
  angle = NULL,
  differentiate_factor_smooths = TRUE,
  ...
)

# S3 method for class 'partial_derivatives'
draw(
  object,
  select = NULL,
  scales = c("free", "fixed"),
  alpha = 0.2,
  ncol = NULL,
  nrow = NULL,
  guides = "keep",
  angle = NULL,
  ...
)
```

## Arguments

- object:

  a fitted GAM, the result of a call to
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

- select:

  character, logical, or numeric; which smooths to plot. If `NULL`, the
  default, then all model smooths are drawn. Numeric `select` indexes
  the smooths in the order they are specified in the formula and stored
  in `object`. Character `select` matches the labels for smooths as
  shown for example in the output from `summary(object)`. Logical
  `select` operates as per numeric `select` in the order that smooths
  are stored.

- scales:

  character; should all univariate smooths be plotted with the same
  y-axis scale? If `scales = "free"`, the default, each univariate
  smooth has its own y-axis scale. If `scales = "fixed"`, a common y
  axis scale is used for all univariate smooths.

  Currently does not affect the y-axis scale of plots of the parametric
  terms.

- add_change:

  logical; should the periods of significant change be highlighted on
  the plot?

- change_type:

  character; the type of change to indicate. If `"change"`, no
  differentiation is made between periods of significant increase or
  decrease. If `"sizer"`, the periods of increase and decrease are
  differentiated in the resulting plot.

- alpha:

  numeric; alpha transparency for confidence or simultaneous interval.

- change_col, decrease_col, increase_col:

  colour specifications to use for indicating periods of change.
  `col_change` is used when `change_type = "change"`, while
  `col_decrease` and `col_increase` are used when \`change_type =
  "sizer"“.

- lwd_change:

  numeric; the `linewidth` to use for the change indicators.

- ncol, nrow:

  numeric; the numbers of rows and columns over which to spread the
  plots

- guides:

  character; one of `"keep"` (the default), `"collect"`, or `"auto"`.
  Passed to
  [`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html)

- angle:

  numeric; the angle at which the x axis tick labels are to be drawn
  passed to the `angle` argument of
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html).

- differentiate_factor_smooths:

  logical; should colour be used to differentiate the levels of the
  factors involved in random factor smooths (`bs = "fs"`) or constrained
  factor smooths (`bs = "sz"`)? If `TRUE`, the factor(s) are used to
  define groups and mapped to the colour aesthetic.

- ...:

  additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Examples

``` r
load_mgcv()
dat <- data_sim("eg1", n = 800, dist = "normal", scale = 2, seed = 42)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

## first derivative of all smooths
df <- derivatives(mod, type = "central")
draw(df)

## fixed axis scales
draw(df, scales = "fixed")
```
