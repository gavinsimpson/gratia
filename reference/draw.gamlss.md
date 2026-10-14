# Plot smooths of a GAMLSS model estimated by `GJRM::gamlss`

Provides a
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md)
method for GAMLSS (distributional GAMs) fitted by
[`GJRM::gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html).

## Usage

``` r
# S3 method for class 'gamlss'
draw(
  object,
  scales = c("free", "fixed"),
  ncol = NULL,
  nrow = NULL,
  guides = "keep",
  widths = NULL,
  heights = NULL,
  ...,
  n = 100,
  envir = NULL,
  select = NULL,
  parametric = NULL,
  terms = NULL,
  partial_match = FALSE,
  wrap = TRUE
)
```

## Arguments

- object:

  a model, fitted by
  [`GJRM::gamlss()`](https://rdrr.io/pkg/GJRM/man/gamlss.html)

- scales:

  character; should all univariate smooths be plotted with the same
  y-axis scale? If `scales = "free"`, the default, each univariate
  smooth has its own y-axis scale. If `scales = "fixed"`, a common y
  axis scale is used for all univariate smooths.

  Currently does not affect the y-axis scale of plots of the parametric
  terms.

- ncol, nrow:

  numeric; the numbers of rows and columns over which to spread the
  plots

- guides:

  character; one of `"keep"` (the default), `"collect"`, or `"auto"`.
  Passed to
  [`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html)

- widths, heights:

  The relative widths and heights of each column and row in the grid.
  Will get repeated to match the dimensions of the grid. If there is
  more than 1 plot and `widths = NULL`, the value of `widths` will be
  set internally to `widths = 1` to accommodate plots of smooths that
  use a fixed aspect ratio.

- ...:

  arguments passed to
  [`draw.gam()`](https://gavinsimpson.github.io/gratia/reference/draw.gam.md)

- n:

  numeric; the number of points over the range of the covariate at which
  to evaluate a univariate smooth.

- envir:

  an optional environment supplying functions and constants used in
  model expressions. The available model formula environment is used
  when `NULL`. Covariate observations should be supplied in `data`.

- select:

  Smooth selection across all equations, using the labels from
  [`smooths()`](https://gavinsimpson.github.io/gratia/reference/smooths.md),
  or numeric or logical indices in equation and smooth order. Repeated
  labels receive suffixes such as `s.1(x)`; existing distinct labels are
  retained. `partial_match` uses these model-wide labels.

- parametric:

  logical or `NULL`; plot parametric terms also? The default, `NULL`,
  includes parametric terms when `select = NULL`, but excludes them when
  smooths are selected explicitly. Use `TRUE` to include parametric
  terms regardless of `select`, or `FALSE` to draw only smooths (the
  previous default). The `terms` argument selects which parametric
  effects are plotted; `select` selects only smooths.

- terms:

  character; which model parametric terms should be drawn? The Default
  of `NULL` will plot all parametric terms that can be drawn.
  Interactions show their individual contributions to the linear
  predictor; main effects and intercepts are not added. Multivariate
  parametric terms use the grid and surface controls below, as described
  in
  [`parametric_effects()`](https://gavinsimpson.github.io/gratia/reference/parametric_effects.md).

- partial_match:

  logical; in the case of character `select`, should `select` match
  partially against `smooths`? If `partial_match = TRUE`, `select` must
  only be a single string, a character vector of length 1.

- wrap:

  Logical; return an assembled patchwork when `TRUE` (the default), or a
  flat named list of plots when `FALSE`.

## Note

For models with multiple linear predictors, smooth plot subtitles
identify the distributional parameter using the parent model's margin
and equation order, rather than the working family of the embedded GAMs.
Single-predictor models omit these automatic labels. Existing smooth
labels, selection, by-variable information and custom subtitles are
retained.

## Examples

``` r
if (suppressPackageStartupMessages(require("GJRM", quietly = TRUE))) {
  # follow example from ?GJRM::gamlss
  load_mgcv()
  suppressPackageStartupMessages(library("GJRM"))
  set.seed(0)
  n <- 100
  x1 <- round(runif(n))
  x2 <- runif(n)
  x3 <- runif(n)
  f1 <- function(x) cos(pi * 2 * x) + sin(pi * x)
  y1 <- -1.55 + 2 * x1 + f1(x2) + rnorm(n)
  dataSim <- data.frame(y1, x1, x2, x3)

  eq_mu <- y1 ~ x1 + s(x2)
  eq_s <- ~ s(x3, k = 6)
  fl <- list(eq_mu, eq_s)
  m <- gamlss(fl, data = dataSim)

  draw(m)
}
```
