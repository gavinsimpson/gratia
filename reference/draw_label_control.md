# Control label layout in gratia plots

Configure labels on axes, legends and colour bars across
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) and
[`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md)
methods. Omitted settings retain the existing appearance.

## Usage

``` r
draw_label_control(axes = list(), legend = list(), colourbar = list())
```

## Arguments

- axes:

  Named list with optional `wrap`, `n_dodge`, `angle` and `title_wrap`
  settings. `wrap` and `title_wrap` are positive integer widths in
  characters. Tick labels wrap at word boundaries, preserving explicit
  line breaks. Unbroken words are not split. `n_dodge` is the positive
  integer number of rows for horizontal-axis labels, or columns for
  vertical-axis labels. `angle` is a finite number of degrees between
  -90 and 90. Tick settings apply only to discrete axes; `title_wrap`
  applies to ordinary x and y axis titles, including continuous axes.

- legend:

  Named list with optional `wrap`, `title_wrap`, `nrow`, `ncol` and
  `byrow` settings. Wrapping widths and row/column counts are positive
  integers. `byrow` is logical. These settings apply to key-based
  legends, including colour, fill, shape and linetype legends.

- colourbar:

  Named list with optional `title_wrap`, `angle`, `width` and `height`
  settings. `title_wrap` is a positive integer width in characters;
  `angle` is between -90 and 90 degrees. Dimensions are positive,
  length-one [`grid::unit()`](https://rdrr.io/r/grid/unit.html) objects
  and refer to the displayed bar's width and height. These settings
  apply to continuous colour and fill guides.

## Value

A `draw_label_control` object.

## Details

Every setting defaults to `NULL`, retaining the method's existing
behaviour. Pass the result as `label_control` to
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) or
[`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md).
The same configuration can be reused across plot types; absent guides
are ignored. Mathematical expressions and missing labels are preserved.
Wrapping changes displayed text after scale labelling, without changing
factor levels, breaks, transformations or palettes. Numeric formatting
remains controlled by the scale's `labels` argument.

Explicit axis settings override the existing `angle` and `n_dodge`
plotting arguments on discrete axes. Hidden guides stay hidden. Native
guide types and unrelated settings are retained; later ggplot2 scale and
guide additions follow their usual precedence. Settings apply to panels
before patchwork collects their guides, including plots returned with
`wrap = FALSE`. Projected coordinate labels in 3D plots retain their
renderer's settings; their ordinary legends and colour bars can use this
control.

## See also

[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md),
[`assemble()`](https://gavinsimpson.github.io/gratia/reference/assemble.md),
[`gratia_theme_get()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)

## Examples

``` r
# Wrap categorical tick labels and stagger them across two rows.
layout <- draw_label_control(axes = list(wrap = 12, n_dodge = 2))

# Reuse one configuration for legend text and colour-bar layout.
layout <- draw_label_control(
  axes = list(wrap = 12),
  legend = list(wrap = 20, title_wrap = 20, ncol = 2),
  colourbar = list(title_wrap = 20, height = grid::unit(35, "mm"))
)

# Draw a model with the requested layout.
load_mgcv()
dat <- data_sim("eg4", n = 100, seed = 42)
model <- gam(y ~ fac + s(x2, by = fac, k = 5), data = dat, method = "REML")
draw(model, label_control = layout)
```
