# Plot accumulated local effects

Plot saved ALE estimates and intervals, without recomputing predictions.

## Usage

``` r
# S3 method for class 'ale'
draw(
  object,
  ci = TRUE,
  ref_line = TRUE,
  alpha = 0.2,
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- object:

  a tibble of class `"ale"`, produced by
  [`ale()`](https://gavinsimpson.github.io/gratia/reference/ale.md).

- ci:

  logical; show the saved interval ribbon?

- ref_line:

  logical; draw a horizontal reference line at zero?

- alpha:

  Deprecated; use
  [`gratia_theme_get()`](https://gavinsimpson.github.io/gratia/reference/gratia_theme_get.md)
  theme settings instead. Numeric; opacity of the interval ribbon.

- xlab, ylab:

  optional axis labels. Defaults identify the focal covariate and
  selected parameter. The computation scale appears in the caption.
  Titles and subtitles are omitted. Use
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
  to change or remove the caption, for example
  `+ ggplot2::labs(caption = NULL)`.

- ...:

  reserved for future use.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Label layout

Pass `label_control = draw_label_control(...)` to
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) to
wrap discrete axis labels and legend text, stagger discrete tick labels,
or adjust legend and colour-bar layout. Settings apply to each panel,
including unwrapped plot lists. See
[`draw_label_control()`](https://gavinsimpson.github.io/gratia/reference/draw_label_control.md)
for the available settings.

## See also

[`ale()`](https://gavinsimpson.github.io/gratia/reference/ale.md)

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 100, seed = 2)
m <- gam(y ~ s(x2), data = df, method = "REML")
ale(m, "x2") |> draw()
```
