# Generic plotting via `ggplot2`

Generic plotting via `ggplot2`

## Usage

``` r
draw(object, ..., label_control = NULL)
```

## Arguments

- object:

  and R object to plot.

- ...:

  arguments passed to other methods.

- label_control:

  Optional layout settings created by
  [`draw_label_control()`](https://gavinsimpson.github.io/gratia/reference/draw_label_control.md).
  These apply across drawing methods without changing their default
  appearance.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

Generic function for plotting of R objects that uses the `ggplot2`
package.

## Label layout

Pass `label_control = draw_label_control(...)` to `draw()` to wrap
discrete axis labels and legend text, stagger discrete tick labels, or
adjust legend and colour-bar layout. Settings apply to each panel,
including unwrapped plot lists. See
[`draw_label_control()`](https://gavinsimpson.github.io/gratia/reference/draw_label_control.md)
for the available settings.

## Author

Gavin L. Simpson
