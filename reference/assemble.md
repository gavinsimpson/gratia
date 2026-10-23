# Prepare plots via `ggplot2` and assembles them as a list

Generic function for assembling plot objects created from R objects,
using the `ggplot2` package.

## Usage

``` r
assemble(object, ..., label_control = NULL)

# S3 method for class 'gamm4'
assemble(object, ...)
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

A list of
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
objects.

## Label layout

Pass `label_control = draw_label_control(...)` to
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) to
wrap discrete axis labels and legend text, stagger discrete tick labels,
or adjust legend and colour-bar layout. Settings apply to each panel,
including unwrapped plot lists. See
[`draw_label_control()`](https://gavinsimpson.github.io/gratia/reference/draw_label_control.md)
for the available settings.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)

## Author

Gavin L. Simpson
