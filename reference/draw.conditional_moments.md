# Draw conditional response moments

Draw conditional response moments

## Usage

``` r
# S3 method for class 'conditional_moments'
draw(
  object,
  facet_scales = "fixed",
  discrete_colour = NULL,
  discrete_fill = NULL,
  xlab = NULL,
  ylab = NULL,
  ...
)
```

## Arguments

- object:

  A tibble returned by
  [`conditional_moments()`](https://gavinsimpson.github.io/gratia/reference/conditional_moments.md).

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

  character; label for the y axis of the plot.

- ...:

  Additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Value

A patchwork containing one plot per moment and response/category.
