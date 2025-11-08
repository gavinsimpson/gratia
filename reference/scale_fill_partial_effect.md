# Default diverging red-blue colour palette for partial effects

Default diverging red-blue colour palette for partial effects

## Usage

``` r
scale_fill_partial_effect(
  name = "Partial effect",
  ...,
  na.value = "grey50",
  guide = "colourbar",
  direction = -1
)
```

## Arguments

- name:

  The name of the scale. Used as the legend title. If `NULL` the legend
  title will be omitted.

- ...:

  arguments passed to
  [`ggplot2::continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html).

- na.value:

  Missing values will be replaced with this value (colour).

- guide:

  A function used to create a guide or its name. See
  [`ggplot2::guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
  for more information.

- direction:

  Sets the order of colours in the scale. If `1`, the default, colours
  are as output by
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html).
  If `-1`, the order of colours is reversed.
