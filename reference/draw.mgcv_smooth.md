# Plot basis functions

Plots basis functions using ggplot2

## Usage

``` r
# S3 method for class 'mgcv_smooth'
draw(
  object,
  legend = FALSE,
  use_facets = TRUE,
  labeller = NULL,
  xlab,
  ylab,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  angle = NULL,
  ...
)
```

## Arguments

- object:

  an object, the result of a call to
  [`basis()`](https://gavinsimpson.github.io/gratia/reference/basis.md).

- legend:

  logical; should a legend by drawn to indicate basis functions?

- use_facets:

  logical; for factor by smooths, use facets to show the basis functions
  for each level of the factor? If `FALSE`, a separate ggplot object
  will be created for each level and combined using
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
  **Currently ignored**.

- labeller:

  a labeller function with which to label facets. The default is to use
  [`ggplot2::label_both()`](https://ggplot2.tidyverse.org/reference/labellers.html).

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

- angle:

  numeric; the angle at which the x axis tick labels are to be drawn
  passed to the `angle` argument of
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html).

- ...:

  arguments passed to other methods. Not used by this method.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg4", n = 400, seed = 42)

bf <- basis(s(x0), data = df)
draw(bf)


bf <- basis(s(x2, by = fac, bs = "bs"), data = df)
draw(bf)
```
