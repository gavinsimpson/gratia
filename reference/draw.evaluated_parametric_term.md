# Plot estimated parametric effects

**\[deprecated\]**

Plots estimated univariate and bivariate smooths using ggplot2.

## Usage

``` r
# S3 method for class 'evaluated_parametric_term'
draw(
  object,
  ci_level = 0.95,
  constant = NULL,
  fun = NULL,
  xlab,
  ylab,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  rug = TRUE,
  position = "identity",
  response_range = NULL,
  ...
)
```

## Arguments

- object:

  an object, the result of a call to
  [`evaluate_parametric_term()`](https://gavinsimpson.github.io/gratia/reference/evaluate_parametric_term.md).

- ci_level:

  numeric between 0 and 1; the coverage of credible interval.

- constant:

  numeric; a constant to add to the estimated values of the smooth.
  `constant`, if supplied, will be added to the estimated value before
  the confidence band is computed.

- fun:

  function; a function that will be applied to the estimated values and
  confidence interval before plotting. Can be a function or the name of
  a function. Function `fun` will be applied after adding any
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

  For `evaluate_parametric_terms()`, a logical to indicate if a rug plot
  should be drawn.

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function.

- response_range:

  numeric; a vector of two values giving the range of response data for
  the guide. Used to fix plots to a common scale/range. Ignored if
  `show` is set to `"se"`.

- ...:

  arguments passed to other methods.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Author

Gavin L. Simpson
