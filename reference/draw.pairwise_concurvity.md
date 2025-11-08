# Plot concurvity measures

Plot concurvity measures

## Usage

``` r
# S3 method for class 'pairwise_concurvity'
draw(
  object,
  title = "Smooth-wise concurvity",
  subtitle = NULL,
  caption = NULL,
  x_lab = "Term",
  y_lab = "With",
  fill_lab = "Concurvity",
  continuous_colour = NULL,
  ...
)

# S3 method for class 'overall_concurvity'
draw(
  object,
  title = "Overall concurvity",
  subtitle = NULL,
  caption = NULL,
  y_lab = "Concurvity",
  x_lab = NULL,
  bar_col = "steelblue",
  bar_fill = "steelblue",
  ...
)
```

## Arguments

- object:

  An object inheriting from class `"concurvity"`, usually the result of
  a call to
  [`model_concurvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md)
  or its abbreviated form
  [`concrvity()`](https://gavinsimpson.github.io/gratia/reference/model_concurvity.md).

- title:

  character; the plot title.

- subtitle:

  character; the plot subtitle.

- caption:

  character; the plot caption

- x_lab:

  character; the label for the x axis.

- y_lab:

  character; the label for the y axis.

- fill_lab:

  character; the label to use for the fill guide.

- continuous_colour:

  function; continuous colour (fill) scale to use.

- ...:

  arguments passed to other methods.

- bar_col:

  colour specification for the bar colour.

- bar_fill:

  colour specification for the bar fill
