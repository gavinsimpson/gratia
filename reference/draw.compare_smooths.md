# Plot comparisons of smooths

Plot comparisons of smooths

## Usage

``` r
# S3 method for class 'compare_smooths'
draw(object, ncol = NULL, nrow = NULL, guides = "collect", ...)
```

## Arguments

- object:

  of class `"compare_smooths"`, the result of a call to
  [`compare_smooths()`](https://gavinsimpson.github.io/gratia/reference/compare_smooths.md).

- ncol, nrow:

  numeric; the numbers of rows and columns over which to spread the
  plots

- guides:

  character; one of `"keep"` (the default), `"collect"`, or `"auto"`.
  Passed to
  [`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html)

- ...:

  additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).
