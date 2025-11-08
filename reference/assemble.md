# Prepare plots via `ggplot2` and assembles them as a list

Generic function for assembling plot objects created from R objects,
using the `ggplot2` package.

## Usage

``` r
assemble(object, ...)
```

## Arguments

- object:

  and R object to plot.

- ...:

  arguments passed to other methods.

## Value

A list of
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
objects.

## Author

Gavin L. Simpson
