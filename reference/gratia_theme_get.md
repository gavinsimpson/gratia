# Native themes for gratia plots

Gratia plots use ordinary ggplot2 themes. Component settings are read
when the plot is built, so they can also be changed after calling
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md).

## Usage

``` r
gratia_theme_get()

gratia_theme_set(theme = NULL)

gratia_theme_update(...)
```

## Arguments

- theme:

  A ggplot2 theme, or `NULL` to clear the package override.

- ...:

  Settings passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

`gratia_theme_get()` returns a theme or `NULL`. The setter and updater
invisibly return the previous override.

## Details

`gratia_theme_get()` returns the override stored in
`options("gratia.plot.theme")`. It does not include ggplot2's active
theme. The override is attached to each plot when it is constructed.
Later changes to this option affect new plots. Use
[`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html)
for a temporary override. Unspecified settings inherit ggplot2's active
theme as usual.

Component elements accept
[`ggplot2::element_geom()`](https://ggplot2.tidyverse.org/reference/element.html).
The main elements are `geom.gratia.smooth`, `geom.gratia.effect`,
`geom.gratia.derivative`, `geom.gratia.sample`, `geom.gratia.interval`,
`geom.gratia.partial.residual`, `geom.gratia.increase`,
`geom.gratia.decrease`, `geom.gratia.change`,
`geom.gratia.diagnostic.point`, `geom.gratia.reference`,
`geom.gratia.rug`, `geom.gratia.contour`, `geom.gratia.boundary`,
`geom.gratia.observed`, `geom.gratia.replicated`, `geom.gratia.chain`,
`geom.gratia.histogram`, `geom.gratia.rootogram.bar`,
`geom.gratia.rootogram.fitted`, and `geom.gratia.concurvity.bar`. Other
elements are `geom.gratia.curve` (the parent of smooth, effect,
derivative, sample and basis), `geom.gratia.basis`,
`geom.gratia.surface`, `geom.gratia.diagnostic.boxplot`,
`geom.gratia.residual.covariate.point`,
`geom.gratia.diagnostic.reference`, `geom.gratia.chain.reference`,
`geom.gratia.correlation.reference`, `geom.gratia.histogram.outline`,
`geom.gratia.rootogram.reference`, `geom.gratia.rootogram.warning`,
`geom.gratia.correlation`, and `geom.gratia.trend`.

Opacity is set separately, using numeric elements between zero and one:
`gratia.alpha.interval`, `gratia.alpha.partial.residual`,
`gratia.alpha.diagnostic.point`, `gratia.alpha.rug`,
`gratia.alpha.sample`, `gratia.alpha.observed`,
`gratia.alpha.replicated`, `gratia.alpha.chain`,
`gratia.alpha.histogram`, and `gratia.alpha.surface.interval`. More
specific elements, such as `geom.gratia.smooth.interval`,
`geom.gratia.diagnostic.interval`, `geom.gratia.surface.interval`,
`geom.gratia.surface.rug`, and their `gratia.alpha.*` counterparts,
override the general component setting.

Standard fields such as `colour`, `fill`, `linewidth`, `pointsize` and
`pointshape` set component defaults. Fields such as `ink` and
`borderwidth` follow the parent ggplot2 geom's interpretation.

Default component settings preserve the existing plot appearance. Mapped
aesthetics retain their scales. Explicit aesthetic arguments to plotting
functions are deprecated but remain fixed overrides; use themes instead.
Palette settings use `palette.colour.discrete`, `palette.fill.discrete`,
`palette.colour.continuous`, and `palette.fill.continuous`. Complete
scale objects can still be supplied through the existing scale
arguments.

For a single ggplot, add a theme with `+`. For a patchwork returned by
[`draw()`](https://gavinsimpson.github.io/gratia/reference/draw.md) or
[`appraise()`](https://gavinsimpson.github.io/gratia/reference/appraise.md),
use `&` to affect every panel. Plots delegated to bayesplot retain that
package's styling.

## Examples

``` r
library("ggplot2")
old <- gratia_theme_set(theme(
  geom.gratia.smooth = element_geom(colour = "navy"),
  gratia.alpha.interval = 0.15
))
gratia_theme_update(geom.gratia.contour = element_geom(colour = "grey40"))
gratia_theme_set(old)

pal <- gratia_palette("tol_banded_16")
theme(palette.colour.discrete = pal, palette.fill.discrete = pal)
#> <theme> List of 2
#>  $ palette.colour.discrete:function (n)  
#>  $ palette.fill.discrete  :function (n)  
#>  @ complete: logi FALSE
#>  @ validate: logi TRUE
```
