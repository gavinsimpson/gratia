# Discrete palettes for gratia plots

Create a palette function for ggplot2 theme settings. The default
preserves gratia's existing nine-colour Okabe-Ito palette and hue
fallback.

## Usage

``` r
gratia_palette(name = "default", reverse = FALSE)
```

## Arguments

- name:

  Palette name: `"default"`, `"tol_bright"`, `"tol_vibrant"`,
  `"tol_muted"`, `"tol_dark"`, `"tol_banded_12"`, `"tol_banded_16"`, or
  `"tol_banded_20"`.

- reverse:

  Logical; reverse the palette order?

## Value

A function taking the number of colours and returning a character vector
of hexadecimal colours.

## Details

Tol's bright and vibrant palettes contain seven colours, muted contains
nine, and dark contains eleven. The dark palette favours visibility
against white over separation between colours. Banded palettes provide
12, 16 or 20 colours; neighbouring shades become less distinct as the
palette grows. These are optional choices and do not change gratia's
defaults.

Finite palettes use the published order, returning the first requested
colours. They error if their capacity is exceeded; they never recycle or
interpolate colours. The default palette uses hue colours above nine
levels. Set both colour and fill palettes to keep grouped curves and
ribbons paired.

Definitions follow Paul Tol's colour schemes as published in August
2026. The banded schemes are derived from his sequential schemes for use
when larger sets of colours are required. See
<https://sronpersonalpages.nl/~pault/> for the definitions and examples.

## Examples

``` r
pal <- gratia_palette("tol_muted")
pal(5)
#> [1] "#CC6677" "#332288" "#DDCC77" "#117733" "#88CCEE"
ggplot2::theme(
  palette.colour.discrete = pal,
  palette.fill.discrete = pal
)
#> <theme> List of 2
#>  $ palette.colour.discrete:function (n)  
#>  $ palette.fill.discrete  :function (n)  
#>  @ complete: logi FALSE
#>  @ validate: logi TRUE
```
