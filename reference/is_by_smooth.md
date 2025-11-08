# Tests for by variable smooths

Functions to check if a smooth is a by-variable one and to test of the
type of by-variable smooth is a factor-smooth or a continuous-smooth
interaction.

## Usage

``` r
is_by_smooth(smooth)

is_factor_by_smooth(smooth)

is_continuous_by_smooth(smooth)

by_variable(smooth)

by_level(smooth)
```

## Arguments

- smooth:

  an object of class `"mgcv.smooth"`

## Value

A logical vector.

## Author

Gavin L. Simpson
