# Extract an mgcv smooth by name

Extract an mgcv smooth by name

## Usage

``` r
get_smooth(object, term)
```

## Arguments

- object:

  a fitted GAM model object.

- term:

  character; the name of a smooth term to extract

## Value

A single smooth object, or a list of smooths if several match the named
term.
