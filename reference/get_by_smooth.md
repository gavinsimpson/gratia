# Extract an factor-by smooth by name

Extract an factor-by smooth by name

## Usage

``` r
get_by_smooth(object, term, level)
```

## Arguments

- object:

  a fitted GAM model object.

- term:

  character; the name of a smooth term to extract.

- level:

  character; which level of the factor to extract the smooth for.

## Value

A single smooth object, or a list of smooths if several match the named
term.
