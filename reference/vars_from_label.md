# Returns names of variables from a smooth label

Returns names of variables from a smooth label

## Usage

``` r
vars_from_label(label)
```

## Arguments

- label:

  character; a length 1 character vector containing the label of a
  smooth.

## Examples

``` r
vars_from_label("s(x1)")
#> [1] "x1"
vars_from_label("t2(x1,x2,x3)")
#> [1] "x1" "x2" "x3"
```
