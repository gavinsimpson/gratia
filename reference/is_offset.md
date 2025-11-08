# Is a model term an offset?

Given a character vector of model terms, checks to see which, if any, is
the model offset.

## Usage

``` r
is_offset(terms)
```

## Arguments

- terms:

  character vector of model terms.

## Value

A logical vector of the same length as `terms`.

## Author

Gavin L. Simpson

## Examples

``` r
load_mgcv()
df <- data_sim("eg1", n = 400, dist = "normal")
m <- gam(y ~ s(x0) + s(x1) + offset(x0), data = df, method = "REML")
nm <- names(model.frame(m))
nm
#> [1] "y"          "offset(x0)" "x0"         "x1"        
is_offset(nm)
#> [1] FALSE  TRUE FALSE FALSE
```
