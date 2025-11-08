# Gu and Wahba test functions

Gu and Wahba test functions

## Usage

``` r
gw_f0(x, ...)

gw_f1(x, ...)

gw_f2(x, ...)

gw_f3(x, ...)
```

## Arguments

- x:

  numeric; vector of points to evaluate the function at, on interval
  (0,1)

- ...:

  arguments passed to other methods, ignored.

## Examples

``` r
x <- seq(0, 1, length = 6)
gw_f0(x)
#> [1] 0.000e+00 1.176e+00 1.902e+00 1.902e+00 1.176e+00 2.449e-16
gw_f1(x)
#> [1] 1.000 1.492 2.226 3.320 4.953 7.389
gw_f2(x)
#> [1] 0.000 8.591 4.261 3.199 1.100 0.000
gw_f3(x) # should be constant 0
#> [1] 0 0 0 0 0 0
```
