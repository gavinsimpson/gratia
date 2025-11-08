# Reorder tensor product terms for nicer plotting

If a tensor product smooth of 3 or more terms contains a 2d marginal
smooth, we will get nicer output from
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
and hence a nicer plot from the
[`draw.smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/draw.smooth_estimates.md)
method if we reorder the terms of the smooth such that we vary the terms
in the 2d marginal first, and any other terms vary more slowly when we
generate data to evaluate the smooth at. This results in automatically
generated data that focuses on the (or the first if more than one) 2d
marginal smooth, with the end result that
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
shows how that 2d smooth changes with the other terms involved in the
smooth.

## Usage

``` r
reorder_tensor_smooth_terms(smooth)
```

## Arguments

- smooth:

  an mgcv smooth object
