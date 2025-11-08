# Create a sequence of evenly-spaced values

For a continuous vector `x`, `evenly` and `seq_min_max()` create a
sequence of `n` evenly-spaced values over the range `lower` – `upper`.
By default, `lower` is defined as `min(x)` and `upper` as `max(x)`,
excluding `NA`s. For a factor `x`, the function returns `levels(x)`.

## Usage

``` r
evenly(x, n = 100, by = NULL, lower = NULL, upper = NULL)

seq_min_max(x, n, by = NULL, lower = NULL, upper = NULL)
```

## Arguments

- x:

  numeric; vector over which evenly-spaced values are returned

- n:

  numeric; the number of evenly-spaced values to return. A default of
  `100` is used for convenience as that what is typically used when
  evaluating a smooth.

- by:

  numeric; the increment of the sequence. If specified, argument `n` is
  ignored and the sequence returned will be from `min(x)` to `max(x)` in
  increments of `by`.

- lower:

  numeric; the lower bound of the interval.

- upper:

  numeric; the upper bound of the interval.

## Value

A numeric vector of length `n`.

## See also

See [`base::seq()`](https://rdrr.io/r/base/seq.html) for details of the
behaviour of `evenly()` when using `by`.

## Examples

``` r
x <- rnorm(10)
n <- 10L

# 10 values evenly over the range of `x`
evenly(x, n = n)
#>  [1] -0.83562861 -0.56552757 -0.29542652 -0.02532547  0.24477557  0.51487662
#>  [7]  0.78497766  1.05507871  1.32517976  1.59528080

# evenly spaced values, incrementing by 0.2
evenly(x, by = 0.2)
#>  [1] -0.83562861 -0.63562861 -0.43562861 -0.23562861 -0.03562861  0.16437139
#>  [7]  0.36437139  0.56437139  0.76437139  0.96437139  1.16437139  1.36437139
#> [13]  1.56437139

# evenly spaced values, incrementing by 0.2, starting at -2
evenly(x, by = 0.2, lower = -2)
#>  [1] -2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2  0.0  0.2  0.4  0.6  0.8
#> [16]  1.0  1.2  1.4
```
