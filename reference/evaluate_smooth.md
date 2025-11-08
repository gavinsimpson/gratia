# Evaluate a smooth

**\[deprecated\]** Evaluate a smooth at a grid of evenly spaced value
over the range of the covariate associated with the smooth.
Alternatively, a set of points at which the smooth should be evaluated
can be supplied.

## Usage

``` r
evaluate_smooth(object, ...)
```

## Arguments

- object:

  an object of class `"gam"` or `"gamm"`.

- ...:

  arguments passed to other methods.

## Value

A data frame, which is of class `"evaluated_1d_smooth"` or
`evaluated_2d_smooth`, which inherit from classes `"evaluated_smooth"`
and `"data.frame"`.

## Details

**\[deprecated\]** `evaluate_smooth()` is deprecated in favour of
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md),
which provides a cleaner way to evaluate a smooth over a range of
covariate values.
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
can handle a much wider range of models than `evaluate_smooth()` is
capable of and
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md)
is much easier to extend to handle new smooth types.

Most code that uses `evaluate_smooth()` should work simply by changing
the function call to
[`smooth_estimates()`](https://gavinsimpson.github.io/gratia/reference/smooth_estimates.md).
However, there are some differences:

- the `newdata` argument becomes `data`
