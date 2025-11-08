# Create a sequence of evenly-spaced values adjusted to accommodate a small adjustment

Creates a sequence of `n` evenly-spaced values over the range `min(x)` –
`max(x)`, where the minimum and maximum are adjusted such that they are
always contained within the range of `x` when `x` may be shifted
forwards or backwards by an amount related to `eps`. This is
particularly useful in computing derivatives via finite differences
where without this adjustment we may be predicting for values outside
the range of the data and hence the constraints of the penalty.

## Usage

``` r
seq_min_max_eps(x, n, order, type = c("forward", "backward", "central"), eps)
```

## Arguments

- x:

  numeric; vector over which evenly-spaced values are returned

- n:

  numeric; the number of evenly-spaced values to return

- order:

  integer; the order of derivative. Either `1` or `2` for first or
  second order derivatives

- type:

  character; the type of finite difference used. One of `"forward"`,
  `"backward"`, or `"central"`

- eps:

  numeric; the finite difference

## Value

A numeric vector of length `n`.
