# Repeat the first level of a factor n times

Function to repeat the first level of a factor n times and return this
vector as a factor with the original levels intact

## Usage

``` r
rep_first_factor_value(f, n)
```

## Arguments

- f:

  a factor

- n:

  numeric; the number of times to repeat the first level of `f`

## Value

A factor of length `n` with the levels of `f`, but whose elements are
all the first level of `f`.
