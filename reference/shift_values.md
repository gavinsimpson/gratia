# Shift numeric values in a data frame by an amount `eps`

Shift numeric values in a data frame by an amount `eps`

## Usage

``` r
shift_values(df, h, i, FUN = `+`, focal = NULL)
```

## Arguments

- df:

  a data frame or tibble.

- h:

  numeric; the amount to shift values in `df` by.

- i:

  logical; a vector indexing columns of `df` that should not be included
  in the shift.

- FUN:

  function; a function to apply the shift. Typically `+` or `-`.

- focal:

  character; the focal variable when computing partial derivatives. This
  allows shifting only the focal variable by `eps`.
