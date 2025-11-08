# Set rows of data to `NA` if the lie too far from a reference set of values

Set rows of data to `NA` if the lie too far from a reference set of
values

## Usage

``` r
too_far_to_na(smooth, input, reference, cols, dist = NULL)
```

## Arguments

- smooth:

  an mgcv smooth object

- input:

  data frame containing the input observations and the columns to be set
  to `NA`

- reference:

  data frame containing the reference values

- cols:

  character vector of columns whose elements will be set to `NA` if the
  data lies too far from the reference set

- dist:

  numeric, the distance from the reference set beyond which elements of
  `input` will be set to `NA`
