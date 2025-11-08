# Select smooths based on user's choices

Given a vector indexing the smooths of a GAM, returns a logical vector
selecting the requested smooths.

## Usage

``` r
check_user_select_smooths(
  smooths,
  select = NULL,
  partial_match = FALSE,
  model_name = NULL
)
```

## Arguments

- smooths:

  character; a vector of smooth labels.

- select:

  numeric, logical, or character vector of selected smooths.

- partial_match:

  logical; in the case of character `select`, should `select` match
  partially against `smooths`? If `partial_match = TRUE`, `select` must
  only be a single string, a character vector of length 1.

- model_name:

  character; a model name that will be used in error messages.

## Value

A logical vector the same length as `length(smooths)` indicating which
smooths have been selected.

## Author

Gavin L. Simpson
