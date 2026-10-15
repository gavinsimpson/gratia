# Extract the covariance matrix of the model parameters or a subset of these for a particular model term

Extract the covariance matrix of the model parameters or a subset of
these for a particular model term

## Usage

``` r
get_vcov(
  object,
  unconditional = FALSE,
  frequentist = FALSE,
  term = NULL,
  by_level = NULL
)

# S3 method for class 'scam'
get_vcov(
  object,
  unconditional = FALSE,
  frequentist = FALSE,
  term = NULL,
  by_level = NULL
)
```

## Arguments

- object:

  an R object from which a covariance matrix shall be extracted.

- unconditional:

  logical; if `TRUE` (and only if `frequentist == FALSE`) then the
  bayesian smoothing parameter uncertainty-corrected covariance matrix
  is returned, if available. Whether it is available depends on which
  smoothness selection method was used to fit the model.

- frequentist:

  logical; if `FALSE`, the default, the bayesian covariance matrix is
  returned, otherwise the frequentist covariance matrix.

- term:

  character, length 1; return the subset of the covariance matrix for a
  particular **smooth** term. Only a single term can be selected.

- by_level:

  character; currently unsued.

## Value

a matrix containing the selected covariance matrix.

## Details

For [`mgcv::scasm()`](https://rdrr.io/pkg/mgcv/man/scasm.html) fits with
linear inequality constraints, the Gaussian covariance approximation
does not enforce the fitted shapes. Pointwise intervals calculated from
this matrix can extend outside those constraints. See
[`post_draws()`](https://gavinsimpson.github.io/gratia/reference/post_draws.md)
for sampling and native-bootstrap details.

## Author

Gavin L. Simpson
