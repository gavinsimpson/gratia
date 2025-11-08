# Transform estimated values and confidence intervals by applying a function

Transform estimated values and confidence intervals by applying a
function

## Usage

``` r
transform_fun(object, fun = NULL, ...)

# S3 method for class 'smooth_estimates'
transform_fun(object, fun = NULL, constant = NULL, ...)

# S3 method for class 'smooth_samples'
transform_fun(object, fun = NULL, constant = NULL, ...)

# S3 method for class 'mgcv_smooth'
transform_fun(object, fun = NULL, constant = NULL, ...)

# S3 method for class 'evaluated_parametric_term'
transform_fun(object, fun = NULL, constant = NULL, ...)

# S3 method for class 'parametric_effects'
transform_fun(object, fun = NULL, constant = NULL, ...)

# S3 method for class 'tbl_df'
transform_fun(object, fun = NULL, column = NULL, constant = NULL, ...)
```

## Arguments

- object:

  an object to apply the transform function to.

- fun:

  the function to apply.

- ...:

  additional arguments passed to methods.

- constant:

  numeric; a constant to apply before transformation.

- column:

  character; for the `"tbl_df"` method, which column to transform.

## Value

Returns `object` but with the estimate and upper and lower values of the
confidence interval transformed via the function.

## Author

Gavin L. Simpson
