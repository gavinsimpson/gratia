# Add a constant to estimated values

Add a constant to estimated values

## Usage

``` r
add_constant(object, constant = NULL, ...)

# S3 method for class 'smooth_estimates'
add_constant(object, constant = NULL, ...)

# S3 method for class 'smooth_samples'
add_constant(object, constant = NULL, ...)

# S3 method for class 'mgcv_smooth'
add_constant(object, constant = NULL, ...)

# S3 method for class 'parametric_effects'
add_constant(object, constant = NULL, ...)

# S3 method for class 'tbl_df'
add_constant(object, constant = NULL, column = NULL, ...)

# S3 method for class 'evaluated_parametric_term'
add_constant(object, constant = NULL, ...)
```

## Arguments

- object:

  a object to add a constant to.

- constant:

  the constant to add.

- ...:

  additional arguments passed to methods.

- column:

  character; for the `"tbl_df"` method, which column to add the constant
  too.

## Value

Returns `object` but with the estimate shifted by the addition of the
supplied constant.

## Author

Gavin L. Simpson
