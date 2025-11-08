# Extract an mgcv smooth given its position in the model object

Extract an mgcv smooth given its position in the model object

## Usage

``` r
get_smooths_by_id(object, id)

# S3 method for class 'gam'
get_smooths_by_id(object, id)

# S3 method for class 'scam'
get_smooths_by_id(object, id)

# S3 method for class 'gamm'
get_smooths_by_id(object, id)

# S3 method for class 'gamm4'
get_smooths_by_id(object, id)

# S3 method for class 'list'
get_smooths_by_id(object, id)
```

## Arguments

- object:

  a fitted GAM model object.

- id:

  numeric; the position of the smooth in the model object.
