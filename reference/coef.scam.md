# Extract coefficients from a fitted `scam` model.

Extract coefficients from a fitted `scam` model.

## Usage

``` r
# S3 method for class 'scam'
coef(object, parametrized = TRUE, ...)
```

## Arguments

- object:

  a model object fitted by `scam()`

- parametrized:

  logical; extract parametrized coefficients, which respect the linear
  inequality constraints of the model.

- ...:

  other arguments.
