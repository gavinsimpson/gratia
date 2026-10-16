# The Number of linear predictors in model

**\[experimental\]** Extracts the number of linear predictors from the
fitted model.

## Usage

``` r
# S3 method for class 'gamm4'
n_eta(model, ...)

# S3 method for class 'scam'
n_eta(model, ...)

n_eta(model, ...)

# S3 method for class 'gam'
n_eta(model, ...)
```

## Arguments

- model:

  a fitted model. Currently, only models inheriting from class `"gam"`
  are supported.

- ...:

  arguments passed to methods.

## Value

An integer vector of length 1 containing the number of linear predictors
in the model.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
