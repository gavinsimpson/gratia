# Find the names of model terms

Returns the names of any terms in a model, without needing to call
[`summary()`](https://rdrr.io/r/base/summary.html). The list of model
terms is especially useful when predicting from a
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) model using the
`exclude` or `terms` argument of
[`mgcv::predict.gam()`](https://rdrr.io/pkg/mgcv/man/predict.gam.html)
or
[`mgcv::predict.bam()`](https://rdrr.io/pkg/mgcv/man/predict.bam.html).

## Usage

``` r
model_terms(object, ...)

# S3 method for class 'gam'
model_terms(object, ...)

# S3 method for class 'gamm'
model_terms(object, ...)

# S3 method for class 'gamm4'
model_terms(object, ...)

# S3 method for class 'lm'
model_terms(object, ...)
```

## Arguments

- object:

  a fitted model.

- ...:

  arguments to be passed to other methods; not currently used.

## Value

A character vector of model terms.

## Details

From the point of view of *gratia*, models contain two types of term:

1.  parametric terms, and

2.  smooth terms.

If we consider the formula `y ~ fac + s(x2, by = fac) + s(x0)`, for a
factor `fac` with three levels, there are seven terms in the model:

1.  the model constant term, with name `"(Intercept)"`,

2.  the parametric factor term, with names

    - `fac2`,

    - `fac3`,

3.  the univariate smooth of `x0`, named `"s(x0)"`, and

4.  the three factor-by smooths with names

    - `"s(x2):fac1"`,

    - `"s(x2):fac2"`, and

    - `"s(x2):fac3"`.\`

`model_terms()` will return a vector of those names.

## Examples

``` r
load_mgcv()

m <- gam(y ~ fac + s(x2, by = fac) + s(x0),
  data = su_eg4, method = "REML")
#> Error in eval(mf, parent.frame()): object 'su_eg4' not found

# return the names of terms in this model
model_terms(m)
#> Error: object 'm' not found
```
