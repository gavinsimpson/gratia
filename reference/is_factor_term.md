# Is a model term a factor (categorical)?

Given the name (a term label) of a term in a model, identify if the term
is a factor term or numeric. This is useful when considering
interactions, where terms like `fac1:fac2` or `num1:fac1` may be
requested by the user. Only for terms of the type `fac1:fac2` will this
function return `TRUE`.

## Usage

``` r
is_factor_term(object, term, ...)

# S3 method for class 'terms'
is_factor_term(object, term, ...)

# S3 method for class 'gam'
is_factor_term(object, term, ...)

# S3 method for class 'bam'
is_factor_term(object, term, ...)

# S3 method for class 'gamm'
is_factor_term(object, term, ...)

# S3 method for class 'list'
is_factor_term(object, term, ...)
```

## Arguments

- object:

  an R object on which method dispatch is performed

- term:

  character; the name of a model term, in the sense of
  `attr(terms(object), "term.labels")`. Currently not checked to see if
  the term exists in the model.

- ...:

  arguments passed to other methods.

## Value

A logical: `TRUE` if and only if all variables involved in the term are
factors, otherwise `FALSE`.
