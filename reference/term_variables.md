# Names of variables involved in a specified model term

Given the name (a term label) of a term in a model, returns the names of
the variables involved in the term.

## Usage

``` r
term_variables(object, term, ...)

# S3 method for class 'terms'
term_variables(object, term, ...)

# S3 method for class 'gam'
term_variables(object, term, ...)

# S3 method for class 'bam'
term_variables(object, term, ...)
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

A character vector of variable names.
