# Names of variables involved in a specified model term

Given the name (a term label) of a term in a model, returns the names of
the variables involved in the term.

## Usage

``` r
# S3 method for class 'gamm4'
term_variables(object, ...)

# S3 method for class 'scam'
term_variables(object, term, ...)

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

- ...:

  arguments passed to other methods.

- term:

  character; the name of a model term, in the sense of
  `attr(terms(object), "term.labels")`. Currently not checked to see if
  the term exists in the model.

## Value

A character vector of variable names.

## See also

[gamm4_support](https://gavinsimpson.github.io/gratia/reference/gamm4_support.md)
