# Test evaluate_parametric_terms

## load packages
library("testthat")
library("gratia")
library("mgcv")

data(gss_vocab, package = "gratia")

m <- gam(vocab ~ nativeBorn * ageGroup, data = gss_vocab, method = 'ML')

test_that("evaluate_parametric_terms() works with factor terms", {
    ## evaluate parametric terms directly
    term <- "nativeBorn"
    expect_silent(para <- evaluate_parametric_term(m, term = term))
    expect_s3_class(para, "evaluated_parametric_term")
    expect_s3_class(para, "tbl_df")
    expect_s3_class(para, "tbl")
    expect_s3_class(para, "data.frame")
    expect_named(para,
                 c("term", "type", "value", "partial", "se"))

    expect_error(evaluate_parametric_term(m, term = "foo"),
                 "Term is not in the parametric part of model: <foo>",
                 fixed = TRUE)

    expect_warning(evaluate_parametric_term(m, term = c('nativeBorn', 'ageGroup')),
                   "More than one `term` requested; using the first <nativeBorn>",
                   fixed = TRUE)
})


set.seed(0)
## fake some data...
f1 <- function(x) {exp(2 * x)}
f2 <- function(x) {
    0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
}
f3 <- function(x) {x*0}

n <- 200
sig2 <- 4
x0 <- rep(1:4,50)
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
e <- rnorm(n, 0, sqrt(sig2))
y <- 2*x0 + f1(x1) + f2(x2) + f3(x3) + e
df <- data.frame(x0 = x0, x1 = x1, x2 = x2, x3 = x3, y = y,
                 fx0 = factor(x0))

## fit
mod <- gam(y ~ x0 + s(x1) + s(x2) + s(x3), data = df)

test_that("evaluate_parametric_terms() works with parametric terms", {
    ## evaluate parametric terms directly
    expect_silent(para <- evaluate_parametric_term(mod, term = "x0"))
    expect_s3_class(para, "evaluated_parametric_term")
    expect_s3_class(para, "tbl_df")
    expect_s3_class(para, "tbl")
    expect_s3_class(para, "data.frame")
    expect_named(para, c("term","type","value","partial","se"))

    expect_error(evaluate_parametric_term(mod, term = "foo"),
                 "Term is not in the parametric part of model: <foo>",
                 fixed = TRUE)

    expect_warning(evaluate_parametric_term(mod, term = c('x0', 'x1')),
                   "More than one `term` requested; using the first <x0>",
                   fixed = TRUE)
})
