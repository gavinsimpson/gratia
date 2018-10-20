## Test confint() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("derivatives methods")

set.seed(42)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("derivatives fails for an unknown object", {
    df <- data.frame(a = 1:10, b = 1:10)
    expect_error(derivatives(df),
                 "Don't know how to calculate derivatives for <data.frame>",
                 fixed = TRUE)
})

test_that("derivatives() fails with inappropriate args", {
    expect_error(derivatives(mod, type = "foo"),
                 paste("'arg' should be one of",
                       paste(dQuote(c("forward","backward","central")),
                             collapse = ", ")),
                 fixed = TRUE)

    expect_error(derivatives(mod, order = 3),
                 "Only 1st or 2nd derivatives are supported: `order %in% c(1,2)`",
                 fixed = TRUE)
})

test_that("derivatives() returns derivatives for all smooths in a GAM", {
    expect_silent(df <- derivatives(mod))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x1"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x1", type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x1", type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x1", type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))
})

test_that("derivatives() throws errors if order == 2L", {
    expect_error( derivatives(mod, order = 2),
                 "2nd derivatives not yet implemented.", fixed = TRUE)

    expect_error( derivatives(mod, order = 2, type = "forward"),
                 "2nd derivatives not yet implemented.", fixed = TRUE)

    expect_error( derivatives(mod, order = 2, type = "backward"),
                 "2nd derivatives not yet implemented.", fixed = TRUE)

    expect_error( derivatives(mod, order = 2, type = "central"),
                 "2nd derivatives not yet implemented.", fixed = TRUE)
})

mod <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("derivatives() returns derivatives for all smooths in a GAMM", {
    expect_silent(df <- derivatives(mod))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))
})

## confint methods for by variables
set.seed(2)
dat <- gamSim(4, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ fac + s(x2, by = fac), data = dat, method = "REML")

test_that("derivatives() fails with inappropriate args", {
    expect_error(derivatives(mod, type = "foo"),
                 paste("'arg' should be one of",
                       paste(dQuote(c("forward","backward","central")),
                             collapse = ", ")),
                 fixed = TRUE)

    expect_error(derivatives(mod, order = 3),
                 "Only 1st or 2nd derivatives are supported: `order %in% c(1,2)`",
                 fixed = TRUE)
})

test_that("derivatives() returns derivatives for all smooths in a factor by GAM", {
    expect_silent(df <- derivatives(mod))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x2"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x2", type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x2", type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))

    expect_silent(df <- derivatives(mod, "x2", type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se"))
})

test_that("derivatives() throws errors if order == 2L in a factor by GAM", {
    expect_error( derivatives(mod, order = 2),
                 "2nd derivatives not yet implemented.", fixed = TRUE)

    expect_error( derivatives(mod, order = 2, type = "forward"),
                 "2nd derivatives not yet implemented.", fixed = TRUE)

    expect_error( derivatives(mod, order = 2, type = "backward"),
                 "2nd derivatives not yet implemented.", fixed = TRUE)

    expect_error( derivatives(mod, order = 2, type = "central"),
                 "2nd derivatives not yet implemented.", fixed = TRUE)
})
