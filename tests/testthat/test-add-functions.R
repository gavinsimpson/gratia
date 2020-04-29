## Test basis() and related functions

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("test add_fitted")

set.seed(42)
data <- gamSim(eg = 1, verbose = FALSE)
## take only some columns
data <- data[, c("y","x0","x1","x2","x3")]
## fit the model
m <-  gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = data, method = 'REML')

test_that("add_fitted works for a GAM", {
    expect_silent(df <- add_fitted(data, m))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".value"))
})

test_that("add_fitted works for a GAM with type = 'terms'", {
    expect_silent(df <- add_fitted(data, m, type = 'terms'))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".constant",
                       ".s(x0)", ".s(x1)", ".s(x2)", ".s(x3)"))
})

test_that("prefix works for a GAM with type = 'terms'", {
    expect_silent(df <- add_fitted(data, m, type = 'terms', prefix = ".."))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", "..constant",
                       "..s(x0)", "..s(x1)", "..s(x2)", "..s(x3)"))
})

test_that("add_residuals works for a GAM", {
    expect_silent(df <- add_residuals(data, m, type = "pearson"))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".residual"))
})

## test what happens with na.action and NAs in input
miss <- sample(nrow(data), 10)
data[["x0"]][miss] <- NA
m_na <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = data, method = 'REML')
m_na_excl <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = data, method = 'REML',
                 na.action = na.exclude)

test_that("add_residuals works for a GAM with NA in data", {
    expect_error(add_residuals(data, m_na, value = "..resid.."),
                 "Length of model residuals does not equal number of rows in 'data'",
                 fixed = TRUE)
    expect_silent(df <- add_residuals(data, m_na_excl))
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("y", "x0", "x1", "x2", "x3", ".residual"))
})
