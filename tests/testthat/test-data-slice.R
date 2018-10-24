## Test data_slice() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("data-slice-methods")

set.seed(1)
dat <- gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("data_slice works for a GAM", {
    expect_silent(ds <- data_slice(m1, var1 = "x1", var2 = "x2"))
    expect_s3_class(ds, "tbl_df")
    expect_named(ds, c("x1", "x2", "x0", "x3"))
    expect_error(data_slice(m1, var1 = "x1", var2 = "foo"),
                 "Variable <foo> not found in data.", fixed = TRUE)
})

test_that("process_slice_data works when passed a 1-row data frame, tibble, or list", {
    expect_silent( result1 <- process_slice_data(dat[1, ]) )
    expect_silent( result2 <- process_slice_data(tibble::as_tibble(dat[1, ])))
    expect_silent( result3 <- process_slice_data(as.list(dat[1, ])))
    expect_equal(NROW(result1), 1L)
    expect_equal(NROW(result2), 1L)
    expect_equal(NROW(result3), 1L)
    expect_equal(result1, result2)
    expect_equal(result1, result3)
    expect_equal(result2, result3)
})

test_that("process_slice_data fails when passed a data frame with > 1 rows", {
    expect_error(process_slice_data(dat),
                 "'data' should have 1 row only. Supplied <200>",
                 fixed = TRUE)
})

test_that("process_slice_data fails when passed a matrix", {
    expect_error(process_slice_data(as.matrix(dat)),
                 "'data' should be a tibble, data frame, or list. Supplied <matrix>",
                 fixed = TRUE)
})

set.seed(42)
dat <- gamSim(4, n = 400, verbose = FALSE)
mf <- gam(y ~ fac + s(x2, by = fac) + s(x0), data = dat)

test_that("data_slice works for a GAM with factor by", {
    expect_silent(ds <- data_slice(mf, var1 = "x2", var2 = "fac"))
    expect_s3_class(ds, "tbl_df")
    expect_named(ds, c("x2", "fac", "x0"))
})

test_that("default data_slice method fails gracefully", {
    expect_error(data_slice(dat),
                 "Don't know how to create a data slice from <data.frame>",
                 fixed = TRUE)
})

test_that("value_closest_to_median fails for character vectors", {
    expect_error(value_closest_to_median(LETTERS),
                 "'x' must be a factor or numeric vector. Supplied <character>",
                 fixed = TRUE)
})

test_that("value_closest_to_median fails for logical vectors", {
    expect_error(value_closest_to_median(sample(c(TRUE, FALSE), 50, replace = TRUE)),
                 "'x' must be a factor or numeric vector. Supplied <logical>",
                 fixed = TRUE)
})

test_that("value_closest_to_median works with a factor", {
    expect_silent( result <- gratia:::value_closest_to_median(dat[["fac"]]) )
    expect_identical(factor(3, levels = c(1,2,3)), result)
})
