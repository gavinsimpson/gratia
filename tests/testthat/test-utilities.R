## Test Utilties

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("Testing Utility Functions")

set.seed(1)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
m2 <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("smooth_terms() methods work", {
    st <- smooth_terms(m1)
    expect_is(st, "list")
    expect_length(st, 4L)
    expect_identical(st, as.list(paste0("x", 0:3)))

    st <- smooth_terms(m2)
    expect_is(st, "list")
    expect_length(st, 4L)
    expect_identical(st, as.list(paste0("x", 0:3)))

    st <- smooth_terms(m1[["smooth"]][[1]])
    expect_is(st, "character")
    expect_length(st, 1L)
    expect_identical(st, "x0")
})

test_that("smooth_dim() methods work", {
    d <- smooth_dim(m1)
    expect_is(d, "integer")
    expect_length(d, 4L)
    expect_identical(d, rep(1L, 4L))

    d <- smooth_dim(m2)
    expect_is(d, "integer")
    expect_length(d, 4L)
    expect_identical(d, rep(1L, 4L))

    d <- smooth_dim(m1[["smooth"]][[1]])
    expect_is(d, "integer")
    expect_length(d, 1L)
    expect_identical(d, rep(1L, 1L))
})

test_that("select_terms() works", {
    st <- select_terms(m1)
    expect_is(st, "character")
    expect_length(st, 4L)
    expect_identical(st, paste0("x", 0:3))

    st <- select_terms(m1, "x1")
    expect_is(st, "character")
    expect_length(st, 1L)
    expect_identical(st, "x1")

    st <- select_terms(m1, c("x1", "x2"))
    expect_is(st, "character")
    expect_length(st, 2L)
    expect_identical(st, c("x1", "x2"))

    expect_message(select_terms(m1, "x4"), "x4 not found in `object`")
    expect_message(select_terms(m1, c("x1", "x4")), "x4 not found in `object`")

})

test_that("select_smooth() works", {
    expect_error(select_smooth(m1), "'smooth' must be supplied")
    expect_message(select_smooth(m1, smooth = c("s(x1)", "s(x2)")),
                   "Multiple smooths supplied. Using only first")

    sm <- select_smooth(m1, smooth = "s(x1)")
    expect_identical(sm, "s(x1)")

})
