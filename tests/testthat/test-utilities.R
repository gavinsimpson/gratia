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

data(columb)       ## data frame
data(columb.polys) ## district shapes list
xt <- list(polys = columb.polys) ## neighbourhood structure info for MRF
## First a full rank MRF...
mrf_mod <- gam(crime ~ s(district, bs="mrf", xt=xt), data = columb,
               method = "REML")

test_that("is_mrf_smooth returns true for an MRF smooth", {
    expect_true(is_mrf_smooth(get_smooth(mrf_mod, "s(district)")))
})

test_that("is_mrf_smooth returns false for an none MRF smooth", {
    expect_false(is_mrf_smooth(get_smooth(m1, "s(x0)")))
})

test_that("is_mgcv_smooth returns false for objects that aren't smooths", {
    expect_false(is_mgcv_smooth(1:10))
})

test_that("check_is_mgcv_smooth throws error for objects that aren't smooths", {
    expect_error(check_is_mgcv_smooth(1:10),
                 "Object passed to 'smooth' is not a 'mgcv.smooth'.",
                 fixed = TRUE)
})

test_that("is.gam returns TRUE for a GAM", {
    expect_true(is.gam(mrf_mod))
    expect_true(is.gam(m1))
})

test_that("is.gam returns FALSE for a none GAM", {
    expect_false(is.gam(1:10))
    expect_false(is.gam(data.frame(x = 1:10)))
    expect_false(is.gam(m2))
})
