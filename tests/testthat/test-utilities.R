## Test Utilties

## load packages
library("testthat")
library("gratia")
library("mgcv")

context("Testing Utility Functions")

set.seed(1)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m_gam <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
m_gamm <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
m_bam <- bam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
m_gamgcv <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

test_that("smooth_terms() methods work", {
    st <- smooth_terms(m_gam)
    expect_is(st, "list")
    expect_length(st, 4L)
    expect_identical(st, as.list(paste0("x", 0:3)))

    st <- smooth_terms(m_gamm)
    expect_is(st, "list")
    expect_length(st, 4L)
    expect_identical(st, as.list(paste0("x", 0:3)))

    st <- smooth_terms(m_gam[["smooth"]][[1]])
    expect_is(st, "character")
    expect_length(st, 1L)
    expect_identical(st, "x0")
})

test_that("smooth_dim() methods work", {
    d <- smooth_dim(m_gam)
    expect_is(d, "integer")
    expect_length(d, 4L)
    expect_identical(d, rep(1L, 4L))

    d <- smooth_dim(m_gamm)
    expect_is(d, "integer")
    expect_length(d, 4L)
    expect_identical(d, rep(1L, 4L))

    d <- smooth_dim(m_gam[["smooth"]][[1]])
    expect_is(d, "integer")
    expect_length(d, 1L)
    expect_identical(d, rep(1L, 1L))
})

test_that("select_terms() works", {
    st <- select_terms(m_gam)
    expect_is(st, "character")
    expect_length(st, 4L)
    expect_identical(st, paste0("x", 0:3))

    st <- select_terms(m_gam, "x1")
    expect_is(st, "character")
    expect_length(st, 1L)
    expect_identical(st, "x1")

    st <- select_terms(m_gam, c("x1", "x2"))
    expect_is(st, "character")
    expect_length(st, 2L)
    expect_identical(st, c("x1", "x2"))

    expect_message(select_terms(m_gam, "x4"), "x4 not found in `object`")
    expect_message(select_terms(m_gam, c("x1", "x4")), "x4 not found in `object`")

})

test_that("select_smooth() works", {
    expect_error(select_smooth(m_gam), "'smooth' must be supplied")
    expect_message(select_smooth(m_gam, smooth = c("s(x1)", "s(x2)")),
                   "Multiple smooths supplied. Using only first")

    sm <- select_smooth(m_gam, smooth = "s(x1)")
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
    expect_false(is_mrf_smooth(get_smooth(m_gam, "s(x0)")))
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
    expect_true(is.gam(m_gam))
})

test_that("is.gam returns FALSE for a none GAM", {
    expect_false(is.gam(1:10))
    expect_false(is.gam(data.frame(x = 1:10)))
    expect_false(is.gam(m_gamm))
})

test_that("is.gamm returns TRUE for a GAMM", {
    expect_true(is.gamm(m_gamm))
})

test_that("is.gam returns FALSE for a none GAMM", {
    expect_false(is.gamm(1:10))
    expect_false(is.gamm(data.frame(x = 1:10)))
    expect_false(is.gamm(m_gam))
    expect_false(is.gamm(mrf_mod))
})

test_that("get_vcov with frequentist TRUE works", {
    V <- get_vcov(m_gam, frequentist = TRUE)
    expect_is(V, "matrix")
    expect_equal(V, m_gam[["Ve"]])
})

test_that("get_vcov with unconditional = TRUE throws warning if not available", {
    expect_warning(V <- get_vcov(m_gamgcv, unconditional = TRUE),
                   "Covariance corrected for smoothness uncertainty not available.")
    expect_is(V, "matrix")
    expect_equal(V, m_gamgcv[["Vp"]])
})

test_that("get_vcov with unconditional = TRUE returns Vp", {
    V <- get_vcov(m_gam, unconditional = TRUE)
    expect_is(V, "matrix")
    expect_equal(V, m_gam[["Vc"]])
})

test_that("get_vcov with term specified works", {
    V <- get_vcov(m_gam, term = "s(x1)")
    expect_is(V, "matrix")
    smooth <- m_gam[["smooth"]][[2L]]
    ind <- smooth$first.para:smooth$last.para
    expect_equal(V, m_gam[["Vp"]][ind, ind, drop = FALSE])

    V <- get_vcov(m_gam, frequentist = TRUE, term = "s(x1)")
    expect_equal(V, m_gam[["Ve"]][ind, ind, drop = FALSE])

    V <- get_vcov(m_gam, unconditional = TRUE, term = "s(x1)")
    expect_equal(V, m_gam[["Vc"]][ind, ind, drop = FALSE])

    expect_message(get_vcov(m_gam, term = c("s(x1)", "s(x2)")),
                   "Supplied more than 1 'term'; using only the first")
})

test_that("get_smooth works for a GAM", {
    sm <- get_smooth(m_gam, "s(x1)")
    expect_is(sm, "mgcv.smooth")
    expect_true(is_mgcv_smooth(sm))
})

test_that("get_smooth works for a GAMM", {
    sm <- get_smooth(m_gamm, "s(x1)")
    expect_is(sm, "mgcv.smooth")
    expect_true(is_mgcv_smooth(sm))
})

test_that("get_smooths_by_id works for a GAM", {
    sm <- get_smooths_by_id(m_gam, 2L)
    expect_is(sm, "list")
    expect_true(is_mgcv_smooth(sm[[1L]]))
    expect_equal(sm[[1L]], get_smooth(m_gam, "s(x1)"))
})

test_that("get_smooths_by_id works for a GAMM", {
    sm <- get_smooths_by_id(m_gamm, 2L)
    expect_is(sm, "list")
    expect_true(is_mgcv_smooth(sm[[1L]]))
    expect_equal(sm[[1L]], get_smooth(m_gamm, "s(x1)"))
})

test_that("seq_min_max works as intended", {
    x <- rnorm(10)
    n <- 50L
    s1 <- seq_min_max(x, n = n)
    s2 <- seq(min(x), max(x), length.out = n)
    expect_equal(s1, s2)
    expect_identical(length(s1), length(s2))
    expect_identical(length(s1), n)
})

set.seed(42)
dat <- gamSim(4, n = 400, verbose = FALSE)

test_that("factor_var_names works", {
    expect_silent( result <- factor_var_names(dat))
    expect_identical("fac", result)

    expect_null( factor_var_names(dat[,1:2]) )
})

test_that("data_class works for a data frame", {
    expect_silent( result <- data_class(dat) )

    expect_named( result, names(dat) )

    actual <- c(rep("numeric", 4L), "factor", rep("numeric", 3L))
    names(actual) <- names(dat)
    expect_identical(actual, result)
})

test_that("n_smooths works for gam models", {
    expect_silent( result <- n_smooths(m_gam) )
    expect_identical(result, 4L)
})

test_that("n_smooths works for gamm models", {
    expect_silent( result <- n_smooths(m_gamm) )
    expect_identical(result, 4L)
})

test_that("n_smooths, works for objects with a smooth component", {
    expect_silent( result <- n_smooths(m_bam) )
    expect_identical( result, 4L)
})

test_that("n_smooths, fails for objects with no smooth component", {
    expect_error( result <- n_smooths(dat),
                 "Don't know how to identify smooths for <data.frame>",
                 fixed = TRUE)
})

test_that("which_smooths throws error if no smooths match the supplied term", {
    expect_error(which_smooths(m_gam, "foo"),
                 "None of the terms matched a smooth.", fixed = TRUE)
    expect_error(which_smooths(m_gamm, "foo"),
                 "None of the terms matched a smooth.", fixed = TRUE)
    expect_error(which_smooths(m_bam, "foo"),
                 "None of the terms matched a smooth.", fixed = TRUE)
})
