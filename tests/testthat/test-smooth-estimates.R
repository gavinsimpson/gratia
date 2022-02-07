# Test draw() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")

dat <- data_sim("eg1", n = 400, seed = 1)
#m_1_smooth <- gam(y ~ s(x0), data = dat, method = "REML")
m1 <- gam(y ~ s(x0) + s(x1, bs = 'cr') + s(x2, bs = 'ps') + s(x3, bs = 'bs'),
          data = dat, method = "REML")
m2 <- gamm(y ~ s(x0) + s(x1, bs = 'cr') + s(x2, bs = 'ps') + s(x3, bs = 'bs'),
           data = dat, method = "REML")
m3 <- gam(y ~ s(x0, x1, x2), data = dat, method = "REML")
m4 <- gam(y ~ te(x0, x1, x2), data = dat, method = "REML")
m_t2 <- gam(y ~ t2(x0, x1, x2), data = dat, method = "REML")
m_ti <- gam(y ~ s(x0) + s(x1) + ti(x0, x1), data = dat, method = "REML")

dat_2d_by <- data_sim("eg4", n = 400, seed = 42)
m_2d_by <- gam(y ~ fac + s(x0, x1, by = fac), data = dat_2d_by)

## simulate example... from ?mgcv::random.effects
## simulate 4 term additive truth
# dat_re <- data_sim("eg1", n = 400, scale = 2, seed = 1)
# fac <- as.factor(sample(1:20, 400, replace = TRUE))
# X <- model.matrix(~ fac - 1)
# b <- rnorm(20) * 0.5
# dat_re <- transform(dat_re, y = drop(y + X %*% b), fac = fac)
# m_re <- gam(y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) + s(x3),
#             data = dat, method = "ML")
# rm(fac, b, X)

# dat <- data_sim("eg1", n = 400, seed = 1)
# m_1_smooth <- gam(y ~ s(x0), data = dat, method = "REML")
# m1 <- gam(y ~ s(x0) + s(x1, bs = 'cr') + s(x2, bs = 'ps') + s(x3, bs = 'bs'),
#           data = dat, method = "REML")
# m2 <- gamm(y ~ s(x0) + s(x1, bs = 'cr') + s(x2, bs = 'ps') + s(x3, bs = 'bs'),
#            data = dat, method = "REML")
# m3 <- gam(y ~ s(x0, x1, x2), data = dat, method = "REML")
# m4 <- gam(y ~ te(x0, x1, x2), data = dat, method = "REML")

## simulate example... from ?mgcv::factor.smooth.interaction
sim_fs <- function(n = 500, nf = 10) {
    f0 <- function(x) 2 * sin(pi * x)
    f1 <- function(x, a=2, b=-1) exp(a * x)+b
    f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * x)^3 *
        (1 - x)^10
    n <- 500
    nf <- 10
    fac <- sample(1:nf, n, replace=TRUE)
    x0 <- runif(n)
    x1 <- runif(n)
    x2 <- runif(n)
    a <- rnorm(nf) * .2 + 2;
    b <- rnorm(nf) * .5
    f <- f0(x0) + f1(x1, a[fac], b[fac]) + f2(x2)
    fac <- factor(fac)
    y <- f + rnorm(n) * 2
    df <- data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, fac = fac)
    df
}
set.seed(0)
dat_fs <- sim_fs()
m_fs <- gam(y ~ s(x1, fac, bs="fs", k=5), method = "ML", data = dat_fs)

test_that("smooth_estimates works for a GAM", {
    sm <- smooth_estimates(m1, "s(x2)")
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
})

test_that("smooth_estimates works with more than one term", {
    sm <- smooth_estimates(m1, c("s(x1)", "s(x2)"))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
})

test_that("smooth_estimates throws error if smooth not found", {
    expect_error(smooth_estimates(m1, smooth = "s(z)"),
                 "Failed to match any smooths in model `m1`",
                 fixed = TRUE)
})

test_that("smooth_estimates works for a GAMM", {
    sm <- smooth_estimates(m2, "s(x2)")
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
})

test_that("smooth_estimates works with a bivariate TPRS smooth", {
    expect_silent(sm <- smooth_estimates(su_m_bivar, "s(x,z)", n = 50))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 2500L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x", "z"))
})

test_that("smooth_estimates works with a bivariate TPRS smooth with dist", {
    expect_silent(sm <- smooth_estimates(su_m_bivar, "s(x,z)", n = 50,
                                         dist = 0.1))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 2500L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x", "z"))
})

test_that("smooth_estimates works with a bivariate te smooth", {
    expect_silent(sm <- smooth_estimates(su_m_bivar_te, "te(x,z)", n = 50))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 2500L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x", "z"))
})

test_that("smooth_estimates works with a bivariate te smooth with dist", {
    expect_silent(sm <- smooth_estimates(su_m_bivar_te, "te(x,z)", n = 50,
                                         dist = 0.1))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 2500L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x", "z"))
})

test_that("smooth_estimates works with a trivariate smooth", {
    expect_silent(sm <- smooth_estimates(m3, "s(x0,x1,x2)", n = 25))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 15625L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x0", "x1", "x2"))
})

test_that("smooth_estimates works with a trivariate tensor product smooth", {
    expect_silent(sm <- smooth_estimates(m4, "te(x0,x1,x2)", n = 25))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 15625L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x0", "x1", "x2"))
})

test_that("smooth_estimates works with a trivariate t2 tensor product smooth", {
    expect_silent(sm <- smooth_estimates(m_t2, "t2(x0,x1,x2)", n = 25))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 15625L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x0", "x1", "x2"))
})

test_that("smooth_estimates works with a tensor product interaction smooth", {
    expect_silent(sm <- smooth_estimates(m_ti, "ti(x0,x1)", n = 25))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 625L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x0", "x1"))
})

test_that("smooth_estimates works", {
    expect_silent(sm <- smooth_estimates(rm1, smooth = "s(fac)"))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), length(levels(su_re[["fac"]])))
    expect_named(sm, c("smooth", "type", "by", "est", "se", "fac"))
})

test_that("smooth_estimates works when passed data", {
    expect_silent(sm <- smooth_estimates(rm1, smooth = "s(fac)",
                                         data = su_re))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), nrow(su_re))
    expect_named(sm, c("smooth", "type", "by", "est", "se", "fac"))
})

test_that("check_user_data fails if smooth var not in data", {
    id <- which(names(dat) == "x0")
    expect_error(check_user_data(data = dat[, -id], "x0"),
                 "Variable(s) 'x0' not found in 'data'.",
                 fixed = TRUE)
})

test_that("smooth_estimates works with vector data", {
    sm1 <- smooth_estimates(m_1_smooth, "s(x0)", data = dat[, "x0"])
    sm2 <- smooth_estimates(m_1_smooth, "s(x0)", data = dat)
    expect_s3_class(sm1, "smooth_estimates")
    expect_equal(sm1, sm2)
})

test_that("smooth_estimates fails if data is not data frame or numeric", {
    expect_error(smooth_estimates(m1, "s(x0)", data = list(x0 = dat[, "x0"])),
                 "'data', if supplied, must be a numeric vector or data frame.",
                 fixed = TRUE)
})


test_that("smooth_estimates works for a 2d factor by smooth", {
    expect_silent(sm <- smooth_estimates(m_2d_by, "s(x0,x1)", 
                                         partial_match = TRUE))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
})

test_that("smooth_estimates works for a factor smooth", {
    expect_silent(sm <- smooth_estimates(m_fs))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), 1000L) # 100 n * 10 levels
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x1", "fac"))

    newdf <- data.frame(x1 = dat_fs$x1, fac = dat_fs$fac)
    expect_silent(sm <- smooth_estimates(m_fs, "s(x1,fac)", data = newdf))
    expect_s3_class(sm, "smooth_estimates")
    expect_s3_class(sm, "tbl_df")
    expect_s3_class(sm, "data.frame")
    expect_identical(nrow(sm), nrow(newdf))
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x1", "fac"))

    newdf <- data.frame(x4 = 1:10, fac = factor(2, levels = 1:10))

    expect_error(smooth_estimates(m_fs, "s(x1,fac)", data = newdf),
                 "Variable(s) 'x1' not found in 'data'.", fixed = TRUE)

    expect_error(smooth_estimates(m_fs, "s(x1,fac)",
                                  data = dat_fs[, !names(dat_fs) == "fac"]),
                 "Variable(s) 'fac' not found in 'data'.",
                 fixed = TRUE)
})

## check_user_data
test_that("check_user_data throws error when passed a vector with vars >1", {
    expect_error(check_user_data(1:10, vars = c("x1", "x2")),
                 "'smooth' requires multiple data vectors but only 1 provided.",
                 fixed = TRUE)
})

test_that("check_user_data errors when passed a non-numeric vector", {
    expect_error(check_user_data(LETTERS, vars = "x1"),
                 "'data', if supplied, must be a numeric vector or a data frame.",
                 fixed = TRUE)
})

test_that("check_user_data works when passed a vector with 1 var", {
    expect_silent(df <- check_user_data(1:10, vars = "x1"))
    expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
})

test_that("smooth_estimates works for m_gamm4", {
    expect_silent(sm <- smooth_estimates(m_gamm4, "s(x2)"))
    expect_s3_class(sm, c("smooth_estimates", "tbl_df", "tbl", "data.frame"))
    expect_identical(ncol(sm), 6L)
    expect_identical(nrow(sm), 100L)
    expect_named(sm, c("smooth", "type", "by", "est", "se", "x2"))
})
