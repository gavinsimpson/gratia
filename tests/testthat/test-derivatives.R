## Test confint() methods

## load packages
library("testthat")
library("gratia")
library("mgcv")
library("ggplot2")

context("Test derivatives() methods")

## Need a local wrapper to allow conditional use of vdiffr
`expect_doppelganger` <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(title, fig, path = path, ...)
}

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
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1", type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1", type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1", type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))
})

test_that("derivatives() returns second derivatives for all smooths in a GAM", {
    expect_silent(df <- derivatives(mod, order = 2))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1", order = 2))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1", order = 2, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1", order = 2, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x1", order = 2, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))
})

mod <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("derivatives() returns derivatives for all smooths in a GAMM", {
    expect_silent(df <- derivatives(mod))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))
})

test_that("derivatives() returns second derivatives for all smooths in a GAMM", {
    expect_silent(df <- derivatives(mod, order = 2))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))
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
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x2"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x2", type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x2", type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x2", type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))
})

test_that("derivatives() returns derivatives for all smooths in a factor by GAM", {
    expect_silent(df <- derivatives(mod, order = 2))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, order = 2, "x2"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x2", order = 2, type = "forward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x2", order = 2, type = "backward"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))

    expect_silent(df <- derivatives(mod, "x2", order = 2, type = "central"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))
})

test_that("internal finite diff functions fail for all factor vars", {
    df <- data.frame(a = factor(rep(letters[1:3], 10)),
                     b = factor(rep(LETTERS[1:3], 10)))

    expect_error( forward_finite_diff1(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( backward_finite_diff1(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( central_finite_diff1(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( forward_finite_diff2(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( backward_finite_diff2(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( central_finite_diff2(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)
})

## for Issue #64
test_that("internal finite diff functions fail for all non-numeric vars", {
    df <- data.frame(a = rep(letters[1:3], 10),
                     b = rep(LETTERS[1:3], 10))

    expect_error( forward_finite_diff1(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( backward_finite_diff1(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( central_finite_diff1(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( forward_finite_diff2(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( backward_finite_diff2(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)

    expect_error( central_finite_diff2(mod, df),
                 "Can't compute finite differences for all non-numeric data.",
                 fixed = TRUE)
})

test_that("derivatives() returns derivatives with simultaneous intervals for all smooths", {
    set.seed(1)
    dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
    mod <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
    expect_silent(df <- derivatives(mod, interval = "simultaneous"))
    expect_s3_class(df, "derivatives")
    expect_s3_class(df, "tbl_df")
    expect_named(df, c("smooth","var","data","derivative","se","crit","lower","upper"))
})

test_that("derivatives() works for factor by smooths issue 47", {
    skip_on_os("mac")
    skip_on_cran()
    set.seed(1)
    dat <- gamSim(4, n = 1000, verbose = FALSE)
    m <- gam(y ~ fac + s(x2, by = fac), data = dat, method = "REML")
    expect_silent(d <- derivatives(m))
    expect_s3_class(d, "derivatives")
    expect_s3_class(d, "tbl_df")
    expect_named(d, c("smooth","var","data","derivative","se","crit","lower","upper"))
    plt <- draw(d)
    expect_doppelganger("draw issue 47 derivatives for factor by", plt)

    m <- gam(y ~ x1 + s(x2) + fac + s(x0, by = fac), data = dat, method = "REML")
    expect_silent(d <- derivatives(m))
    expect_s3_class(d, "derivatives")
    expect_s3_class(d, "tbl_df")
    expect_named(d, c("smooth","var","data","derivative","se","crit","lower","upper"))
    plt <- draw(d)
    expect_doppelganger("draw issue 47 derivatives for complex factor by", plt)
    
    dat <- transform(dat, ofac = ordered(fac))
    m <- gam(y ~ x1 + s(x2) + ofac + s(x0) + s(x0, by = ofac), data = dat, method = "REML")
    expect_silent(d <- derivatives(m))
    expect_s3_class(d, "derivatives")
    expect_s3_class(d, "tbl_df")
    expect_named(d, c("smooth","var","data","derivative","se","crit","lower","upper"))
    plt <- draw(d)
    expect_doppelganger("draw issue 47 derivs for ordered factor by", plt)    

    m <- gamm(y ~ x1 + s(x2) + fac + s(x0, by = fac), data = dat)
    expect_silent(d <- derivatives(m))
    expect_s3_class(d, "derivatives")
    expect_s3_class(d, "tbl_df")
    expect_named(d, c("smooth","var","data","derivative","se","crit","lower","upper"))
    plt <- draw(d)
    expect_doppelganger("draw issue 47 derivatives for gamm factor by", plt)
})

test_that("derivatives() works for fs smooths issue 57", {
    skip_on_cran()
    set.seed(1)
    logistic.growth <- function(t, y0, K, r) {
        return(K * (y0 / (y0 + (K - y0) * exp(-r * t))))
    }
    N <- 16
    n <- 12
    y0 <- 0.5
    r  <- 0.25
    K  <- rnorm(N, mean=5, sd=1)
    d <- data.frame(unit = factor(rep(seq_len(N), each = n)),
                    t = rep(seq(0, 20, length = n), N))
    d <- transform(d, y = logistic.growth(t, y0, K[unit], r))
    S  <- 0.25
    d <- transform(d, y.obs = y + rnorm(nrow(d), sd = S))
    m <- gam(y.obs ~ s(t, unit, k=5, bs="fs", m=2), data=d, method="REML")
    
    expect_silent(d <- derivatives(m))
    expect_s3_class(d, "derivatives")
    expect_s3_class(d, "tbl_df")
    expect_named(d, c("smooth","var","fs_var","data","derivative","se","crit",
                      "lower","upper"))
    ## plt <- draw(d) # FIXME: need to update draw(d) so it works with fs smooths
    ## expect_doppelganger("draw issue 57 derivatives for factor by", plt)
})
