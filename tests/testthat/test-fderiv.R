## Test `analog()` function

## load packages
library("testthat")
library("tsgam")
library("mgcv")

context("Testing fedriv()")

test_that("fderiv() can create newdata with factors in model", {
    ## Example from https://github.com/scottkosty/tsgam/commit/a964ef3fcfc6847f737bd54e4d831b97d9d8b280
    dat <- gamSim(4, n = 401, dist = "normal", scale = 2)
    mod <- gam(y ~ s(x0) + s(x1) + fac, data = dat, method = "REML")
    fd <- fderiv(mod)                   # shouldn't thrown an error
    expect_is(fd, "fderiv")
})

test_that("fderiv() can handle factors in user-supplied newdata", {
    ## Example from https://github.com/scottkosty/tsgam/commit/80293d4887ef322686d056ad54dcd183cdab0966
    dat <- gamSim(4, n = 400, dist = "normal", scale = 2)
    mod <- gam(y ~ s(x0) + s(x1) + fac, data = dat, method = "REML")
    newd <- dat[1,]
    fd <- fderiv(mod, newdata = newd)   # shouldn't thrown an error
    expect_is(fd, "fderiv")
})
