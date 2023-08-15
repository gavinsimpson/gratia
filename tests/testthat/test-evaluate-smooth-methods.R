# Test draw() methods

# set.seed(1)
# dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
dat <- data_sim("eg1", n = 400, dist = "normal", scale = 2, seed = 1)
m1 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")
m2 <- gamm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("evaluate_smooth is deprecated", {
    skip_on_cran()
    skip_on_ci()
    skip_if_not_installed("withr")
    withr::local_options(digits = 3)
    expect_snapshot(evaluate_smooth(m1, smooth = "s(x0)"))
})

test_that("evaluate_smooth works for a GAM", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    sm <- evaluate_smooth(m1, "s(x2)")
    expect_s3_class(sm, "evaluated_1d_smooth")
    expect_s3_class(sm, "evaluated_smooth")
    expect_s3_class(sm, "data.frame")
})

test_that("evaluate_smooth throws a message with more than one term", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    expect_message(evaluate_smooth(m1, c("s(x1)", "s(x2)")),
                   "Supplied more than 1 'smooth'; using only the first")
})

test_that("evaluate_smooth throws error if smooth not found", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    expect_error(evaluate_smooth(m1, smooth = "s(z)"),
                 "Requested smooth 's(z)' not found",
                 fixed = TRUE)
})

test_that("evaluate_smooth works for a GAMM", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    sm <- evaluate_smooth(m2, "s(x2)")
    expect_s3_class(sm, "evaluated_1d_smooth")
    expect_s3_class(sm, "evaluated_smooth")
    expect_s3_class(sm, "data.frame")
})

test_that("evaluate_1d_smooth fails with multiple smooths that aren't by factor smooths", {
    withr::local_options(lifecycle_verbosity = "quiet")
    expect_error(gratia:::evaluate_1d_smooth(m1[["smooth"]]),
                 "Not all of these are 'by' variable smooths")
})

## test_that("evaluate_2d_smooth fails with multiple smooths that aren't by factor smooths", {
##    skip_if_not_installed("withr")
##    withr::local_options(lifecycle_verbosity = "quiet")
##     ## need to rethink this test
##     expect_error(gratia:::evaluate_2d_smooth(m1[["smooth"]]),
##                  "Not all of these are 'by' variable smooths")
## })

test_that("evaluate_fs_smooth fails with multiple smooths that aren't by factor smooths", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    expect_error(gratia:::evaluate_fs_smooth(m1[["smooth"]]),
                 "Not all of these are 'by' variable smooths")
})

## test_that("evaluate_re_smooth fails with multiple smooths that aren't by factor smooths", {
##    skip_if_not_installed("withr")
##    withr::local_options(lifecycle_verbosity = "quiet")
##     expect_error(gratia:::evaluate_re_smooth(m1[["smooth"]]),
##                  "Not all of these are 'by' variable smooths")
## })

test_that("evaluate_smooth fails with a trivariate smooth", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    m <- gam(y ~ s(x0, x1, x2), data = dat, method = "REML")
    expect_error(evaluate_smooth(m, "s(x0,x1,x2)"))
    m <- gam(y ~ te(x0, x1, x2), data = dat, method = "REML")
    expect_error(evaluate_smooth(m, "s(x0,x1,x2)"))
})

test_that("evaluate_re_smooth throws error when passed newdata", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    ## simulate example... from ?mgcv::random.effects
    # set.seed(1)
    dat <- data_sim("eg1", n = 400, scale = 2, seed = 1)

    fac <- as.factor(sample(1:20, 400, replace = TRUE))
    dat$X <- model.matrix(~ fac - 1)
    b <- rnorm(20) * 0.5
    dat <- transform(dat, y = y + X %*% b)

    rm1 <- gam(y ~ s(fac, bs = "re") + s(x0) + s(x1) + s(x2) +
                   s(x3), data = dat, method = "ML")

    expect_error(evaluate_smooth(rm1, smooth = "s(fac)", newdata = model.frame(rm1)),
                 "Not yet implemented: user-supplied data in 're' smooth")
})

test_that("evaluate_1d_smooth fails if smooth var not in newdata", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    m <- gam(y ~ s(x0), data = dat, method = "REML")
    id <- which(names(dat) == "x0")
    expect_error(evaluate_smooth(m, "s(x0)", newdata = dat[, -id]),
                 "Variable x0 not found in 'newdata'.",
                 fixed = TRUE)
})

test_that("evaluate_1d_smooth works with vector newdata", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    m <- gam(y ~ s(x0), data = dat, method = "REML")
    sm1 <- evaluate_smooth(m, "s(x0)", newdata = dat[, "x0"])
    sm2 <- evaluate_smooth(m, "s(x0)", newdata = dat)
    expect_s3_class(sm1, "evaluated_1d_smooth")
    expect_equal(sm1, sm2)
})

test_that("evaluate_1d_smooth fails if newdata is not data frame or numeric", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    expect_error(evaluate_smooth(m1, "s(x0)", newdata = list(x0 = dat[, "x0"])),
                 "'newdata', if supplied, must be a numeric vector or a data frame.",
                 fixed = TRUE)
})

test_that("evaluate_2d_smooth fails if smooth var not in newdata", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    m <- gam(y ~ s(x0, x1), data = dat, method = "REML")
    id <- which(names(dat) == "x0")
    expect_error(evaluate_smooth(m, "s(x0,x1)", newdata = dat[, -id]),
                 "Variable x0 not found in 'newdata'.",
                 fixed = TRUE)
})

test_that("evaluate_2d_smooth fails if newdata is not data frame or numeric", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    m <- gam(y ~ s(x0, x1), data = dat, method = "REML")
    expect_error(evaluate_smooth(m, "s(x0,x1)", newdata = list(x0 = dat[, "x0"])),
                 "'newdata', if supplied, must be a numeric vector or a data frame.",
                 fixed = TRUE)
})

test_that("evaluate_2d_smooth works for a 2d factor by smooth", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    # set.seed(42)
    dat <- data_sim("eg4", n = 400, seed = 42)
    mf <- gam(y ~ fac + s(x0, x1, by = fac), data = dat)
    sm <- evaluate_smooth(mf, "s(x0,x1)")
    expect_s3_class(sm, "evaluated_2d_smooth")
    expect_s3_class(sm, "evaluated_smooth")
    expect_s3_class(sm, "data.frame")
})

test_that("evaluate_fs_smooth() ", {
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    ## simulate example... from ?mgcv::factor.smooth.interaction
    # set.seed(0)
    ## simulate data...
    df <- withr::with_seed(0, {
        f0 <- function(x) 2 * sin(pi * x)
        f1 <- function(x, a = 2, b = -1) exp(a * x) + b
        f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
            (10 * x)^3 * (1 - x)^10
        n <- 500
        nf <- 10
        fac <- sample(1:nf, n, replace = TRUE)
        x0 <- runif(n)
        x1 <- runif(n)
        x2 <- runif(n)
        a <- rnorm(nf) * .2 + 2
        b <- rnorm(nf) * .5
        f <- f0(x0) + f1(x1, a[fac], b[fac]) + f2(x2)
        fac <- factor(fac)
        y <- f + rnorm(n) * 2

        data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, fac = fac)
    })
    mod <- gam(y ~ s(x1, fac, bs="fs", k=5), method = "ML")

    newdf <- data.frame(x4 = 1:10, fac = factor(2, levels = 1:10))

    expect_error( evaluate_smooth(mod, "x1", newdata = newdf),
                 "Variable x1 not found in 'newdata'.", fixed = TRUE)

    expect_error( evaluate_smooth(mod, "x1", newdata = newdf$x4),
                 "'newdata', if supplied, must be a data frame.", fixed = TRUE)

    newdf <- data.frame(x1 = x1, fac = fac)
    expect_silent( evaluate_smooth(mod, "x1", newdata = newdf) )
})

test_that("evaluate_re_smooth works with ordered factor #99", {
    skip_on_cran()
    skip_if_not_installed("withr")
    withr::local_options(lifecycle_verbosity = "quiet")
    m <- gam(uptake ~ s(conc, Type, bs = 'fs', k = 5) + s(Plant, bs = 're'),
             family = Gamma('log'), data = CO2)
    expect_silent(sm <- evaluate_smooth(m, "Plant"))
    expect_identical(nrow(sm), nlevels(CO2[["Plant"]]))
})
