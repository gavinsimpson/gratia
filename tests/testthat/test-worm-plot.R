## Test worm_plot() methods

## simulate binomial data...
# set.seed(0)
n.samp <- 200
dat <- data_sim("eg1", n = n.samp, dist = "binary",
                scale = .33, seed = 0)
p <- binomial()$linkinv(dat$f)               # binomial p
n  <- withr::with_seed(0, sample(c(1, 3), n.samp, replace = TRUE)) # binomial n
dat  <- withr::with_seed(0, transform(dat, y = rbinom(n, n, p), n = n))
m <- gam(y / n ~ s(x0) + s(x1) + s(x2) + s(x3),
         family = binomial, data = dat, weights = n,
         method = "REML")

types <- c("deviance", "response", "pearson")
methods <- c("uniform", "simulate", "normal")

# uniform randomisation of uniform quantiles
test_that("worm_plot() uniform method works", {
    plt <- withr::with_seed(42, worm_plot(m))
    expect_doppelganger("worm_plot uniform randomisation", plt)
})

test_that("worm_plot() uniform method works with response residuals", {
    plt <- withr::with_seed(42, worm_plot(m, type = "response"))
    expect_doppelganger("worm_plot uniform randomisation response residuals",
        plt)
})

test_that("worm_plot() uniform method works with pearson residuals", {
    plt <- withr::with_seed(42, worm_plot(m, type = "pearson"))
    expect_doppelganger("worm_plot uniform randomisation pearson residuals",
        plt)
})

# normality assumption
test_that("worm_plot() normal method works", {
    plt <- worm_plot(m, method = "normal")
    expect_doppelganger("worm_plot normality assumption", plt)
})

test_that("worm_plot() normal method works", {
    plt <- worm_plot(m, method = "normal", type = "response")
    expect_doppelganger("worm_plot normality assumption response residuals",
        plt)
})

test_that("worm_plot() normal method works", {
    plt <- worm_plot(m, method = "normal", type = "pearson")
    expect_doppelganger("worm_plot normality assumption pearson residuals",
        plt)
})

# simulate data to get quantiles
test_that("worm_plot() simulate method works", {
    plt <- withr::with_seed(42, worm_plot(m, method = "simulate"))
    expect_doppelganger("worm_plot data simulation", plt)
})

test_that("worm_plot() simulate method works", {
    plt <- withr::with_seed(42, worm_plot(m, method = "simulate",
        type = "response"))
    expect_doppelganger("worm_plot data simulation response residuals", plt)
})

test_that("worm_plot() simulate method works", {
    plt <- withr::with_seed(42, worm_plot(m, method = "simulate",
        type = "pearson"))
    expect_doppelganger("worm_plot data simulation pearson residuals", plt)
})

test_that("worm_plot() fails if unsupported residuals requested", {
    expect_error(worm_plot(m, type = "scaled.pearson"),
                 paste("'arg' should be one of",
                       paste(dQuote(types), collapse = ', ')),
                 fixed = TRUE)
})

test_that("worm_plot() fails if unsupported method requested", {
    expect_error(worm_plot(m, method = "foo"),
                 paste("'arg' should be one of",
                       paste(dQuote(methods), collapse = ', ')),
                 fixed = TRUE)
})

test_that("worm_plot() prints message if direct method requested", {
    expect_message(worm_plot(m, method = "direct"),
                   "`method = \"direct\"` is deprecated, use `\"uniform\"`",
                   fixed = TRUE)
})

test_that("worm_plot.default fails with error", {
    expect_error(worm_plot(dat),
                 "Unable to produce a worm plot for <data.frame>")
})

