## Test subsetting methods `[`

## load packages
library("testthat")
library("mgcv")
library("gratia")

set.seed(12398)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m2 <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, method = "REML")

test_that("subsetting works for smooth_samples", {
    sm <- smooth_samples(m2, seed = 42)
    hsm <- head(sm)
    expect_identical(NROW(hsm), 6L)
    expect_true(! is.null(attr(hsm, "seed")))

    attrs <- attributes(hsm)
    expect_type(attrs, "list")
    skip_on_cran() # temporary until tibble 3.0 hits CRAN
    expect_named(attrs, expected = c("names", "row.names", "class", "seed"))
})
