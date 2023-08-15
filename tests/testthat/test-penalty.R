## Test penalty()

dat <- data_sim("eg4", n = 400, seed = 42)
m <- gam(y ~ s(x0) + s(x1) + s(x2, by = fac),
         data = dat, method = "REML")

test_that("penalty() works with a simple GAM", {
    expect_silent(p <- penalty(m))
    expect_s3_class(p, "penalty_df")
    expect_named(p, c("smooth", "type", "penalty", "row", "col", "value"))
})

test_that("penalty() resclaing works with a simple GAM", {
    expect_silent(p <- penalty(m, rescale = TRUE))
    expect_s3_class(p, "penalty_df")
    expect_named(p, c("smooth", "type", "penalty", "row", "col", "value"))
})

test_that("penalty() works with a factor by smooth", {
    expect_silent(p <- penalty(m, smooth = "s(x2):fac2"))
    expect_s3_class(p, "penalty_df")
    expect_named(p, c("smooth", "type", "penalty", "row", "col", "value"))
})

test_that("penalty() rescaling works with a factor by smooth", {
    expect_silent(p <- penalty(m, smooth = "s(x2):fac2", rescale = TRUE))
    expect_s3_class(p, "penalty_df")
    expect_named(p, c("smooth", "type", "penalty", "row", "col", "value"))
})
