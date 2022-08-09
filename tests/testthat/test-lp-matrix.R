# Test lp_matrix() & methods

# load packages
library("testthat")
library("gratia")

test_that("lp_matrix works for a GAM", {
    expect_silent(xp <- lp_matrix(m_gam))
    expect_s3_class(xp, "lp_matrix")
    expect_s3_class(xp, "matrix")

    expect_silent(ds <- data_slice(m_gam, x2 = evenly(x2, n = 50)))
    expect_silent(xp <- lp_matrix(m_gam, data = ds))
    expect_s3_class(xp, "lp_matrix")
    expect_s3_class(xp, "matrix")
})
