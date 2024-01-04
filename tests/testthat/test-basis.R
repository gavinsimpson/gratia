## Test basis() and related functions

bs_nms <- c(".smooth", ".type", ".by", ".bf", ".value")

test_that("basis() works with a simple smooth", {
    expect_silent(bs <- basis(s(x0), data = su_eg4))
    expect_s3_class(bs, "basis")
    expect_named(bs, c(bs_nms, "x0"))
})

test_that("basis() works with a factor by smooth", {
    expect_silent(bs <- basis(s(x2, by = fac), data = su_eg4))
    expect_s3_class(bs, "basis")
    expect_named(bs, c(bs_nms, "x2", "fac"))
})

test_that("basis() works with a gam", {
    expect_silent(bs <- basis(m_gam))
    expect_s3_class(bs, "basis")

    plt1 <- draw(bs)

    expect_silent(bs <- basis(m_gam, "s(x2)"))
    expect_s3_class(bs, "basis")
    expect_named(bs, c(bs_nms, "x2"))

    plt2 <- draw(bs)

    skip_on_ci()
    expect_doppelganger("draw basis works with a gam multiple smooths", plt1)
    expect_doppelganger("draw basis works with a gam single smooth", plt2)
})

test_that("basis() works with a scam", {
    skip_on_cran()
    expect_silent(bs <- basis(m_scam, "s(x2)"))
    expect_s3_class(bs, "basis")
    expect_named(bs, c(bs_nms, "x2"))

    plt <- draw(bs)

    skip_on_ci()
    expect_doppelganger("draw basis works with a scam single smooth", plt)
})

test_that("basis() works with bivariate tprs smooths", {
    expect_silent(ds <- data_slice(su_eg2, x = evenly(x, n = 20),
        z = evenly(z, n = 20)))
    expect_silent(bs <- basis(s(x, z, k = 25), data = su_eg2, at = ds,
        constraints = TRUE))
    expect_s3_class(bs, "basis")

    plt1 <- draw(bs)

    skip_on_cran()
    skip_on_os("mac")
    plt2 <- draw(bs, contour = TRUE)

    skip_on_ci()
    expect_doppelganger("draw basis works with a bivariate tprs", plt1)
    expect_doppelganger("draw basis works with a bivariate tprs contour", plt2)
})

test_that("basis() works with bivariate te smooths", {
    expect_silent(ds <- data_slice(su_eg2, x = evenly(x, n = 20),
        z = evenly(z, n = 20)))
    expect_silent(bs <- basis(te(x, z, k = c(5, 5)), data = su_eg2, at = ds,
        constraints = TRUE))
    expect_s3_class(bs, "basis")

    plt1 <- draw(bs)

    skip_on_cran()
    skip_on_os("mac")
    plt2 <- draw(bs, contour = TRUE)

    skip_on_ci()
    expect_doppelganger("draw basis works with a bivariate te", plt1)
    expect_doppelganger("draw basis works with a bivariate te contour", plt2)
})
