## Test basis() and related functions

test_that("basis() works with a simple smooth", {
    expect_silent(bs <- basis(s(x0), data = su_eg4))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "type", "by_variable", "bf", "value", "x0"))
})

test_that("basis() works with a factor by smooth", {
    expect_silent(bs <- basis(s(x2, by = fac), data = su_eg4))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "type", "by_variable", "bf", "value", "x2",
        "fac"))
})

test_that("basis() works with a gam", {
    expect_silent(bs <- basis(m_gam))
    expect_s3_class(bs, "basis")

    plt <- draw(bs)
    expect_doppelganger("draw basis works with a gam multiple smooths", plt)

    expect_silent(bs <- basis(m_gam, "s(x2)"))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "type", "by_variable", "bf", "value", "x2"))

    plt <- draw(bs)
    expect_doppelganger("draw basis works with a gam single smooth", plt)
})

test_that("basis() works with a scam", {
    skip_on_cran()
    expect_silent(bs <- basis(m_scam, "s(x2)"))
    expect_s3_class(bs, "basis")
    expect_named(bs, c("smooth", "type", "by_variable", "bf", "value", "x2"))

    plt <- draw(bs)
    expect_doppelganger("draw basis works with a scam single smooth", plt)
})
