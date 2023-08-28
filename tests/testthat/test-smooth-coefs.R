## Test smooth_coefs() and related functions

skip_on_ci()

test_that("smooth_coefs() works with a gam", {
    expect_silent(b <- smooth_coefs(m_gam, term = "s(x0)"))
    expect_named(b, paste("s(x0)", 1:9, sep = "."))
    expect_identical(length(b), 9L)
})

test_that("smooth_coefs() throws error without term", {
    expect_error(smooth_coefs(m_gam),
        "argument \"term\" is missing, with no default")
})

test_that("smooth_coefs() works with a gamm", {
    expect_silent(b <- smooth_coefs(m_gamm, term = "s(x0)"))
    expect_named(b, paste("s(x0)", 1:9, sep = "."))
    expect_identical(length(b), 9L)
})

test_that("smooth_coefs() works with a gamm4", {
    expect_silent(b <- smooth_coefs(m_gamm4, term = "s(x0)"))
    expect_named(b, paste("s(x0)", 1:9, sep = "."))
    expect_identical(length(b), 9L)
})

test_that("smooth_coefs() works with a proper gamm4 class", {
    expect_silent(b <- smooth_coefs(m_gamm4_real, term = "s(x0)"))
    expect_named(b, paste("s(x0)", 1:9, sep = "."))
    expect_identical(length(b), 9L)
})

test_that("smooth_coefs() works with a bam", {
    expect_silent(b <- smooth_coefs(m_bam, term = "s(x0)"))
    expect_named(b, paste("s(x0)", 1:9, sep = "."))
    expect_identical(length(b), 9L)
})

test_that("smooth_coefs() works with a scam", {
    expect_silent(b <- smooth_coefs(m_scam, term = "s(x2)"))
    expect_named(b, paste("s(x2)", 1:9, sep = "."))
    expect_identical(length(b), 9L)
})

test_that("smooth_coefs() works with a mgcv.smooth", {
    sm <- get_smooth(m_gam, term = "s(x0)")
    expect_silent(b <- smooth_coefs(sm, model = m_gam))
    expect_named(b, paste("s(x0)", 1:9, sep = "."))
    expect_identical(length(b), 9L)
})

test_that("smothh-coefs print() output is as expected", {
    skip_on_ci()
    expect_snapshot({
        print(smooth_coefs(m_gam, term = "s(x0)"))
    })
})
