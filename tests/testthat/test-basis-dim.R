# Test overview() & methods

test_that("basis_dim works for a GAM", {
    expect_silent(bs <- basis_dim(m_gam))
    expect_type(bs, "double")
    expect_identical(length(bs), 4L)
    expect_named(bs, smooths(m_gam))
})
