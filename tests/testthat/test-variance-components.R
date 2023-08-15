# Test variance_components()

test_that("variance_comp works for a gam", {
    expect_silent(vc <- variance_comp(m_gam))
    expect_s3_class(df, c("variance_comp", "tbl_df", "tbl", "data.frame"))
    expect_identical(ncol(vc), 5L)
    expect_identical(nrow(vc), 5L)

    expect_silent(vc <- variance_comp(m_1_smooth))
    expect_s3_class(df, c("variance_comp", "tbl_df", "tbl", "data.frame"))
    expect_identical(ncol(vc), 5L)
    expect_identical(nrow(vc), 2L)
})
