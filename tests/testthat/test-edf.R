## Test edf()

test_that("edf() works as expected", {
    expect_silent(edfs <- edf(m_gam))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 4L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_silent(edfs <- edf(m_gam, smooth = c("s(x0)", "s(x2)")))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 2L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_silent(edfs <- edf(m_gam, type = "unconditional"))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 4L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_silent(edfs <- edf(m_gam, type = "alternative"))
    expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(edfs), 4L)
    expect_identical(ncol(edfs), 2L)
    expect_named(edfs, c("smooth", "edf"))

    expect_warning(edf(m_gamgcv, type = "unconditional"),
                   "Smoothness parameter uncertainty unavailable; using `type = \"default\"`",
                   fixed = TRUE)
})

test_that("model_edf() works as expected for a single model", {
    expect_silent(medf <- model_edf(m_gam))
    expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(medf), 1L)
    expect_identical(ncol(medf), 2L)
    expect_named(medf, c("model", "edf"))
})

test_that("model_edf() works for a single model type default", {
    expect_silent(medf <- model_edf(m_gam, type = "default"))
    expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(medf), 1L)
    expect_identical(ncol(medf), 2L)
    expect_named(medf, c("model", "edf"))
})

test_that("model_edf() works for a single model type alternative", {
    expect_silent(medf <- model_edf(m_gam, type = "alternative"))
    expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(medf), 1L)
    expect_identical(ncol(medf), 2L)
    expect_named(medf, c("model", "edf"))
})

test_that("model_edf() works for a single model, type unconditional", {
    expect_silent(medf <- model_edf(m_gam, type = "unconditional"))
    expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(medf), 1L)
    expect_identical(ncol(medf), 2L)
    expect_named(medf, c("model", "edf"))
})

test_that("model_edf() works as expected for a single model", {
    expect_silent(medf <- model_edf(m_gam, m_1_smooth))
    expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
    expect_identical(nrow(medf), 2L)
    expect_identical(ncol(medf), 2L)
    expect_named(medf, c("model", "edf"))
})