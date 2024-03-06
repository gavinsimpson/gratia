# Test variance_components()

# reference names for tests
var_nms <- c(".component", ".variance", ".std_dev", ".lower_ci", ".upper_ci")

test_that("variance_comp works for a gam", {
  expect_silent(vc <- variance_comp(m_gam))
  expect_s3_class(df, c("variance_comp", "tbl_df", "tbl", "data.frame"))
  expect_identical(ncol(vc), 5L)
  expect_identical(nrow(vc), 5L)
  expect_named(vc, expected = var_nms)

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(vc))
})

test_that("variance_comp works for a gam with rescaling", {
  expect_silent(vc <- variance_comp(m_gam, rescale = TRUE))
  expect_s3_class(df, c("variance_comp", "tbl_df", "tbl", "data.frame"))
  expect_identical(ncol(vc), 5L)
  expect_identical(nrow(vc), 5L)
  expect_named(vc, expected = var_nms)

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(vc))
})

test_that("variance_comp works for a single term gam", {
  expect_silent(vc <- variance_comp(m_1_smooth))
  expect_s3_class(df, c("variance_comp", "tbl_df", "tbl", "data.frame"))
  expect_identical(ncol(vc), 5L)
  expect_identical(nrow(vc), 2L)
  expect_named(vc, expected = var_nms)

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(vc))
})

test_that("variance_comp works for a continuous by gam", {
  expect_silent(vc <- variance_comp(su_m_cont_by))
  expect_s3_class(df, c("variance_comp", "tbl_df", "tbl", "data.frame"))
  expect_identical(ncol(vc), 5L)
  expect_identical(nrow(vc), 2L)
  expect_named(vc, expected = var_nms)

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(vc))
})

test_that("variance_comp works for a factor by gam", {
  expect_silent(vc <- variance_comp(su_m_factor_by))
  expect_s3_class(df, c("variance_comp", "tbl_df", "tbl", "data.frame"))
  expect_identical(ncol(vc), 5L)
  expect_identical(nrow(vc), 5L)
  expect_named(vc, expected = var_nms)

  skip_on_ci()
  skip_on_cran()
  expect_snapshot(print(vc))
})
