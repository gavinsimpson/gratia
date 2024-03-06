# Test edf() and model_edf()

edf_nms <- c(".smooth", ".edf")

test_that("edf() works for a simple GAM", {
  expect_silent(edfs <- edf(m_gam))
  expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(edfs), 4L)
  expect_identical(ncol(edfs), 2L)
  expect_named(edfs, edf_nms)
  expect_snapshot(print(edfs))
})

test_that("edf() works when selecting smooths", {
  expect_silent(edfs <- edf(m_gam, smooth = c("s(x0)", "s(x2)")))
  expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(edfs), 2L)
  expect_identical(ncol(edfs), 2L)
  expect_named(edfs, edf_nms)
  expect_snapshot(print(edfs))
})

test_that("edf() works with type unconditional", {
  expect_silent(edfs <- edf(m_gam, type = "unconditional"))
  expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(edfs), 4L)
  expect_identical(ncol(edfs), 2L)
  expect_named(edfs, edf_nms)
  expect_snapshot(print(edfs))
})

test_that("edf() works with type alternative", {
  expect_silent(edfs <- edf(m_gam, type = "alternative"))
  expect_s3_class(edfs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(edfs), 4L)
  expect_identical(ncol(edfs), 2L)
  expect_named(edfs, edf_nms)
  expect_snapshot(print(edfs))
})

test_that("edf() throws warning when unconditional not available", {
  expect_warning(edf(m_gamgcv, type = "unconditional"),
    "Smoothness parameter uncertainty unavailable; using `type = \"default\"`",
    fixed = TRUE
  )
})

model_edf_nms <- c(".model", ".edf")

test_that("model_edf() works as expected for a single model", {
  expect_silent(medf <- model_edf(m_gam))
  expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(medf), 1L)
  expect_identical(ncol(medf), 2L)
  expect_named(medf, model_edf_nms)
  expect_snapshot(print(medf))
})

test_that("model_edf() works for a single model type default", {
  expect_silent(medf <- model_edf(m_gam, type = "default"))
  expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(medf), 1L)
  expect_identical(ncol(medf), 2L)
  expect_named(medf, model_edf_nms)
  expect_snapshot(print(medf))
})

test_that("model_edf() works for a single model type alternative", {
  expect_silent(medf <- model_edf(m_gam, type = "alternative"))
  expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(medf), 1L)
  expect_identical(ncol(medf), 2L)
  expect_named(medf, model_edf_nms)
  expect_snapshot(print(medf))
})

test_that("model_edf() works for a single model, type unconditional", {
  expect_silent(medf <- model_edf(m_gam, type = "unconditional"))
  expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(medf), 1L)
  expect_identical(ncol(medf), 2L)
  expect_named(medf, model_edf_nms)
  expect_snapshot(print(medf))
})

test_that("model_edf() works as expected for a single model", {
  expect_silent(medf <- model_edf(m_gam, m_1_smooth))
  expect_s3_class(medf, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(medf), 2L)
  expect_identical(ncol(medf), 2L)
  expect_named(medf, model_edf_nms)
  expect_snapshot(print(medf))
})
