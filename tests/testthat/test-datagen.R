## Test datagen() methods

test_that("datagen is deprecated", {
  skip_if_not_installed("withr")
  withr::local_options(digits = 3)
  expect_snapshot(datagen(get_smooths_by_id(m_gam, 1)[[1L]],
    data = su_eg1, n = 10
  ))
})

test_that("datagen works for a simple 1d smooth", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(dg <- datagen(get_smooths_by_id(m_gam, 1)[[1L]],
    data = su_eg1, n = 50
  ))
  expect_s3_class(dg, "data.frame")
  expect_named(dg, c("smooth", "x"))
  expect_identical(nrow(dg), 50L)
  expect_identical(ncol(dg), 2L)
})

test_that("datagen works for a simple 2d smooth", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(dg <- datagen(get_smooths_by_id(su_m_bivar, 1)[[1L]],
    data = su_eg2, n = 10
  ))
  expect_s3_class(dg, "data.frame")
  expect_named(dg, c("smooth", "x1", "x2"))
  expect_identical(nrow(dg), 100L)
  expect_identical(ncol(dg), 3L)
})

test_that("datagen.mgcv.smooth fails for a 3d smooth", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(datagen(su_m_trivar, smooth = "s(x0,x1,x2)"),
    "Cannot handle smooths of three (3) or more terms.",
    fixed = TRUE
  )
})

test_that("datagen gam method works for a simple 1d smooth", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(dg <- datagen(m_gam, smooth = "s(x0)", n = 50))
  expect_s3_class(dg, "data.frame")
  expect_named(dg, c("smooth", "x"))
  expect_identical(nrow(dg), 50L)
  expect_identical(ncol(dg), 2L)
})

test_that("datagen gam method fails if smooth not specified", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(datagen(m_gam),
    "Argument 'smooth' must be specified and not 'NULL'.",
    fixed = TRUE
  )
})

test_that("datagen gam method fails if multiple smooths specified", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(datagen(m_gam, smooth = c("s(x0)", "s(x1)")),
    "More than one smooth requested in argument 'smooth'.",
    fixed = TRUE
  )
})

test_that("datagen gam method works for a simple 2d smooth", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(dg <- datagen(su_m_bivar, smooth = "s(x,z)", n = 10))
  expect_s3_class(dg, "data.frame")
  expect_named(dg, c("smooth", "x1", "x2"))
  expect_identical(nrow(dg), 100L)
  expect_identical(ncol(dg), 3L)
})

test_that("datagen gamm method works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(dg <- datagen(m_gamm, smooth = "s(x0)", n = 50))
  expect_s3_class(dg, "data.frame")
  expect_named(dg, c("smooth", "x"))
  expect_identical(nrow(dg), 50L)
  expect_identical(ncol(dg), 2L)
})

test_that("datagen gamm methods fails if not supplied a gamm object", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(datagen.gamm(m_gam),
    "Model doesn't appear to be a 'gamm()' model object",
    fixed = TRUE
  )
})

test_that("datagen gamm4 method works", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_silent(dg <- datagen(m_gamm4, smooth = "s(x0)", n = 50))
  expect_s3_class(dg, "data.frame")
  expect_named(dg, c("smooth", "x"))
  expect_identical(nrow(dg), 50L)
  expect_identical(ncol(dg), 2L)
})

test_that("datagen gamm methods fails if not supplied a gamm object", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(datagen.list(m_gam),
    "Model doesn't appear to be a 'gamm4()' model object",
    fixed = TRUE
  )
})
