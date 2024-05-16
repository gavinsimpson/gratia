## Test difference_smooths()

test_that("difference_smooths() works for a gam model", {
  expect_silent(ds <- difference_smooths(su_m_factor_by, select = "s(x2)"))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths gam", plt)
})

test_that("difference_smooths() works for a gam model including group means", {
  expect_silent(ds <- difference_smooths(su_m_factor_by,
    select = "s(x2)",
    group_means = TRUE
  ))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths gam inc grp means", plt)
})

test_that("difference_smooths() works for a gam model fixed scales", {
  expect_silent(ds <- difference_smooths(su_m_factor_by, select = "s(x2)"))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds, scales = "fixed")

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths gam fixed scales", plt)
})

test_that("difference_smooths() works for a gam model fixed scales", {
  expect_silent(ds <- difference_smooths(su_m_factor_by, select = "s(x2)"))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds, ref_line = TRUE)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths gam ref line", plt)
})

test_that("difference_smooths() works for a bam model", {
  skip_on_cran()
  expect_silent(ds <- difference_smooths(su_m_factor_by_bam,
    select = "s(x2)"
  ))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths bam", plt)
})

test_that("difference_smooths() works for a gamm model", {
  skip_on_cran()
  skip_on_os(c("windows"))
  expect_silent(ds <- difference_smooths(su_m_factor_by_gamm,
    select = "s(x2)"
  ))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths gamm", plt)
})

test_that("difference_smooths() works for a gamm4 model", {
  skip_on_cran()
  skip_on_os(c("windows"))
  expect_silent(ds <- difference_smooths(su_m_factor_by_gamm4,
    select = "s(x2)"
  ))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths gamm4", plt)
})

test_that("difference_smooths() works with user data", {
  df <- with(
    su_eg4,
    expand_grid(
      fac = factor(levels(fac), levels = levels(fac)),
      x2 = evenly(x2, n = 100),
      x0 = mean(x0)
    )
  )
  expect_silent(ds <-
    difference_smooths(su_m_factor_by, select = "s(x2)", data = df))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt <- draw(ds)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths gam user data", plt)
})

test_that("difference_smooths() works for a bivariate gam", {
  skip_on_cran()
  expect_silent(ds <- difference_smooths(su_m_bivar_by_fac,
    select = "s(x,z)"
  ))
  expect_s3_class(ds, c("difference_smooth", "tbl_df", "tbl", "data.frame"))

  ## plot
  plt1 <- draw(ds)

  ## plot
  plt2 <- draw(ds, contour = TRUE)

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw difference_smooths bivariate gam", plt1)
  expect_doppelganger("draw difference_smooths bivariate gam contours", plt2)
})

test_that("smooth arg is deprecated in difference_smooths()", {
  expect_warning(ds <- difference_smooths(su_m_factor_by, smooth = "s(x2)"),
    "deprecated")
})

test_that("difference smooths works with a decomposed model issue 223", {
  skip_on_cran()
  data("iris")
  m_223 <- gam(Petal.Width ~ Species +
    s(Sepal.Length, k = 4) +
    s(Sepal.Length, by = Species, k = 4) +
    s(Sepal.Width, k = 4) +
    s(Sepal.Width, by = Species, k = 4) +
    ti(Sepal.Length, Sepal.Width, k = 4) +
    ti(Sepal.Length, Sepal.Width, by = Species, k = 4),
  method = "REML",
  data = iris)
  sm_take <- "ti(Sepal.Length,Sepal.Width)"
  expect_silent(d <- difference_smooths(m_223, select = sm_take, n = 50))
  expect_identical(nrow(d), 7500L) # 3 comparisons * 50 * 50 grid

  # skip_on_ci() # testing without as moved to mac os x
  expect_snapshot(print(d))
})
