test_that("quantile residuals works for a GAM and pit", {
  expect_snapshot(
    head(quantile_residuals(m_gam, type = "pit", seed = 1), n = 10)
  )
})

test_that("quantile residuals works for a poisson GAM and quantile", {
  expect_snapshot(
    head(quantile_residuals(b_pois, type = "quantile", seed = 1), n = 10)
  )
})

test_that("quantile residuals fails correctly for a nb GAM", {
  expect_snapshot(
    head(quantile_residuals(m_nb, type = "quantile", seed = 1), n = 10),
    error = TRUE
  )
})
