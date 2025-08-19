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

test_that("quantile residuals fails correctly for an unsupported GAM", {
  expect_snapshot(
    quantile_residuals(m_twlss, type = "quantile", seed = 1),
    error = TRUE
  )
})
