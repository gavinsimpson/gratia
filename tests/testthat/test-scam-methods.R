## Test other scam methods

test_that("smooth_estimates works for scams", {
  expect_silent(sm1 <- smooth_estimates(sw))
  expect_silent(sm2 <- smooth_estimates(sw_mdcx))
  expect_silent(sm3 <- smooth_estimates(sw_mdcv))
  expect_silent(sm4 <- smooth_estimates(m_scam))
  expect_silent(sm5 <- smooth_estimates(m_scam_micx))
  expect_silent(sm6 <- smooth_estimates(m_scam_micv))
})

test_that("vcov.scam works", {
  V <- vcov(sw)
  expect_identical(dim(V), c(5L, 5L))
  V <- vcov(sw, dispersion = 2)
  expect_identical(dim(V), c(5L, 5L))
  V <- vcov(sw, freq = TRUE)
  expect_identical(dim(V), c(5L, 5L))
  V <- vcov(sw, freq = TRUE, parametrized = TRUE)
  expect_identical(dim(V), c(5L, 5L))
  V <- vcov(sw, freq = TRUE, parametrized = FALSE)
  expect_identical(dim(V), c(5L, 5L))
  V <- vcov(sw, freq = FALSE, parametrized = FALSE)
  expect_identical(dim(V), c(5L, 5L))
})

test_that("coef.scam works", {
  beta <- coef(sw)
  expect_identical(length(beta), 5L)
  beta <- coef(sw, parametrized = FALSE)
  expect_identical(length(beta), 5L)
})
