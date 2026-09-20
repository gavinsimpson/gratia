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

test_that("ziplss residuals randomize within each count's probability mass", {
  y <- c(0, 0, 1, 2, 3, 5)
  lambda <- c(1, 3, 2, 4, 2, 5)
  positive <- c(0.3, 0.7, 0.4, 0.8, 0.6, 0.9)
  fv <- cbind(log(lambda), log(-log1p(-positive)))
  # Independent CDF: a mass at zero plus a zero-truncated Poisson for
  # positive counts. Moderate parameters avoid cancellation in this reference.
  cdf <- function(q) {
    ifelse(q < 0, 0, (1 - positive) + positive *
      (ppois(q, lambda) - exp(-lambda)) / (1 - exp(-lambda)))
  }
  lower <- cdf(y - 1)
  upper <- cdf(y)
  expect_equal(lower[y == 0], c(0, 0))
  expect_equal(upper[y == 0], 1 - positive[y == 0])
  expect_true(all(upper > lower))
  expected <- withr::with_seed(7007, runif(length(y), lower, upper))
  for (type in c("pit", "quantile")) {
    actual <- withr::with_seed(7007, do_quantile_residuals(y = y, fv = fv,
      wt = rep(1, length(y)), scale = 1, fam = mgcv::ziplss(), type = type))
    reference <- if (type == "pit") expected else qnorm(expected)
    expect_equal(actual, reference, tolerance = 1e-12)
    expect_true(all(is.finite(actual)))
    pit <- if (type == "pit") actual else pnorm(actual)
    expect_true(all(pit > lower & pit < upper))
  }
})

test_that("ziplss quantile residuals honor seeds and preserve the caller RNG", {
  withr::local_seed(7008)
  before <- .Random.seed
  pit <- quantile_residuals(m_ziplss, type = "pit", seed = 7009)
  expect_identical(.Random.seed, before)
  expect_identical(pit, quantile_residuals(m_ziplss, type = "pit", seed = 7009))
  other <- quantile_residuals(m_ziplss, type = "pit", seed = 7010)
  zero <- m_ziplss$y == 0
  expect_true(any(zero))
  expect_true(any(!zero))
  expect_false(identical(pit[zero], other[zero]))
  expect_false(identical(pit[!zero], other[!zero]))

  eta <- predict(m_ziplss, type = "link")
  lambda <- exp(eta[, 1])
  positive <- -expm1(-exp(eta[, 2]))
  cdf <- function(q) {
    ifelse(q < 0, 0, (1 - positive) + positive *
      (ppois(q, lambda) - exp(-lambda)) / (1 - exp(-lambda)))
  }
  lower <- cdf(m_ziplss$y - 1)
  upper <- cdf(m_ziplss$y)
  expected <- withr::with_seed(7009, runif(length(pit), lower, upper))
  expect_equal(pit, expected, tolerance = 1e-10)
  expect_true(all(pit > lower & pit < upper))
  quantile <- quantile_residuals(m_ziplss, type = "quantile", seed = 7009)
  expect_equal(quantile, qnorm(expected), tolerance = 1e-10)
  expect_true(all(is.finite(quantile)))
  expect_identical(.Random.seed, before)
})
