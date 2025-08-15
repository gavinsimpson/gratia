test_that("fix family cdf is null if we can not fix it", {
  expect_null(fix_family_cdf(quasipoisson())$cdf)
})

test_that("fix family cdf works for a GAM", {
  expect_equal(
    fix_family_cdf(gaussian())$cdf, cdf_gaussian
  )
})

test_that("cdf fun works for gaussian cdf", {
  expect_snapshot(
    cdf_gaussian(
      q = c(-1, 3, 2.3), mu = c(0, 2, 1.5), wt = rep(1, 3), scale = c(1, 2, 1)
    )
  )
})

test_that("cdf fun works for poisson cdf", {
  expect_snapshot(
    cdf_poisson(
      q = c(0, 1, 3), mu = c(0.1, 1.3, 3.5), wt = rep(1, 3), scale = c(1, 1, 1)
    )
  )
})

test_that("cdf fun works for binomial cdf", {
  expect_snapshot(
    cdf_binomial(
      q = c(0.01, 0.7, 0.8), mu = c(0.1, 0.5, 0.7), wt = rep(1, 3),
      scale = c(1, 1, 1)
    )
  )
})

test_that("cdf fun works for gamma cdf", {
  expect_snapshot(
    cdf_gamma(
      q = c(1.1, 3.2, 2.3), mu = c(2, 2, 1.5), wt = rep(1, 3),
      scale = c(1, 2, 1)
    )
  )
})