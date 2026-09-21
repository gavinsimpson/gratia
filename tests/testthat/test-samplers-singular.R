# Tiny coefficient fixtures avoid fitting the large gamm4 model from #332.
test_that("Gaussian GAM draws fall back for singular covariance matrices", {
  model <- structure(list(
    coefficients = matrix(c(2, 4), ncol = 1),
    Vp = diag(c(1, 0))
  ), class = "gam")
  withr::local_seed(42)

  for (n in c(1L, 8L)) {
    expect_warning(draws <- gaussian_draws(model, n = n),
      "Cholesky decomposition failed; using mgcv::rmvn().", fixed = TRUE)
    expect_identical(dim(draws), c(n, 2L))
    expect_true(all(is.finite(draws)))
    expect_equal(draws[, 2], rep(4, n))
  }

  set.seed(42)
  expect_warning(first <- gaussian_draws(model, n = 8), "Cholesky")
  set.seed(42)
  expect_warning(second <- gaussian_draws(model, n = 8), "Cholesky")
  expect_identical(first, second)

  # Selecting the nonsingular part must not trigger fallback.
  expect_silent(selected <- gaussian_draws(model, n = 8, index = 1))
  expect_identical(dim(selected), c(8L, 1L))
})

test_that("Successful mvnfast draws retain their seeded results", {
  model <- structure(list(coefficients = c(2, 4), Vp = diag(2)),
    class = "gam")
  local_mocked_bindings(rmvn = function(...) stop("Unexpected fallback"),
    .package = "mgcv")
  withr::local_seed(42)
  expected <- mvnfast::rmvn(8, model$coefficients, model$Vp, ncores = 1)
  set.seed(42)
  expect_silent(actual <- gaussian_draws(model, n = 8))
  expect_identical(actual, expected)
})

test_that("Unrelated mvnfast errors propagate unchanged", {
  model <- structure(list(coefficients = c(2, 4), Vp = diag(2)),
    class = "gam")
  original <- simpleError("Unrelated sampler failure")
  local_mocked_bindings(rmvn = function(...) stop(original),
    .package = "mvnfast")
  local_mocked_bindings(rmvn = function(...) stop("Unexpected fallback"),
    .package = "mgcv")
  expect_identical(
    tryCatch(gaussian_draws(model, n = 8), error = identity), original)
})

test_that("Errors from the fallback sampler propagate unchanged", {
  model <- structure(list(coefficients = c(2, 4), Vp = diag(c(1, 0))),
    class = "gam")
  original <- simpleError("Fallback sampler failure")
  local_mocked_bindings(rmvn = function(...) stop(original),
    .package = "mgcv")
  expect_warning(
    error <- tryCatch(gaussian_draws(model, n = 8), error = identity),
    "Cholesky decomposition failed; using mgcv::rmvn().", fixed = TRUE)
  expect_identical(error, original)
})

test_that("Explicit mgcv sampling bypasses mvnfast", {
  model <- structure(list(coefficients = c(2, 4), Vp = diag(c(1, 0))),
    class = "gam")
  local_mocked_bindings(rmvn = function(...) stop("Unexpected mvnfast call"),
    .package = "mvnfast")
  withr::local_seed(42)
  expect_silent(draws <- gaussian_draws(model, n = 8, mvn_method = "mgcv"))
  expect_identical(dim(draws), c(8L, 2L))
  expect_equal(draws[, 2], rep(4, 8))
})
