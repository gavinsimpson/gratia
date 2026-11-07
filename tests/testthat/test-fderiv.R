## Test `fderiv()` function

mod <- su_m_factor_by

test_that("fderiv() is deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(result <- fderiv(mod), class = "lifecycle_warning_deprecated")
  expect_s3_class(result, "fderiv")
})

test_that("fderiv() agrees with native prediction differences", {
  withr::local_options(lifecycle_verbosity = "quiet")
  data <- model.frame(mod)[c(2, 7, 11), ]
  eps <- 1e-5
  result <- fderiv(mod, newdata = data, eps = eps)
  for (smooth in mod$smooth) {
    shifted <- data
    shifted[[smooth$term]] <- shifted[[smooth$term]] + eps
    expected <- (predict(mod, shifted, type = "terms") -
      predict(mod, data, type = "terms")) / eps
    columns <- smooth$first.para:smooth$last.para
    X <- (predict(mod, shifted, type = "lpmatrix") -
      predict(mod, data, type = "lpmatrix"))[, columns, drop = FALSE] / eps
    covariance <- vcov(mod)[columns, columns, drop = FALSE]
    actual <- result$derivatives[[smooth$label]]
    expect_equal(as.numeric(actual$deriv), as.numeric(expected[, smooth$label]),
      tolerance = 1e-7)
    expect_equal(actual$se.deriv, sqrt(rowSums((X %*% covariance) * X)),
      ignore_attr = TRUE, tolerance = 1e-7)
  }
})

test_that("fderiv() can create newdata with factors in model", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ## Example from https://github.com/scottkosty/tsgam/commit/a964ef3fcfc6847f737bd54e4d831b97d9d8b280
  fd <- fderiv(mod) # shouldn't thrown an error
  expect_s3_class(fd, "fderiv")
})

test_that("fderiv() can handle factors in user-supplied newdata", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ## Example from https://github.com/scottkosty/tsgam/commit/80293d4887ef322686d056ad54dcd183cdab0966
  newd <- delete_response(mod, su_eg4)
  expect_silent(fd <- fderiv(mod, newdata = su_eg4))
  expect_s3_class(fd, "fderiv")
})

test_that("fderiv() can handle offsets", {
  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")
  ## Example from https://github.com/scottkosty/tsgam/commit/80293d4887ef322686d056ad54dcd183cdab0966
  mod <- gam(y ~ fac + s(x1) + offset(x0),
    data = su_eg4,
    method = "REML"
  )
  expect_silent(fd <- fderiv(mod))
  expect_s3_class(fd, "fderiv")
  expect_silent(fd <- fderiv(mod, newdata = su_eg4))
  expect_s3_class(fd, "fderiv")
})
