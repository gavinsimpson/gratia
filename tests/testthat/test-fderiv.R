## Test `fderiv()` function

mod <- gam(y ~ s(x0) + s(x1) + fac, data = su_eg4, method = "REML")

test_that("fderiv is deprecated", {
  skip_on_cran()
  skip_on_ci() # testing without as moved to mac os x
  skip_on_covr()
  skip_on_os(os = c("linux", "windows"))
  skip_if_not_installed("withr")
  withr::local_options(digits = 3)
  expect_snapshot(fderiv(mod))
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
