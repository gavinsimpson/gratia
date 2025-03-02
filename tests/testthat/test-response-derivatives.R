# uses df_pois & m_nb

# response derivatives
test_that("first order response derivatives works", {
  skip_on_cran()
  skip_on_ci() # testing without as moved to mac os x
  #skip_on_covr()
  skip_on_os(os = c("linux", "windows"))

  N <- 50L
  expect_silent(ds <- data_slice(m_nb,
    x2 = evenly(x2, n = N),
    data = df_pois, envir = teardown_env()
  ))
  expect_silent(yd <- response_derivatives(m_nb,
    data = ds,
    type = "central", focal = "x2", seed = 2
  ))
  expect_s3_class(yd, class = "response_derivatives")
  expect_identical(nrow(yd), N)
  expect_snapshot(print(yd), variant = "central")

  expect_silent(yd <- response_derivatives(m_nb,
    data = ds,
    type = "forward", focal = "x2", seed = 2
  ))
  expect_s3_class(yd, class = "response_derivatives")
  expect_identical(nrow(yd), N)
  expect_snapshot(print(yd), variant = "forward")

  expect_silent(yd <- response_derivatives(m_nb,
    data = ds,
    type = "backward", focal = "x2", seed = 2
  ))
  expect_s3_class(yd, class = "response_derivatives")
  expect_identical(nrow(yd), N)
  expect_snapshot(print(yd), variant = "backward")
})

test_that("second order response derivatives works", {
  skip_on_cran()
  skip_on_ci() # testing without as moved to mac os x
  skip_on_covr()
  skip_on_os(os = c("linux", "windows"))

  N <- 50L
  expect_silent(ds <- data_slice(m_nb,
    x2 = evenly(x2, n = N),
    data = df_pois, envir = teardown_env()
  ))
  expect_silent(yd <- response_derivatives(m_nb,
    data = ds,
    type = "central", focal = "x2", seed = 2, order = 2
  ))
  expect_s3_class(yd, class = "response_derivatives")
  expect_identical(nrow(yd), N)
  expect_snapshot(print(yd), variant = "central")

  expect_silent(yd <- response_derivatives(m_nb,
    data = ds,
    type = "forward", focal = "x2", seed = 2, order = 2
  ))
  expect_s3_class(yd, class = "response_derivatives")
  expect_identical(nrow(yd), N)
  expect_snapshot(print(yd), variant = "forward")

  expect_silent(yd <- response_derivatives(m_nb,
    data = ds,
    type = "backward", focal = "x2", seed = 2, order = 2
  ))
  expect_s3_class(yd, class = "response_derivatives")
  expect_identical(nrow(yd), N)
  expect_snapshot(print(yd), variant = "backward")
})
