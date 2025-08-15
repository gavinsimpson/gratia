test_that("rtw works with scalar p and phi", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot(
    withr::with_seed(
      seed = 34657,
      rtw(
        mu = runif(10, min = 0, max = 10),
        p = 1.5,
        phi = 1.1
      )
    )
  )
})

test_that("rtw works with vector p and phi", {
  skip_on_cran()
  skip_on_ci()

  expect_snapshot(
    withr::with_seed(
      seed = 34657,
      rtw(
        mu = runif(10, min = 0, max = 10),
        p = runif(10, min = 1, max = 2),
        phi = runif(10, 1, 2)
      )
    )
  )
})
