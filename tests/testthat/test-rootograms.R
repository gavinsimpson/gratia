## Test rootograms() and related residuals functions

test_that("rootogram works for a continuous Gaussian response", {
  skip_on_cran()
  expect_silent(rg <- rootogram(m_gam))

  skip_on_cran()
  expect_doppelganger("draw gaussian rootogram", draw(rg))
})

test_that("rootogram works for a discrete Poisson response", {
  expect_silent(rg <- rootogram(b_pois))

  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw poisson rootogram", draw(rg))
})

test_that("rootogram works for a discrete negative binomial response", {
  skip_on_cran()
  expect_silent(rg <- rootogram(m_nb))

  skip_on_cran()
  expect_doppelganger("draw neg bin rootogram", draw(rg))
  expect_doppelganger("draw neg bin sqrt rootogram", draw(rg, sqrt = TRUE))
  expect_doppelganger("draw neg bin suspended rootogram",
    draw(rg, type = "suspended"))
  expect_doppelganger("draw neg bin standing rootogram",
    draw(rg, type = "standing"))
})

test_that("rootogram fails for a a non-supported response", {
  skip_on_cran()
  expect_error(rootogram(m_tw),
    "Only <Poisson, Negative Binomial, Gaussian> models supported.",
    fixed = TRUE
  )
})
