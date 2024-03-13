# -- Soap films ---------------------------------------------------------------
test_that("draw for gam can plot a so soap film", {
  #skip("mgcv can't find the boundary")

  expect_silent(plt_so <- draw(m_soap))

  skip_on_cran()
  skip_on_ci()
  expect_doppelganger("draw.gam so soap film", plt_so)
})

test_that("smooth estimates can evaluate a so soap film", {
  #skip("mgcv can't find the boundary")
  expect_silent(sm_so <- smooth_estimates(m_soap, n = 100))
  bnd <- boundary(get_smooth(m_soap, "s(v,w)"))
  n_pts <- vapply(bnd, \(x) length(x[[1]]), integer(1))

  # check the nrow of the object, should be 10000 (100 * 100) *plus* the
  # boundary coords, which is 160 in the example
  expect_identical(nrow(sm_so), as.integer((100 * 100) + n_pts[1L]))

  skip_on_cran()
  skip_on_ci()
  expect_snapshot(print(sm_so))
})

test_that("draw for smooth estimates can plot a so soap film", {
  #skip("mgcv can't find the boundary")
  expect_silent(plt_so <- smooth_estimates(m_soap) |> draw())

  skip_on_cran()
  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates so soap film", plt_so)
})
