# -- Soap films ---------------------------------------------------------------
test_that("draw for gam can plot a so soap film", {
  #skip("mgcv can't find the boundary")

  expect_silent(plt_so <- draw(m_soap))

  skip_on_cran()
  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw.gam so soap film", plt_so)
})

test_that("smooth estimates can evaluate a so soap film", {
  #skip("mgcv can't find the boundary")
  expect_silent(sm_so <- smooth_estimates(m_soap, n = 100, clip = TRUE))
  bnd <- boundary(get_smooth(m_soap, "s(v,w)"))
  n_pts <- vapply(bnd, \(x) length(x[[1]]), integer(1))

  # check the nrow of the object, would be 10000 (100 * 100) *plus* the
  # boundary coords, which is 160 in the example, *if* all eval points were
  # inside the boundary, but some aren't: 8465 points including the boundary
  # are inside
  expect_identical(nrow(sm_so), as.integer(8627))

  skip_on_cran()
  # skip_on_ci() # testing without as moved to mac os x
  expect_snapshot(print(sm_so))

})

test_that("smooth estimates can evaluate a so soap film no clipping", {
  #skip("mgcv can't find the boundary")
  expect_silent(sm_so <- smooth_estimates(m_soap, n = 100, clip = FALSE))
  bnd <- boundary(get_smooth(m_soap, "s(v,w)"))
  n_pts <- vapply(bnd, \(x) length(x[[1]]), integer(1))

  # check the nrow of the object, should be 10000 (100 * 100) *plus* the
  # boundary coords, which is 160 in the example, *if* we are clipping
  expect_identical(nrow(sm_so), as.integer(100 * 100 + n_pts))

  skip_on_cran()
  # skip_on_ci() # testing without as moved to mac os x
  expect_snapshot(print(sm_so))

})

test_that("draw for smooth estimates can plot a so soap film", {
  #skip("mgcv can't find the boundary")
  expect_silent(plt_so <- smooth_estimates(m_soap, clip = TRUE) |> draw())

  skip_on_cran()
  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw.smooth_estimates so soap film", plt_so)
})

test_that("draw smooth estimates can plot a so soap film with known bndry", {
  #skip("mgcv can't find the boundary")
  expect_silent(
    plt_so <- smooth_estimates(m_soap_bndry, clip = TRUE) |>
      draw()
  )

  skip_on_cran()
  # skip_on_ci() # testing without as moved to mac os x
  expect_doppelganger("draw.smooth_estimates so soap film bndry", plt_so)
})

test_that(
  "smooth estimates can evaluate a nested so soap film",
  {
    expect_silent(
      sm_so <- smooth_estimates(m_soap_nested, n = 100, clip = TRUE)
    )
    bnd <- boundary(get_smooth(m_soap_nested, "s(x,y)"))
    n_pts <- vapply(bnd, \(x) length(x[[1]]), integer(1))

    # check the nrow of the object, should be 10000 (100 * 100) *plus* the
    # boundary coords, which is 100+100 in the example, *if* we aren't clipping
    # but we are so 7507 is what we get with the example data
    expect_identical(nrow(sm_so), as.integer(7507))

    skip_on_cran()
    # skip_on_ci() # testing without as moved to mac os x
    expect_snapshot(print(sm_so))
  }
)

test_that(
  "draw smooth estimates can plot a nested so soap film",
  {
    expect_silent(
      plt_so <- smooth_estimates(m_soap_nested, clip = TRUE) |>
        draw()
    )

    skip_on_cran()
    # skip_on_ci() # testing without as moved to mac os x
    expect_doppelganger("draw.smooth_estimates so nested soap film", plt_so)
  }
)
