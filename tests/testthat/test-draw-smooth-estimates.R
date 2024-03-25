# Test draw.smooth_estimates()
# A lot of the current tests for this are over in test-draw-methods.R

test_that("draw.smooth_estimates works for m_1_smooth", {
  skip_on_cran()
  expect_silent(plt <- draw(smooth_estimates(m_1_smooth, "s(x0)")))

  skip_on_ci()
  expect_doppelganger("draw_smooth_estimates m_1_smooth", plt)
})

test_that("draw.smooth_estimates works for m_gam", {
  expect_silent(plt <- draw(smooth_estimates(m_gam, "s(x2)")))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates m_gam", plt)
})

test_that("draw.smooth_estimates works for m_gamm", {
  skip_on_ci() # Windows on GH seems very fragile
  expect_silent(plt <- draw(smooth_estimates(m_gamm, "s(x2)")))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates m_gamm", plt)
})

test_that("draw.smooth_estimates works for m_gamm4", {
  skip_on_ci()
  skip_on_cran()
  expect_silent(plt <- draw(smooth_estimates(m_gamm4, "s(x2)")))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates m_gamm4", plt)
})

test_that("draw.smooth_estimates works for m_bam", {
  expect_silent(plt <- draw(smooth_estimates(m_bam, "s(x2)")))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates m_bam", plt)
})

test_that("draw.smooth_estimates works for m_gaulss", {
  expect_silent(plt <- draw(smooth_estimates(m_gaulss, "s(x2)")))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates m_gaulss", plt)
})

test_that("draw.smooth_estimates works for m_scat", {
  expect_silent(plt <- draw(smooth_estimates(m_scat, "s(x2)")))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates m_scat", plt)
})

test_that("draw.smooth_estimates works for m_gamgcv", {
  expect_silent(plt <- draw(smooth_estimates(m_gamgcv, "s(x2)")))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates m_gamgcv", plt)
})

test_that("draw.smooth_estimates works for su_m_bivar", {
  skip_on_os(os = "mac") # trivial diffs in contours
  skip_on_os(os = "win") # trivial diffs in contours
  expect_silent(plt <- draw(smooth_estimates(su_m_bivar, dist = 0.1)))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_bivar", plt)
})

test_that("draw.smooth_estimates works for su_m_bivar_te", {
  skip_on_os(os = "win") # trivial diffs in contours
  expect_silent(plt <- draw(smooth_estimates(su_m_bivar_te, dist = 0.1)))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_bivar_te", plt)
})

test_that("draw.smooth_estimates works for su_m_bivar_t2", {
  expect_silent(plt <- draw(smooth_estimates(su_m_bivar_t2, dist = 0.1)))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_bivar_t2", plt)
})

test_that("draw.smooth_estimates works for su_m_trivar", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  expect_silent(plt <- draw(smooth_estimates(su_m_trivar,
    dist = 0.1,
    n = 25, n_3d = 4
  )))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_trivar", plt)
})

test_that("draw.smooth_estimates works for su_m_trivar_te", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  expect_silent(plt <- draw(smooth_estimates(su_m_trivar_te,
    dist = 0.1,
    n = 25, n_3d = 4
  )))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_trivar_te", plt)
})

test_that("draw.smooth_estimates works for su_m_trivar_t2", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  expect_silent(plt <- draw(smooth_estimates(su_m_trivar_t2,
    dist = 0.1,
    n = 25, n_3d = 4
  )))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_trivar_t2", plt)
})

test_that("draw.smooth_estimates works for su_m_quadvar", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  expect_silent(plt <- draw(smooth_estimates(su_m_quadvar,
    dist = 0.1,
    n = 25, n_3d = 4, n_4d = 3
  )))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_quadvar", plt)
})

test_that("draw.smooth_estimates works for su_m_quadvar_te", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  expect_silent(plt <- draw(smooth_estimates(su_m_quadvar_te,
    dist = 0.1,
    n = 25, n_3d = 4, n_4d = 3
  )))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_quadvar_te", plt)
})

test_that("draw.smooth_estimates works for su_m_quadvar_t2", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  expect_silent(plt <- draw(smooth_estimates(su_m_quadvar_t2,
    dist = 0.1,
    n = 25, n_3d = 4, n_4d = 3
  )))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_quadvar_t2", plt)
})

test_that("draw.smooth_estimates works for sz factor smooth", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  skip_if_not_installed("mgcv", minimum_version = "1.8.41")
  expect_silent(plt <- draw(smooth_estimates(m_sz)))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates sz factor smooth", plt)
})

test_that("draw.smooth_estimates works for sz two factor smooth", {
  skip_on_os(os = "win")
  skip_on_os(os = "mac")
  skip_if_not_installed("mgcv", minimum_version = "1.8.41")
  expect_silent(plt <- draw(smooth_estimates(m_sz_2f)))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates sz two factor smooth", plt)
})

test_that("draw.smooth_estimates works for trivar te with a 2d marginal", {
  skip_on_cran()
  skip_on_os(os = "win")
  skip_on_os(os = "mac")

  # only running this here so it doesn't add too much compute time for CRAN
  su_m_trivar_te2 <- gam(
    y ~ te(x0, x1, x2,
      k = c(3, 9), d = c(1, 2),
      bs = c("cr", "ds")
    ),
    data = su_eg1, method = "REML"
  )
  expect_silent(plt1 <- draw(smooth_estimates(su_m_trivar_te2,
    dist = 0.1,
    n = 20, n_3d = 4
  )))

  # also check that draw.gam() works for this model
  expect_silent(plt2 <- draw(su_m_trivar_te2, dist = 0.1, n = 20, n_3d = 5))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_trivar_te2", plt1)
  expect_doppelganger("draw.gam for su_m_trivar_te2", plt2)
})

test_that("draw.smooth_estimates works for quadvar te with 2d marginals", {
  skip_on_cran()
  skip_on_os(os = "win")
  skip_on_os(os = "mac")

  # only running this here so it doesn't add too much compute time for CRAN
  su_m_quadvar_te2 <- bam(
    y ~ te(x0, x1, x2, x3,
      k = c(3, 3, 3, 3),
      d = c(2, 2), bs = c("ds", "ds")
    ),
    data = su_eg1, method = "fREML", discrete = TRUE, nthreads = 2
  )

  expect_silent(plt1 <- draw(smooth_estimates(su_m_quadvar_te2,
    dist = 0.1,
    n = 20, n_3d = 4, n_4d = 3
  )))

  # also check that draw.gam() works for this model
  expect_silent(plt2 <- draw(su_m_quadvar_te2,
    dist = 0.1, n = 20, n_3d = 4,
    n_4d = 3
  ))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_quadvar_te2", plt1)
  expect_doppelganger("draw.gam for su_m_quadvar_te2", plt2)
})

test_that("draw.smooth_estimates works for trivar t2 with a 2d marginal", {
  skip_on_cran()
  skip_on_os(os = "win")
  skip_on_os(os = "mac")

  # only running this here so it doesn't add too much compute time for CRAN
  su_m_trivar_t22 <- gam(
    y ~ t2(x0, x1, x2,
      k = c(3, 9), d = c(1, 2),
      bs = c("cr", "ds")
    ),
    data = su_eg1, method = "REML"
  )
  expect_silent(plt1 <- draw(smooth_estimates(su_m_trivar_t22,
    dist = 0.1,
    n = 20, n_3d = 4
  )))
  # also check that draw.gam() works for this model
  expect_silent(plt2 <- draw(su_m_trivar_t22, dist = 0.1, n = 20, n_3d = 5))

  skip_on_ci()
  expect_doppelganger("draw.gam for su_m_trivar_t22", plt2)
  expect_doppelganger("draw.smooth_estimates su_m_trivar_t22", plt1)
})

test_that("draw.smooth_estimates works for quadvar t2 with 2d marginals", {
  skip_on_cran()
  skip_on_os(os = "win")
  skip_on_os(os = "mac")

  # only running this here so it doesn't add too much compute time for CRAN
  su_m_quadvar_t22 <- bam(
    y ~ t2(x0, x1, x2, x3,
      k = c(3, 3, 3, 3),
      d = c(2, 2), bs = c("ds", "ds")
    ),
    data = su_eg1, method = "fREML", discrete = TRUE, nthreads = 2
  )

  expect_silent(plt1 <- draw(smooth_estimates(su_m_quadvar_t22,
    dist = 0.1,
    n = 20, n_3d = 4, n_4d = 3
  )))

  # also check that draw.gam() works for this model
  expect_silent(plt2 <- draw(su_m_quadvar_t22,
    dist = 0.1, n = 20, n_3d = 4,
    n_4d = 3
  ))

  skip_on_ci()
  expect_doppelganger("draw.smooth_estimates su_m_quadvar_t22", plt1)
  expect_doppelganger("draw.gam for su_m_quadvar_t22", plt2)
})

test_that("draw.gam works with sos spline chlorophyll a", {
  skip_on_cran()
  skip_if_not_installed("gamair")
  data(chl, package = "gamair")
  m_chla <- bam(chl ~ s(lat, lon, bs = "sos"),
    data = chl, method = "fREML",
    discrete = TRUE
  )

  expect_silent(plt1 <- draw(m_chla, rug = FALSE, n = 25))
  expect_silent(plt2 <- draw(m_chla,
    rug = FALSE, n = 25,
    crs = "+proj=wintri"
  ))

  skip_on_ci()
  expect_doppelganger("draw.gam sos chlorophyll", plt1)
  expect_doppelganger("draw.gam sos chlorophyll with crs", plt2)
})

test_that("draw for smooth estimates works with univar tensor products #260", {
  expect_silent(plt_uni_te <- smooth_estimates(m_univar_te) |> draw())
  expect_silent(plt_uni_ti <- smooth_estimates(m_univar_ti) |> draw())
  expect_silent(plt_uni_t2 <- smooth_estimates(m_univar_t2) |> draw())

  skip_on_cran()

  expect_doppelganger("draw.smooth_estimates univariate te smooth", plt_uni_te)
  expect_doppelganger("draw.smooth_estimates univariate ti smooth", plt_uni_ti)
  expect_doppelganger("draw.smooth_estimates univariate t2 smooth", plt_uni_t2)
})

test_that("plot has correct label with ordered factor by models", {
  expect_silent(plt <- draw(m_ordered_by, rug = FALSE))

  skip_on_cran()
  expect_doppelganger("draw subtitle ordered by smooths", plt)
})

test_that("grouped_by works", {
  sm <- smooth_estimates(su_m_factor_by)
  expect_silent(plt1 <- draw(sm, grouped_by = TRUE))
  sm <- smooth_estimates(m_ordered_by)
  expect_silent(plt2 <- draw(sm, grouped_by = TRUE))

  skip_on_cran()
  expect_doppelganger("draw sm est grouped_by true", plt1)
  expect_doppelganger("draw sm est grouped_by true ordered", plt2)
})
