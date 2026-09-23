# Regression coverage for independently controlled surface grids (#153).
test_that("surface resolution is independent of curve resolution", {
  for (m in list(su_m_bivar, su_m_bivar_te, su_m_bivar_t2)) {
    expect_equal(nrow(smooth_estimates(m)), 2500L)
    sm <- smooth_estimates(m, n = 11, n_2d = 7)
    expect_equal(nrow(sm), 49L)
    expect_equal(length(unique(sm$x)), 7L)
    expect_equal(length(unique(sm$z)), 7L)
    expect_equal(nrow(smooth_estimates(m, n = 11, n_2d = NULL)), 121L)
    expect_equal(nrow(smooth_data(m, id = 1, n = 11)), 121L)
    d <- smooth_data(m, id = 1, n = 3)
    expect_equal(smooth_estimates(m, data = d, n_2d = 7),
      smooth_estimates(m, data = d, n_2d = 9))
  }
  expect_equal(nrow(smooth_estimates(m_1_smooth, n = 11, n_2d = 7)), 11L)
})

test_that("surface panels retain independent slice controls and tensor order", {
  for (m in list(su_m_trivar, su_m_trivar_te, su_m_trivar_t2)) {
    sm <- smooth_estimates(m, n = 11, n_2d = 7, n_3d = 3)
    expect_equal(nrow(sm), 7L * 7L * 3L)
    expect_equal(vapply(sm[c("x0", "x1", "x2")],
      function(x) length(unique(x)), integer(1)), c(x0 = 7L, x1 = 7L, x2 = 3L))
  }
  m <- gam(y ~ te(x0, x1, x2, x3, k = c(3, 3, 3, 3)), data = su_eg1)
  expect_equal(nrow(smooth_estimates(m, n = 11, n_2d = 7, n_4d = 2)),
    7L * 7L * 2L * 2L)
  m <- gam(y ~ te(x0, x1, x2, d = c(1, 2), k = c(3, 10)), data = su_eg1)
  sm <- smooth_estimates(m, n = 11, n_2d = 7, n_3d = 3)
  expect_equal(vapply(sm[c("x0", "x1", "x2")],
    function(x) length(unique(x)), integer(1)), c(x0 = 3L, x1 = 7L, x2 = 7L))
})

test_that("factor smooth curves and factor-by surfaces retain levels", {
  m <- gam(y ~ fac + s(x0, x1, by = fac, k = 10), data = su_eg4)
  sm <- smooth_estimates(m, n = 11, n_2d = 7)
  expect_equal(nrow(sm), 49L * nlevels(su_eg4$fac))
  expect_equal(levels(sm$fac), levels(su_eg4$fac))
  dif <- difference_smooths(m, select = "s(x0,x1)", n = 11, n_2d = 7)
  expect_equal(nrow(dif), 49L * choose(nlevels(su_eg4$fac), 2))
  expect_equal(nrow(difference_smooths(m, select = "s(x0,x1)")),
    2500L * choose(nlevels(su_eg4$fac), 2))
  for (bs in c("fs", "sz")) {
    m <- gam(y ~ s(x0, fac, bs = bs, k = 4), data = su_eg4)
    sm <- smooth_estimates(m, n = 11, n_2d = 7)
    expect_equal(nrow(sm), 11L * nlevels(su_eg4$fac))
    expect_equal(levels(sm$fac), levels(su_eg4$fac))
  }
})

test_that("special surfaces honour n_2d", {
  for (m in list(m_sos, m_soap)) {
    sm <- smooth_estimates(m, n = 11, n_2d = 7, clip = FALSE)
    n_boundary <- if (identical(m, m_soap)) {
      sum(vapply(boundary(m$smooth[[1]]), function(x) length(x[[1]]), integer(1)))
    } else 0L
    expect_equal(nrow(sm), 49L + n_boundary)
    expect_true(any(is.finite(sm$.estimate)))
  }
})

test_that("plotting, model wrappers, bases and comparisons forward n_2d", {
  m <- su_m_bivar
  for (obj in list(m, structure(list(gam = m), class = "gamm"))) {
    expect_equal(nrow(smooth_estimates(obj, n_2d = 7)), 49L)
    p <- draw(obj, n = 11, n_2d = 7, rug = FALSE, contour = FALSE, wrap = FALSE)
    expect_equal(nrow(p[[1]]$data), 49L)
    expect_equal(nrow(ggplot2::ggplot_build(p[[1]])$data[[1]]), 49L)
    bf <- basis(obj, n = 11, n_2d = 7)
    expect_equal(nrow(unique(bf[c("x", "z")])), 49L)
  }
  expect_equal(nrow(assemble(m, rug = FALSE)[[1]]$data), 2500L)
  expect_equal(nrow(draw(m, rug = FALSE, wrap = FALSE)[[1]]$data), 2500L)
  expect_equal(nrow(unique(basis(m)[c("x", "z")])), 2500L)
  cmp <- compare_smooths(m, m, n = 11, n_2d = 7)
  expect_true(all(vapply(cmp$data, nrow, integer(1)) == 49L))
})

test_that("parallel surface evaluation and bases agree with sequential results", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  m <- su_m_bivar_te
  sm <- smooth_estimates(m, n = 11, n_2d = 7)
  bf <- basis(m, n = 11, n_2d = 7)
  withr::defer(mirai::daemons(0))
  mirai::daemons(1, dispatcher = FALSE)
  expect_equal(smooth_estimates(m, n = 11, n_2d = 7), sm)
  expect_equal(basis(m, n = 11, n_2d = 7), bf)
})
