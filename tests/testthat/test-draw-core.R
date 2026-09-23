# Small functional checks stay on CRAN; the full visual matrices are secondary.
# Build every panel: constructing a ggplot alone can hide rendering errors.
expect_draw_build <- function(plots) {
  expect_s3_class(plots, "patchwork")
  for (i in seq_len(length(plots))) {
    expect_silent(built <- ggplot2::ggplot_build(plots[[i]]))
    expect_gt(length(built$data), 0L)
  }
}

test_that("drawing preserves selection, transformations and argument errors", {
  for (select in list(2, "s(x1)", c(FALSE, TRUE, FALSE, FALSE))) {
    p <- draw(m_gam, select = select, n = 8, rug = FALSE)
    expect_length(p, 1L)
    expect_equal(unique(p[[1]]$data$.smooth), "s(x1)")
    expect_draw_build(p)
  }
  expect_draw_build(draw(m_1_smooth, n = 8, residuals = TRUE,
    constant = 1, fun = exp, rug = FALSE))
  expect_error(draw(m_gam, select = 8), "indices in 'select'")
  expect_error(draw(m_gam, select = c(1, 3, 5, 6)), "indices in 'select'")
  expect_error(draw(m_gam, select = 1:5), "Trying to select more smooths")
  expect_error(draw(m_gam, select = TRUE), "logical vector")
  expect_error(draw(su_m_factor_by, select = "s(x2)", partial_match = FALSE),
    "Failed to match any smooths")
  expect_draw_build(draw(su_m_factor_by, select = "s(x2)",
    partial_match = TRUE, n = 8, rug = FALSE))
})

test_that("drawing handles small smooth grids and derived objects", {
  expect_draw_build(draw(smooth_estimates(su_m_bivar_te, n_2d = 5)))
  expect_draw_build(draw(smooth_estimates(m_univar_t2, n = 8)))
  expect_draw_build(draw(derivatives(m_1_smooth, n = 8), add_change = TRUE))
  expect_draw_build(draw(partial_derivatives(su_m_bivar_te, focal = "x", n = 8)))
  sm <- smooth_estimates(m_1_smooth, n = 8)
  d <- derivatives(m_1_smooth, n = 8)
  expect_draw_build(draw(add_sizer(sm, derivatives = d, type = "sizer")))
})

test_that("ordered factor labels and grouped panels survive drawing", {
  withr::local_seed(901)
  d <- data.frame(x = rep(seq(0, 1, length.out = 20), 3),
    f = ordered(rep(letters[1:3], each = 20)))
  d$y <- sin(d$x * 4) + as.numeric(d$f) + rnorm(60, sd = 0.2)
  m <- gam(y ~ f + s(x, k = 4) + s(x, by = f, k = 4), data = d)
  sm <- smooth_estimates(m, n = 8)
  separate <- draw(sm)
  grouped <- draw(sm, grouped_by = TRUE)
  expect_length(separate, 3L)
  expect_length(grouped, 2L)
  expect_match(separate[[2]]$labels$subtitle, "f.*b")
  expect_match(separate[[3]]$labels$subtitle, "f.*c")
  expect_draw_build(grouped)
  expect_draw_build(draw(m, grouped_by = TRUE, n = 8, rug = FALSE))
})

test_that("parametric drawing retains values and validation", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_seed(902)
  d <- data.frame(x = runif(60), z = runif(60), f = factor(rep(1:3, 20)))
  d$y <- d$x + sin(4 * d$z) + rnorm(60)
  m <- gam(y ~ x + f + s(z, k = 4), data = d)
  e <- evaluate_parametric_term(m, term = "x")
  expect_s3_class(e, "evaluated_parametric_term")
  expect_named(e, c("term", "type", "value", "partial", "se"))
  expect_s3_class(evaluate_parametric_term(m, term = "f"), "evaluated_parametric_term")
  expect_error(evaluate_parametric_term(m, term = "z"), "not in the parametric part")
  expect_warning(evaluate_parametric_term(m, term = c("x", "f")), "More than one")
  expect_draw_build(draw(m, parametric = TRUE, data = d, n = 8, rug = FALSE))
})

test_that("small four dimensional smooths can be evaluated and drawn", {
  withr::local_seed(903)
  d <- data.frame(x = runif(100), z = runif(100), u = runif(100), v = runif(100))
  d$y <- sin(d$x * 4) + d$z + rnorm(100)
  m <- gam(y ~ te(x, z, u, v, k = c(3, 3, 3, 3)), data = d)
  sm <- smooth_estimates(m, n_2d = 4, n_3d = 3, n_4d = 2)
  expect_true(all(is.finite(sm$.estimate)))
  expect_draw_build(draw(sm))
})

test_that("small tensor random effects retain supported and unsupported paths", {
  withr::local_seed(904)
  d <- data.frame(x = runif(90), z = runif(90), f = factor(rep(1:3, 30)))
  d$y <- sin(d$x * 4) + rnorm(90)
  a <- gam(y ~ s(z, k = 4) + ti(x, f, bs = c("tp", "re"), k = c(4, 3)), data = d)
  b <- gam(y ~ s(z, k = 4) + ti(x, z, f,
    bs = c("tp", "tp", "re"), k = c(4, 4, 3)), data = d)
  expect_draw_build(draw(a, n = 8, rug = FALSE))
  expect_message(p <- draw(b, n = 8, rug = FALSE),
    "Can't yet plot multivariate smooths with a 're' marginal", fixed = TRUE)
  expect_length(p, 1L)
  expect_draw_build(p)
})

test_that("small AR and matrix covariate models can be drawn", {
  withr::local_seed(905)
  d <- data.frame(x = seq(0, 1, length.out = 60))
  d$y <- sin(d$x * 4) + rnorm(60)
  ar <- bam(y ~ s(x, k = 5), data = d, rho = 0.4)
  expect_draw_build(draw(ar, n = 8, rug = FALSE))
  d$X <- I(cbind(d$x, d$x + 0.1))
  d$L <- I(matrix(0.5, nrow(d), 2))
  m <- gam(y ~ s(X, by = L, k = 5), data = d)
  expect_draw_build(draw(m, n = 8, rug = FALSE))
})

test_that("small bivariate smooth differences preserve factor comparisons", {
  withr::local_seed(906)
  d <- data.frame(x = runif(80), z = runif(80), f = factor(rep(1:2, 40)))
  d$y <- sin(d$x * 4) + as.numeric(d$f) + rnorm(80)
  m <- gam(y ~ f + s(x, z, by = f, k = 8), data = d)
  ds <- difference_smooths(m, select = "s(x,z)", n_2d = 5)
  expect_equal(nrow(ds), 25L)
  expect_true(all(is.finite(ds$.diff)))
  expect_draw_build(draw(ds))
})

test_that("soap boundaries and clipping work on a small grid", {
  uncut <- smooth_estimates(m_soap, n_2d = 8, clip = FALSE)
  clipped <- smooth_estimates(m_soap, n_2d = 8, clip = TRUE)
  bnd <- boundary(get_smooth(m_soap, "s(v,w)"))
  n_boundary <- sum(vapply(bnd, function(x) length(x[[1]]), integer(1)))
  expect_equal(nrow(uncut), 8L * 8L + n_boundary)
  expect_lt(nrow(clipped), nrow(uncut))
  reference <- predict(m_soap, newdata = clipped, type = "terms")
  interior <- is.finite(clipped$.estimate)
  expect_true(any(interior))
  expect_equal(clipped$.estimate[interior],
    unname(reference[interior, 1L]), tolerance = 1e-7
  )
  expect_draw_build(draw(clipped))
})
