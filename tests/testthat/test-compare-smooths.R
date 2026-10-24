## Test compare_smooths()

# uses
# * su_m_quick_eg1, and
# * su_m_quick_eg1_shrink
# from setup.R

test_that("compare_smooths() works for a pair of GAMs", {
  expect_silent(cs <- compare_smooths(
    su_m_quick_eg1,
    su_m_quick_eg1_shrink
  ))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models
})

test_that("compare_smooths() errors when passed a single model", {
  expect_error(
    compare_smooths(su_m_quick_eg1),
    "Need at least two models to compare smooths"
  )
})

test_that("draw.compare_smooths() can plot a comparison of smooths", {
  expect_silent(cs <- compare_smooths(
    su_m_quick_eg1,
    su_m_quick_eg1_shrink
  ))
  expect_silent(plt1 <- draw(cs))

  expect_silent(plt2 <- draw(cs, nrow = 2, ncol = 3))
  skip_on_cran()
  # this `&` operation is causing an warning with ggplot 4 & patchwork 1.3.1
  expect_silent(plt3 <- draw(cs) & theme(legend.position = "bottom"))
  expect_doppelganger("compare smooths - all smooths", plt1)
  expect_doppelganger("compare smooths - set nrow ncol", plt2)
  expect_doppelganger("compare smooths - bottom legend", plt3)
})

test_that("compare_smooths() works when a model is not a simple object", {
  expect_silent(cs <- compare_smooths(m_gam, m_gamm$gam))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models

  expect_silent(cs <- compare_smooths(m_gam, m_gamm[["gam"]]))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models
})

test_that("compare_smooths() works when a model is not a simple object", {
  l1 <- list(model1 = m_gam)
  l2 <- list(model2 = m_gamm$gam)

  expect_silent(cs <- compare_smooths(l1[[1]], l2[[1]]))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models

  expect_silent(cs <- compare_smooths(m_gam, l2[["model2"]]))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models

  expect_silent(cs <- compare_smooths(m_gam, l2$model2))
  expect_named(cs, expected = c(".model", ".smooth", ".type", ".by", "data"))
  expect_s3_class(cs, c("tbl_df", "tbl", "data.frame"))
  expect_identical(nrow(cs), 8L) # 4 smooths x 2 models
})

# Use the existing surface fits, which have different fitting rows and bases.
surface_comparison <- function(...) {
  compare_smooths(su_m_bivar, Duchon = su_m_bivar_ds, n_2d = 5, ...)
}

test_that("surface comparisons use both axes and fixed facets in model order", {
  cs <- surface_comparison()
  expect_named(cs, c(".model", ".smooth", ".type", ".by", "data"))
  expect_equal(cs$data[[1]][c("x", "z")], cs$data[[2]][c("x", "z")])
  p <- draw(cs, contour = FALSE)[[1]]
  expect_equal(rlang::eval_tidy(p$mapping$x, p$data), p$data$x)
  expect_equal(rlang::eval_tidy(p$mapping$y, p$data), p$data$z)
  expect_equal(rlang::eval_tidy(p$layers[[1]]$mapping$fill, p$data), p$data$.estimate)
  b <- ggplot_build(p)
  expect_equal(as.character(b$layout$layout$.model), c("su_m_bivar", "Duchon"))
  expect_equal(b$layout$layout$COL, 1:2)
  expect_equal(b$layout$layout$SCALE_X, c(1L, 1L))
  expect_equal(b$layout$layout$SCALE_Y, c(1L, 1L))
  expect_length(b$plot$scales$get_scales("fill")$range$range, 2L)
  expect_equal(b$plot$scales$get_scales("fill")$range$range,
    c(-1, 1) * max(abs(p$data$.estimate)))
  expect_equal(p$labels$x, "x")
  expect_equal(p$labels$y, "z")
  expect_null(p$labels$caption)
})

test_that("surface differences match independent predictions, irrespective of row order", {
  cs <- surface_comparison()
  nd <- cs$data[[1]][c("x", "z")]
  expected <- as.numeric(predict(su_m_bivar, nd, type = "terms") -
    predict(su_m_bivar_ds, nd, type = "terms"))
  cs$data[[2]] <- cs$data[[2]][rev(seq_len(nrow(nd))), ]
  p <- draw(cs, show = "difference", contour = FALSE)[[1]]
  expect_equal(p$data$.diff, unname(expected))
  expect_equal(p$data[c("x", "z")], nd)
  expect_named(p$data, c("x", "z", ".diff"))
  expect_equal(p$labels$fill, "su_m_bivar - Duchon")
  b <- ggplot_build(p)
  expect_equal(b$plot$scales$get_scales("fill")$range$range,
    c(-1, 1) * max(abs(expected)))
  reverse <- draw(cs, show = "difference", pair = c("Duchon", "su_m_bivar"),
    contour = FALSE)[[1]]$data
  reverse <- dplyr::left_join(nd, reverse, by = c("x", "z"))
  expect_equal(reverse$.diff, -unname(expected))
  same <- compare_smooths(su_m_bivar, Same = su_m_bivar, n_2d = 5)
  expect_equal(draw(same, show = "difference", contour = FALSE)[[1]]$data$.diff,
    rep(0, 25))
})

test_that("surface comparisons retain supplied locations and missing regions", {
  cs <- surface_comparison()
  nd <- cs$data[[1]][c("x", "z")][-13, ]
  supplied <- surface_comparison(data = nd)
  expect_equal(supplied$data[[1]][c("x", "z")], nd)
  expect_equal(supplied$data[[2]][c("x", "z")], nd)
  # A missing value and an absent row both remain missing after matching.
  supplied$data[[1]]$.estimate[3] <- NA_real_
  supplied$data[[2]] <- supplied$data[[2]][-7, ]
  p <- draw(supplied, show = "difference", contour = FALSE)[[1]]
  expect_equal(nrow(p$data), nrow(nd))
  expect_equal(which(is.na(p$data$.diff)), c(3L, 7L))
  expect_false(any(p$data$x == cs$data[[1]]$x[13] &
    p$data$z == cs$data[[1]]$z[13]))
  masked <- surface_comparison(dist = 0.05)
  p <- draw(masked, show = "difference", contour = FALSE)[[1]]
  expect_equal(is.na(p$data$.diff),
    is.na(masked$data[[1]]$.estimate) | is.na(masked$data[[2]]$.estimate))
  expect_true(anyNA(p$data$.diff))
  # An irregular supplied grid uses bounded rectangles, preserving gaps.
  nd$x[nd$x == unique(nd$x)[2]] <- mean(unique(nd$x)[1:2])
  p <- draw(surface_comparison(data = nd), contour = FALSE)[[1]]
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
})

test_that("more than two models require an explicit difference pair", {
  cs <- compare_smooths(su_m_bivar, Duchon = su_m_bivar_ds,
    Third = su_m_bivar, n_2d = 5)
  expect_equal(as.character(ggplot_build(draw(cs, contour = FALSE)[[1]])$
    layout$layout$.model), c("su_m_bivar", "Duchon", "Third"))
  expect_error(draw(cs, show = "difference"), "supply `pair`")
  p <- draw(cs, show = "difference", pair = c(3, 1), contour = FALSE)[[1]]
  expect_equal(p$data$.diff, rep(0, 25))
  expect_equal(p$labels$fill, "Third - su_m_bivar")
  for (pair in list(c(1, 1), c(1, 4), c(1.5, 2), c("absent", "Duchon"))) {
    expect_error(draw(cs, show = "difference", pair = pair), "two distinct")
  }
})

test_that("selection matches term names across models and mixed dimensions draw correctly", {
  fits <- comparison_mixed_fixture()
  reversed <- compare_smooths(fits$second, Other = fits$first, n = 6, n_2d = 5)
  expect_identical(unique(reversed$.smooth), sort(smooths(fits$second)))
  cs <- compare_smooths(fits$first, Second = fits$second, n = 6, n_2d = 5)
  p <- draw(cs, contour = FALSE)
  expect_length(p, 2L)
  expect_equal(p[[1]]$labels$y, "x1")
  expect_equal(p[[2]]$labels$y, "Partial effect")
  expect_s3_class(p[[2]]$layers[[1]]$geom, "GeomRibbon")
  expect_equal(unique(draw(cs, select = 2)[[1]]$data$.smooth), "s(x2)")
  expect_equal(draw(cs, select = "s(x0", partial_match = TRUE)[[1]]$labels$y, "x1")
  expect_equal(compare_smooths(fits$first, Second = fits$second, select = 1,
    n_2d = 5)$.smooth, rep("s(x0,x1)", 2))
  expect_error(draw(cs, show = "difference"), "requires two-variable")
  expect_silent(draw(cs, select = 1, show = "difference", contour = FALSE))
  narrow <- draw(cs, select = 2, ci_level = 0.5)[[1]]$data
  wide <- draw(cs, select = 2, ci_level = 0.99)[[1]]$data
  expect_true(all(wide$.upper_ci - wide$.lower_ci > narrow$.upper_ci - narrow$.lower_ci))
})

test_that("differences match conditioning values as well as both axes", {
  cs <- surface_comparison()
  # Represent supplied estimates conditional on a factor and a numeric by value.
  for (i in seq_along(cs$data)) {
    d <- cs$data[[i]]
    d$condition <- factor("a", levels = c("a", "b"))
    d$multiplier <- 1
    e <- d
    e$condition[] <- "b"
    e$multiplier <- 2
    e$.estimate <- e$.estimate * 2
    cs$data[[i]] <- dplyr::bind_rows(d, e)
  }
  expected <- cs$data[[1]]$.estimate - cs$data[[2]]$.estimate
  cs$data[[2]] <- cs$data[[2]][50:1, ]
  p <- draw(cs, show = "difference", contour = FALSE)[[1]]
  expect_equal(p$data$.diff, expected)
  expect_true(all(c("condition", "multiplier") %in% names(p$data)))
  expect_equal(nrow(ggplot_build(p)$layout$layout), 2L)
  cs$data[[2]]$x <- cs$data[[2]]$x + 10
  expect_error(draw(cs, show = "difference"), "no overlapping")
})

test_that("incompatible comparisons and unsupported dimensions fail clearly", {
  expect_error(compare_smooths(su_m_bivar, su_m_quick_eg1), "must occur exactly once")
  cs <- surface_comparison()
  cs$data[[2]] <- cs$data[[2]][c(1, seq_len(25)), ]
  expect_error(draw(cs, show = "difference"), "locations must be unique")
  cs <- surface_comparison()
  cs$data[[2]]$z <- NULL
  expect_error(draw(cs, show = "difference"), "covariates must match")
  cs <- compare_smooths(su_m_trivar, Same = su_m_trivar, n = 3, n_2d = 3)
  expect_error(draw(cs), "only one- or two-variable")
  # Change stored fitting coordinates to check range and type validation.
  other <- su_m_bivar_ds
  other$model$x <- other$model$x + 10
  expect_error(compare_smooths(su_m_bivar, other), "Nonoverlapping support")
  other$model$x <- factor(other$model$x)
  expect_error(compare_smooths(su_m_bivar, other), "Incompatible or unsupported")
})

test_that("factor-by surfaces retain the selected level and supplied rows", {
  m <- conditional_surface_fixture()
  cs <- compare_smooths(m, Same = m, select = 2, n_2d = 5)
  expect_equal(cs$.smooth, rep("s(x,z):aB", 2))
  expect_equal(as.character(cs$data[[1]]$a), rep("B", 25))
  nd <- cs$data[[1]][c("x", "z", "a")][c(5, 1, 12, 20), ]
  cs <- compare_smooths(m, Same = m, select = 2, data = nd)
  expect_equal(cs$data[[1]][c("x", "z", "a")], nd)
  expect_equal(draw(cs, show = "difference", contour = FALSE)[[1]]$data$.diff,
    rep(0, nrow(nd)))
})

test_that("transformed surface coordinates retain their names and supplied locations", {
  m <- comparison_transformed_fixture()
  nd <- quick_eg1[1:8, c("x0", "x1")]
  cs <- compare_smooths(m, Same = m, data = nd)
  p <- draw(cs, contour = FALSE)[[1]]
  expect_equal(p$labels$x, "log(x0 + 1)")
  expect_equal(p$data[["log(x0 + 1)"]], rep(log(nd$x0 + 1), 2))
  expect_equal(p$data$x1, rep(nd$x1, 2))
  expect_equal(draw(cs, show = "difference", contour = FALSE)[[1]]$data$.diff, rep(0, 8))
})

test_that("soap boundaries are not matched as surface observations", {
  cs <- compare_smooths(m_soap, Same = m_soap, n_2d = 10)
  p <- draw(cs, show = "difference", contour = FALSE)[[1]]
  expect_true(anyNA(p$data$.diff))
  expect_true(all(p$data$.diff[is.finite(p$data$.diff)] == 0))
  expect_false(any(c(".bndry", ".loop") %in% names(p$data)))
  expect_silent(ggplot_build(p))
  p <- draw(cs, contour = FALSE)[[1]]
  expect_equal(nrow(ggplot_build(p)$layout$layout), 2L)
})

test_that("spherical comparisons reuse geographic plotting", {
  skip_if_not_installed("sf")
  cs <- compare_smooths(m_sos, Same = m_sos, n_2d = 5)
  p <- draw(cs, contour = FALSE)[[1]]
  expect_s3_class(p$coordinates, "CoordSf")
  expect_equal(nrow(ggplot_build(p)$layout$layout), 2L)
  p <- draw(cs, show = "difference", contour = FALSE)[[1]]
  expect_s3_class(p$coordinates, "CoordSf")
  expect_true(all(p$data$.diff == 0))
  expect_equal(p$labels$fill, "m_sos - Same")
  expect_null(p$labels$caption)
})
