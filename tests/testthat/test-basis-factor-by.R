factor_by_basis_data <- function() {
  withr::with_seed(428, data.frame(
    x = runif(160), z = runif(160), w = runif(160), v = runif(160),
    f = factor(rep(c("a", "b"), each = 80)), y = rnorm(160)
  ))
}

test_that("unweighted factor-by bases draw once in one to four dimensions", {
  d <- factor_by_basis_data()
  for (dimension in 1:4) {
    vars <- c("x", "z", "w", "v")[seq_len(dimension)]
    term <- sprintf("s(%s, by = f, k = 20)", paste(vars, collapse = ","))
    m <- mgcv::gam(stats::as.formula(paste("y ~ f +", term)), data = d)
    b <- basis(m, unweighted = TRUE, n = 5, n_2d = 3,
      n_3d = 2, n_4d = 2, variables = rev(vars))
    weighted <- basis(m, n = 5, n_2d = 3, n_3d = 2, n_4d = 2,
      variables = rev(vars))
    expect_identical(unique(b$.smooth), smooths(m))
    expect_identical(b[setdiff(names(b), ".value")],
      structure(weighted[setdiff(names(weighted), ".value")],
        factor_by_groups = attr(b, "factor_by_groups")))
    sm <- m$smooth[[1L]]
    grid <- b[b$.smooth == sm$label & b$.bf == "1", c(vars, "f")]
    grid$f <- factor(grid$f, levels = levels(d$f))
    X <- mgcv::PredictMat(sm, as.data.frame(grid))
    expect_equal(b$.value[b$.smooth == sm$label], as.vector(t(X)))
    expect_equal(weighted$.value[weighted$.smooth == sm$label],
      as.vector(t(sweep(X, 2, stats::coef(m)[sm$first.para:sm$last.para], "*"))))
    plots <- draw(b, contour = FALSE)
    n_plots <- if (dimension < 3L) 1L else nlevels(b$.bf)
    expect_length(plots, n_plots)
    expect_length(draw(weighted, contour = FALSE), 2L * n_plots)
    p <- plots[[1L]]
    expect_identical(p$labels$title, sub("a$", "", sm$label))
    expect_true(all(is.na(p$data$.by)))
    expect_false("f" %in% names(p$data))
    expect_equal(c(p$labels$x, if (dimension > 1L) p$labels$y), rev(vars)[1:min(2L, dimension)])
    expect_null(p$labels$caption)
    built <- ggplot2::ggplot_build(p)
    n_facets <- switch(dimension, 1L, nlevels(b$.bf), 2L, 4L)
    expect_equal(nrow(built$layout$layout), n_facets)
    if (dimension == 2L) {
      # Each surface panel contains only one value per evaluation coordinate.
      expect_true(".bf" %in% names(built$layout$layout))
      wp <- ggplot2::ggplot_build(draw(weighted, contour = FALSE)[[1L]])
      expect_equal(nrow(wp$layout$layout), nlevels(b$.bf))
      expect_equal(nrow(wp$data[[1L]]), 9L * nlevels(b$.bf))
    }
  }
})

test_that("standalone factor-by plots combine evaluations across levels", {
  d <- factor_by_basis_data()
  # Disjoint ranges ensure drawing cannot simply discard all but one level.
  d$x <- ifelse(d$f == "a", d$x / 4, 0.75 + d$x / 4)
  for (constraints in c(FALSE, TRUE)) {
    b <- basis(mgcv::s(x, by = f, k = 5), data = d,
      constraints = constraints)
    plots <- draw(b)
    expect_length(plots, 1L)
    p <- plots[[1L]]
    expect_setequal(p$data$x, d$x)
    expect_identical(p$labels$title, "s(x):f")
    sm <- mgcv::smoothCon(mgcv::s(x, by = f, k = 5), data = d,
      absorb.cons = constraints)[[1L]]
    grid <- d
    grid$f[] <- "a"
    values <- mgcv::PredictMat(sm, grid)
    expected <- data.frame(x = rep(grid$x, each = ncol(values)),
      bf = rep(seq_len(ncol(values)), nrow(grid)), value = as.vector(t(values)))
    actual <- data.frame(x = p$data$x, bf = as.integer(p$data$.bf),
      value = p$data$.value)
    expect_equal(actual[order(actual$x, actual$bf), ],
      expected[order(expected$x, expected$bf), ], ignore_attr = TRUE)
  }
  grid <- expand.grid(x = seq(0, 1, length.out = 4),
    z = seq(0, 1, length.out = 4), f = levels(d$f))
  b <- basis(mgcv::te(x, z, by = f, k = c(3, 3)), data = d, at = grid,
    coefficients = seq_len(9))
  p <- draw(b, contour = FALSE)[[1L]]
  expect_equal(nrow(p$data), 16L * 9L)
  expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 9L)
})

test_that("shared basis groups survive selection, subsetting and serialization", {
  d <- factor_by_basis_data()
  d$f <- ordered(d$f)
  m <- mgcv::gam(y ~ f + s(x, by = f, k = 5) + s(z, by = f, k = 5), data = d)
  b <- basis(m, unweighted = TRUE, n = 5)
  expect_length(draw(b), 2L)
  expect_identical(vapply(seq_along(draw(b)), function(i) draw(b)[[i]]$labels$title,
    character(1)), c("s(x):f", "s(z):f"))
  one <- b[b$.smooth == smooths(m)[1L] & b$.bf %in% c("1", "3"), ]
  expect_named(attr(one, "factor_by_groups"), smooths(m)[1L])
  p <- draw(one)[[1L]]
  expect_setequal(as.character(p$data$.bf), c("1", "3"))
  expect_identical(p$data$.value, one$.value)
  expect_null(attr(b[, c("x", "z")], "factor_by_groups"))
  selected <- basis(m, select = smooths(m)[2L], unweighted = TRUE, n = 5)
  expect_identical(draw(selected)[[1L]]$labels$title, "s(z):f")
  restored <- unserialize(serialize(b, NULL))
  expect_equal(draw(restored)[[1L]]$data, draw(b)[[1L]]$data)
  attr(b, "factor_by_groups") <- NULL
  expect_identical(draw(b)[[1L]]$labels$title, smooths(m)[1L])
})

test_that("unweighted basis options reach wrappers and configured workers", {
  for (model in list(m_gam, m_gamm, m_gamm4, m_scam)) {
    b <- basis(model, select = "s(x2)", unweighted = TRUE, n = 5)
    reference <- basis(model, select = "s(x2)", n = 5)
    expect_equal(b$x2, reference$x2)
    expect_false(isTRUE(all.equal(b$.value, reference$.value)))
  }
  expect_error(basis(m_gam, unweighted = NA), "unweighted")
  expect_error(basis(m_gam, unweighted = 1), "unweighted")
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  d <- factor_by_basis_data()
  m <- mgcv::gam(y ~ f + te(w, x, z, d = c(1, 2), k = c(3, 5), by = f), data = d)
  calculate <- function() basis(m, unweighted = TRUE, n_2d = 3, n_3d = 2,
    variables = c("z", "x", "w"))
  reference <- calculate()
  expect_length(draw(reference), nlevels(reference$.bf))
  gratia_test_workers(1L)
  expect_equal(calculate(), reference, tolerance = 1e-12)
})
