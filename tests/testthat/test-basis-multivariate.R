# Small cached fits exercise basis evaluation and plotting on the same terms.
multivariate_basis_model <- local({
  models <- new.env(parent = emptyenv())
  function(dimension = 3L, type = "tp", by = FALSE) {
    key <- paste(dimension, type, by)
    if (!exists(key, envir = models, inherits = FALSE)) {
      d <- withr::with_seed(258, {
        data.frame(x = runif(180), z = runif(180, 10, 20),
          w = runif(180), v = runif(180),
          f = factor(rep(c("a", "b"), each = 90)), y = rnorm(180))
      })
      by_arg <- if (by) ", by = f" else ""
      term <- if (type == "tp") {
        sprintf("s(%s, k = 20%s)",
          paste(c("x", "z", "w", "v")[seq_len(dimension)], collapse = ","),
          by_arg)
      } else if (dimension == 3L) {
        sprintf("%s(w,x,z,d=c(1,2),k=c(3,5)%s)", type, by_arg)
      } else {
        sprintf("%s(w,v,x,z,d=c(1,1,2),k=c(3,3,5)%s)", type, by_arg)
      }
      model <- mgcv::gam(stats::as.formula(paste("y ~", term)), data = d)
      assign(key, model, envir = models)
    }
    get(key, envir = models, inherits = FALSE)
  }
})

basis_grid_counts <- function(b, variables) {
  unname(vapply(b[variables], function(x) length(unique(x)), integer(1)))
}

test_that("basis covariate orders control 3D and 4D grids and values", {
  for (dimension in 3:4) for (type in c("tp", "te", "t2")) {
    model <- multivariate_basis_model(dimension, type)
    vars <- c("w", "z", "x", "v")[seq_len(dimension)]
    b <- basis(model, variables = vars, n = 7, n_2d = 3,
      n_3d = 2, n_4d = 2)
    label <- model$smooth[[1]]$label
    expect_identical(attr(b, "plot_variables")[[label]], vars)
    expect_identical(basis_grid_counts(b, vars),
      c(3L, 3L, rep(2L, dimension - 2L)))
    grid <- as.data.frame(b[b$.bf == levels(b$.bf)[1L], vars])
    sm <- model$smooth[[1L]]
    X <- mgcv::PredictMat(sm, grid)
    weighted <- sweep(X, 2, stats::coef(model)[sm$first.para:sm$last.para], "*")
    expect_equal(b$.value, as.vector(t(weighted)), tolerance = 1e-12)
    # Explicit data preserve even duplicate and shuffled evaluation rows.
    grid <- grid[c(3, 1, 3, 2), , drop = FALSE]
    explicit <- basis(model, data = grid, variables = vars,
      n_2d = 9, n_3d = 9, n_4d = 9)
    expect_equal(as.data.frame(explicit[explicit$.bf == levels(b$.bf)[1L], vars]),
      grid, ignore_attr = TRUE)
  }
})

test_that("basis tensor defaults and NULL resolutions follow smooth conventions", {
  for (dimension in 3:4) for (type in c("te", "t2")) {
    model <- multivariate_basis_model(dimension, type)
    vars <- c("x", "z", "w", "v")[seq_len(dimension)]
    b <- basis(model, n = 3, n_2d = 2, n_3d = NULL, n_4d = NULL)
    expect_identical(attr(b, "plot_variables")[[model$smooth[[1L]]$label]], vars)
    expect_identical(basis_grid_counts(b, vars),
      c(2L, 2L, rep(3L, dimension - 2L)))
    b <- basis(model, n = 3, n_2d = NULL, n_3d = 2, n_4d = 2)
    expect_identical(basis_grid_counts(b, vars),
      c(3L, 3L, rep(2L, dimension - 2L)))
  }
})

test_that("basis surfaces separate basis functions and facet conditioning values", {
  for (dimension in 3:4) for (type in c("tp", "te", "t2")) {
    vars <- c("w", "z", "x", "v")[seq_len(dimension)]
    b <- basis(multivariate_basis_model(dimension, type), variables = vars,
      n_2d = 3, n_3d = 2, n_4d = 2)
    plots <- draw(b, legend = TRUE, ncol = 2)
    expect_s3_class(plots, "patchwork")
    expect_length(plots, nlevels(b$.bf))
    expect_equal(plots$patches$layout$ncol, 2)
    expect_equal(vapply(seq_along(plots), function(i) {
      unique(as.character(plots[[i]]$data$.bf))
    }, character(1)), levels(b$.bf))
    for (i in unique(c(1L, length(plots)))) {
      p <- plots[[i]]
      expect_equal(unique(as.character(p$data$.bf)), levels(b$.bf)[i])
      expect_equal(p$labels$subtitle, paste("Basis function", levels(b$.bf)[i]))
      expect_equal(c(p$labels$x, p$labels$y), vars[1:2])
      expect_null(p$labels$caption)
      expect_equal(p$labels$fill, "value")
      built <- ggplot2::ggplot_build(p)
      expect_equal(built$plot$scales$get_scales("fill")$get_limits(), range(b$.value))
      layout <- built$layout$layout
      expect_equal(nrow(layout), 2^(dimension - 2L))
      expect_equal(sort(unique(as.numeric(as.character(layout[[vars[3L]]])))),
        sort(unique(b[[vars[3L]]])))
      if (dimension == 3L) {
        expect_s3_class(p$facet, "FacetWrap")
      } else {
        expect_s3_class(p$facet, "FacetGrid")
        expect_false(p$facet$params$as.table)
        expect_equal(length(unique(layout$ROW)), 2L)
        expect_equal(length(unique(layout$COL)), 2L)
        expect_equal(sort(unique(layout[[vars[4L]]])), sort(unique(b[[vars[4L]]])))
      }
      if (type == "tp") expect_equal(p$coordinates$ratio, 1)
    }
    expect_silent(ggplot2::ggplotGrob(plots[[1L]]))
    override <- rev(vars)
    p <- draw(b, variables = override)[[1L]]
    expect_equal(c(p$labels$x, p$labels$y), override[1:2])
    expect_identical(p$data$.value, b$.value[b$.bf == levels(b$.bf)[1L]])
  }
})

test_that("basis metadata survives subsetting and old objects remain drawable", {
  b <- basis(multivariate_basis_model(), n_2d = 3, n_3d = 2,
    variables = c("w", "z", "x"))
  subset <- b[b$.bf %in% c("1", "3"), ]
  expect_identical(attr(subset, "plot_variables"), attr(b, "plot_variables"))
  expect_equal(draw(subset)[[2L]]$labels$subtitle, "Basis function 3")
  expect_null(attr(b[, c("x", "z", "w")], "plot_variables"))
  attr(b, "plot_variables") <- NULL
  expect_equal(c(draw(b)[[1L]]$labels$x, draw(b)[[1L]]$labels$y), c("x", "z"))
})

test_that("standalone basis specifications store orders without changing at", {
  d <- multivariate_basis_model(4L)$model
  for (dimension in 3:4) {
    spec <- if (dimension == 3L) mgcv::te(w, x, z, d = c(1, 2), k = c(3, 5)) else
      mgcv::te(w, v, x, z, d = c(1, 1, 2), k = c(3, 3, 5))
    grid <- expand.grid(x = c(.2, .8), z = c(12, 18), w = c(.3, .7), v = c(.1, .9))
    vars <- c("x", "z", "w", "v")[seq_len(dimension)]
    grid <- unique(grid[vars])
    b <- basis(spec, data = d, at = grid)
    label <- unique(b$.smooth)
    expect_identical(attr(b, "plot_variables")[[label]], vars)
    sm <- mgcv::smoothCon(spec, data = d)[[1L]]
    expect_equal(b$.value, as.vector(t(mgcv::PredictMat(sm, grid))))
    coefficients <- seq_len(nlevels(b$.bf)) / 10
    weighted <- basis(spec, data = d, at = grid, coefficients = coefficients,
      variables = rev(vars))
    expect_equal(weighted$.value,
      as.vector(t(sweep(mgcv::PredictMat(sm, grid), 2, coefficients, "*"))))
    expect_identical(attr(weighted, "plot_variables")[[label]], rev(vars))
  }
})

test_that("basis variable validation matches other smooth methods", {
  model <- multivariate_basis_model()
  b <- basis(model, n_2d = 3, n_3d = 2)
  for (vars in list("x", c("x", "x", "z"), c("x", "z", "bad"),
      c("x", "z", NA_character_), 1:3)) {
    expect_error(basis(model, variables = vars), "variables")
    expect_error(draw(b, variables = vars), "variables")
    expect_error(basis(mgcv::s(x, z, w, k = 20), data = model$model,
      variables = vars), "variables")
  }
  mixed <- mgcv::gam(y ~ s(x, k = 4) + s(z, k = 4), data = model$model)
  expect_error(basis(mixed, variables = "x"), "variables")
  expect_silent(basis(mixed, select = "s(x)", variables = "x"))
})

test_that("factor-by bases keep smooths and basis functions separate", {
  model <- multivariate_basis_model(3L, "te", by = TRUE)
  b <- basis(model, variables = c("z", "x", "w"), n_2d = 3, n_3d = 2)
  plots <- draw(b)
  labels <- unique(b$.smooth)
  expect_length(plots, sum(vapply(split(b$.bf, b$.smooth),
    function(x) length(unique(x)), integer(1))))
  for (i in seq_along(plots)) {
    p <- plots[[i]]
    expect_length(unique(p$data$.smooth), 1L)
    expect_length(unique(p$data$.bf), 1L)
    expect_length(unique(p$data$f), 1L)
    expect_equal(nrow(ggplot2::ggplot_build(p)$layout$layout), 2L)
  }
  first <- b[b$.smooth == labels[1L], ]
  expect_named(attr(first, "plot_variables"), labels[1L])
  expect_identical(plots[[1L]]$data$.smooth[1L], labels[1L])
  expect_identical(plots[[length(plots)]]$data$.smooth[1L], labels[2L])
})

test_that("conditioned basis plots retain labels, contours, legends and themes", {
  b <- basis(multivariate_basis_model(), n_2d = 6, n_3d = 2)
  b <- b[b$.bf == "1", ]
  p <- draw(b, title = "Custom", subtitle = "Subtitle", caption = "Caption",
    ylab = "Y", labeller = ggplot2::label_value, legend = TRUE,
    angle = 45, contour = TRUE, n_contour = 4)[[1L]]
  expect_equal(p$labels$title, "Custom")
  expect_equal(p$labels$subtitle, "Subtitle")
  expect_equal(p$labels$caption, "Caption")
  expect_equal(p$labels$y, "Y")
  expect_equal(p$guides$guides$x$params$angle, 45)
  expect_equal(p$layers[[2L]]$stat_params$bins, 4)
  expect_null(p$guides$guides$fill)
  expect_equal(draw(b)[[1L]]$guides$guides$fill, "none")
  expect_silent(ggplot2::ggplotGrob(p))
  themed <- draw(b) & ggplot2::theme(gratia.alpha.surface = 0.6)
  expect_s3_class(themed, "patchwork")
})

test_that("configured workers preserve multivariate basis grids and metadata", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  mirai::daemons(0)
  withr::defer(mirai::daemons(0))
  model <- multivariate_basis_model(4L, "te", by = TRUE)
  calculate <- function() basis(model, variables = c("z", "x", "w", "v"),
    n_2d = 3, n_4d = 2)
  reference <- calculate()
  gratia_test_workers(1L)
  expect_equal(calculate(), reference, tolerance = 1e-12)
})

test_that("basis wrappers forward the covariate order", {
  for (model in list(m_gam, m_gamm, m_gamm4, m_scam)) {
    b <- basis(model, select = "s(x2)", variables = "x2", n = 5)
    expect_identical(attr(b, "plot_variables")[[unique(b$.smooth)]], "x2")
    expect_equal(draw(b)[[1L]]$labels$x, "x2")
  }
})

test_that("factor conditioning and spherical axes follow existing restrictions", {
  d <- multivariate_basis_model()$model
  d$f <- ordered(rep(c("a", "b"), each = nrow(d) / 2))
  m <- mgcv::gam(y ~ te(x, z, w, f, d = c(2, 1, 1),
    bs = c("tp", "cr", "re"), k = c(5, 3, 3)), data = d)
  b <- basis(m, n_2d = 3, n_4d = 2)
  p <- draw(b)[[1L]]
  layout <- ggplot2::ggplot_build(p)$layout$layout
  expect_identical(unique(as.character(layout$f)), c("a", "b"))
  expect_equal(nrow(layout), 4L)
  expect_error(basis(m, variables = c("x", "z", "f", "w")), "Continuous")
  expect_error(draw(b, variables = c("x", "z", "f", "w")), "Continuous")
  m <- mgcv::gam(y ~ s(x, z, bs = "sos", k = 8), data = d)
  b <- basis(m, n_2d = 3)
  expect_error(basis(m, variables = c("z", "x")), "sphere")
  expect_error(draw(b, variables = c("z", "x")), "sphere")
  expect_error(basis(mgcv::s(x, z, bs = "sos", k = 8), data = d,
    variables = c("z", "x")), "sphere")
})

test_that("3D and 4D basis layouts render", {
  skip_on_cran()
  for (dimension in 3:4) {
    variables <- c("x", "w", "z", "v")[seq_len(dimension)]
    b <- basis(multivariate_basis_model(dimension), variables = variables, n_2d = 6,
      n_3d = 2, n_4d = 2)
    b <- b[b$.bf %in% c("1", "2"), ]
    expect_doppelganger(paste(dimension, "covariate basis surfaces"),
      draw(b, ncol = 2))
  }
})
