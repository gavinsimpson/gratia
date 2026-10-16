# Small models are cached because the same fitted terms exercise estimation,
# saved estimates, and plotting below.
plot_variables_model <- local({
  models <- new.env(parent = emptyenv())
  function(dimension = 3L, basis = "tp", groups = 2L) {
    key <- paste(dimension, basis, groups, sep = "_")
    if (!exists(key, envir = models, inherits = FALSE)) {
      d <- withr::with_seed(8201, {
        n <- 300L
        data.frame(x = runif(n), z = runif(n, 10, 20),
          w = runif(n), v = runif(n),
          f = factor(rep(letters[seq_len(groups)], length.out = n)))
      })
      d$y <- sin(5 * d$x) + (as.integer(d$f) - 1) * d$w +
        cos(d$z) + withr::with_seed(17, rnorm(nrow(d), sd = 0.2))
      vars <- c("x", "z", "w", "v")[seq_len(dimension)]
      term <- if (basis == "tp") {
        sprintf("s(%s, by = f, k = 25)", paste(vars, collapse = ","))
      } else if (dimension == 3L) {
        sprintf("%s(w,x,z, by = f, d = c(1,2), k = c(3,5))", basis)
      } else {
        sprintf("%s(w,v,x,z, by = f, d = c(1,1,2), k = c(3,3,5))", basis)
      }
      model <- mgcv::gam(stats::as.formula(paste("y ~ f +", term)), data = d)
      assign(key, model, envir = models)
    }
    get(key, envir = models, inherits = FALSE)
  }
})

plot_variables_label <- function(model) {
  sub(":f[^:]+$", "", model$smooth[[1]]$label)
}

plot_variables_counts <- function(data, variables) {
  vapply(data[variables], function(x) length(unique(x)), integer(1L))
}

expect_plot_variables_mapping <- function(plot, variables) {
  expect_equal(rlang::as_label(plot$mapping$x), variables[1])
  expect_equal(rlang::as_label(plot$mapping$y), variables[2])
}

test_that("selected axes determine grid resolution for 3D and 4D smooths", {
  for (dimension in 3:4) for (basis in c("tp", "te", "t2")) {
    model <- plot_variables_model(dimension, basis)
    vars <- c("w", "z", "x", "v")[seq_len(dimension)]
    estimates <- smooth_estimates(model, variables = vars,
      n = 7, n_2d = 3, n_3d = 2, n_4d = 2, dist = 0)
    differences <- difference_smooths(model,
      select = plot_variables_label(model), variables = vars,
      n = 7, n_2d = 3, n_3d = 2, n_4d = 2)
    counts <- stats::setNames(c(3L, 3L, rep(2L, dimension - 2L)), vars)
    expect_identical(plot_variables_counts(estimates, vars), counts)
    expect_identical(plot_variables_counts(differences, vars), counts)
    expect_equal(nrow(differences), prod(counts))
    expect_identical(attr(differences, "plot_variables")[[unique(differences$.smooth)]], vars)
    for (label in unique(estimates$.smooth)) {
      expect_identical(attr(estimates, "plot_variables")[[label]], vars)
    }
  }
})

test_that("tensor defaults put the two-variable margin on the axes", {
  for (dimension in 3:4) for (basis in c("te", "t2")) {
    model <- plot_variables_model(dimension, basis)
    vars <- if (dimension == 3L) c("x", "z", "w") else c("x", "z", "w", "v")
    estimates <- smooth_estimates(model, n_2d = 3, n_3d = 2, n_4d = 2, dist = 0)
    differences <- difference_smooths(model, select = plot_variables_label(model),
      n_2d = 3, n_3d = 2, n_4d = 2)
    expect_identical(attr(estimates, "plot_variables")[[model$smooth[[1]]$label]], vars)
    expect_identical(attr(differences, "plot_variables")[[unique(differences$.smooth)]], vars)
    expect_identical(plot_variables_counts(differences, vars),
      stats::setNames(c(3L, 3L, rep(2L, dimension - 2L)), vars))
  }
})

test_that("NULL conditioning resolutions fall back to n", {
  for (dimension in 3:4) {
    model <- plot_variables_model(dimension)
    vars <- c("x", "z", "w", "v")[seq_len(dimension)]
    differences <- difference_smooths(model, select = plot_variables_label(model),
      n = 3, n_2d = 2, n_3d = NULL, n_4d = NULL)
    expect_identical(plot_variables_counts(differences, vars),
      stats::setNames(c(2L, 2L, rep(3L, dimension - 2L)), vars))
  }
})

test_that("differences agree with independent coefficient contrasts on shuffled grids", {
  for (dimension in 3:4) for (basis in c("tp", "te", "t2")) {
    model <- plot_variables_model(dimension, basis)
    grid <- if (dimension == 3L) {
      data_slice(model, x = c(0.2, 0.7), z = c(12, 18),
        w = c(0.3, 0.8), f = factor(levels(f), levels = levels(f)))
    } else {
      data_slice(model, x = c(0.2, 0.7), z = c(12, 18),
        w = c(0.3, 0.8), v = c(0.2, 0.6),
        f = factor(levels(f), levels = levels(f)))
    }
    grid <- withr::with_seed(92, grid[sample(nrow(grid)), , drop = FALSE])
    vars <- c("w", "z", "x", "v")[seq_len(dimension)]
    result <- difference_smooths(model, select = plot_variables_label(model),
      data = grid, variables = vars)
    first <- second <- as.data.frame(result[vars])
    first$f <- factor(result$.level_1, levels = levels(model$model$f))
    second$f <- factor(result$.level_2, levels = levels(model$model$f))
    X <- stats::predict(model, newdata = first, type = "lpmatrix") -
      stats::predict(model, newdata = second, type = "lpmatrix")
    keep <- unlist(lapply(model$smooth, function(sm) sm$first.para:sm$last.para))
    X <- X[, keep, drop = FALSE]
    estimate <- drop(X %*% stats::coef(model)[keep])
    se <- sqrt(rowSums((X %*% model$Vp[keep, keep, drop = FALSE]) * X))
    expect_equal(unname(result$.diff), unname(estimate), tolerance = 1e-8)
    expect_equal(unname(result$.se), unname(se), tolerance = 1e-8)
    expect_identical(sort(unique(result$w)), c(0.3, 0.8))
    expect_identical(sort(unique(result$x)), c(0.2, 0.7))
    if (dimension == 4L) expect_identical(sort(unique(result$v)), c(0.2, 0.6))
  }
})

test_that("3D differences facet the selected third variable", {
  for (basis in c("tp", "te", "t2")) {
    model <- plot_variables_model(3L, basis)
    differences <- difference_smooths(model, select = plot_variables_label(model),
      variables = c("w", "z", "x"), n_2d = 3, n_3d = 2)
    plot <- draw(differences)[[1]]
    expect_plot_variables_mapping(plot, c("w", "z", "x"))
    expect_s3_class(plot$facet, "FacetWrap")
    layout <- ggplot2::ggplot_build(plot)$layout$layout
    expect_equal(nrow(layout), 2L)
    expect_equal(sort(layout$x), sort(unique(differences$x)))
    expect_equal(ggplot2::ggplot_build(plot)$plot$scales$get_scales("fill")$get_limits(),
      c(-1, 1) * max(abs(differences$.diff)))
    expect_silent(ggplot2::ggplotGrob(plot))
  }
})

test_that("4D differences place the third and fourth variables in rows and columns", {
  for (basis in c("tp", "te", "t2")) {
    model <- plot_variables_model(4L, basis)
    differences <- difference_smooths(model, select = plot_variables_label(model),
      variables = c("w", "z", "x", "v"), n_2d = 3, n_4d = 2)
    plot <- draw(differences)[[1]]
    expect_plot_variables_mapping(plot, c("w", "z", "x", "v"))
    expect_s3_class(plot$facet, "FacetGrid")
    layout <- ggplot2::ggplot_build(plot)$layout$layout
    expect_equal(nrow(layout), 4L)
    expect_equal(as.integer(factor(layout$x)), layout$ROW)
    expect_equal(as.integer(factor(layout$v)), layout$COL)
    swapped <- draw(differences, variables = c("w", "z", "v", "x"))[[1]]
    swapped_layout <- ggplot2::ggplot_build(swapped)$layout$layout
    expect_equal(as.integer(factor(swapped_layout$v)), swapped_layout$ROW)
    expect_equal(as.integer(factor(swapped_layout$x)), swapped_layout$COL)
    expect_equal(nrow(swapped$data), nrow(plot$data))
    expect_silent(ggplot2::ggplotGrob(plot))
    estimates <- smooth_estimates(model, variables = c("w", "z", "x", "v"),
      n_2d = 3, n_4d = 2, dist = 0)
    smooth_plot <- draw(estimates)[[1]]
    expect_plot_variables_mapping(smooth_plot, c("w", "z", "x", "v"))
    smooth_layout <- ggplot2::ggplot_build(smooth_plot)$layout$layout
    expect_equal(as.integer(factor(smooth_layout$x)), smooth_layout$ROW)
    expect_equal(as.integer(factor(smooth_layout$v)), smooth_layout$COL)
  }
})

test_that("character groups in data slices retain the fitted factor order", {
  original <- plot_variables_model(3L)
  d <- original$model
  d$f <- factor(d$f, levels = c("b", "a"))
  model <- mgcv::gam(stats::formula(original), data = d)
  grid <- data_slice(model, x = c(0.2, 0.7), z = c(12, 18),
    w = c(0.3, 0.8), f = levels(f))
  expect_type(grid$f, "character")
  character_result <- difference_smooths(model,
    select = plot_variables_label(model), data = grid, variables = c("w", "x", "z"))
  grid$f <- factor(grid$f, levels = levels(d$f))
  factor_result <- difference_smooths(model,
    select = plot_variables_label(model), data = grid, variables = c("w", "x", "z"))
  expect_identical(character_result, factor_result)
  expect_identical(unique(character_result$.level_1), "b")
  expect_identical(unique(character_result$.level_2), "a")
})

test_that("saved variable choices survive redraws and explicit choices override them", {
  model <- plot_variables_model(3L, "te")
  estimates <- smooth_estimates(model, variables = c("w", "z", "x"),
    n_2d = 3, n_3d = 2, dist = 0)
  differences <- difference_smooths(model, select = plot_variables_label(model),
    variables = c("w", "z", "x"), n_2d = 3, n_3d = 2)
  for (object in list(estimates, differences)) {
    expect_plot_variables_mapping(draw(object)[[1]], c("w", "z", "x"))
    overridden <- draw(object, variables = c("x", "w", "z"))[[1]]
    expect_plot_variables_mapping(overridden, c("x", "w", "z"))
    expect_equal(nrow(overridden$data), nrow(draw(object)[[1]]$data))
  }
  attr(estimates, "plot_variables") <- NULL
  expect_plot_variables_mapping(draw(estimates)[[1]], c("x", "z", "w"))
  attr(differences, "plot_variables") <- NULL
  expect_plot_variables_mapping(draw(differences)[[1]], c("w", "x", "z"))
})

test_that("model drawing and assembly pass variable choices to estimation", {
  model <- plot_variables_model(3L)
  for (plots in list(
    draw(model, variables = c("w", "z", "x"), n_2d = 3, n_3d = 2,
      rug = FALSE, dist = 0, wrap = FALSE, parametric = FALSE),
    assemble(model, variables = c("w", "z", "x"), n_2d = 3, n_3d = 2,
      rug = FALSE, dist = 0, parametric = FALSE))) {
    expect_length(plots, 2L)
    for (plot in plots) {
      expect_plot_variables_mapping(plot, c("w", "z", "x"))
      expect_identical(plot_variables_counts(plot$data, c("w", "z", "x")),
        c(w = 3L, z = 3L, x = 2L))
    }
  }
})

test_that("fixed surface scales share differences without constraining covariate axes", {
  model <- plot_variables_model(3L, groups = 3L)
  differences <- difference_smooths(model, select = plot_variables_label(model),
    n_2d = 3, n_3d = 2)
  plots <- draw(differences, scales = "fixed")
  expect_length(plots, 3L)
  for (i in seq_along(plots)) {
    plot <- plots[[i]]
    expect_equal(ggplot2::ggplot_build(plot)$plot$scales$get_scales("fill")$get_limits(),
      c(-1, 1) * max(abs(differences$.diff)))
    built <- ggplot2::ggplot_build(plot)
    free <- ggplot2::ggplot_build(draw(differences, scales = "free")[[i]])
    expect_equal(built$layout$panel_scales_y[[1]]$get_limits(),
      free$layout$panel_scales_y[[1]]$get_limits())
    expect_gt(min(built$layout$panel_scales_y[[1]]$get_limits()), 0)
  }
})

test_that("comparison plots preserve distinct factor labels and pair order", {
  original <- plot_variables_model(3L, groups = 4L)
  d <- original$model
  levels(d$f) <- c("a.b", "a", "b.c", "c")
  model <- mgcv::gam(stats::formula(original), data = d)
  differences <- difference_smooths(model, select = plot_variables_label(model),
    n_2d = 3, n_3d = 2)
  plots <- draw(differences)
  pairs <- combn(levels(d$f), 2, simplify = FALSE)
  expect_length(plots, length(pairs))
  for (i in seq_along(pairs)) {
    expect_identical(unique(plots[[i]]$data$.level_1), pairs[[i]][1])
    expect_identical(unique(plots[[i]]$data$.level_2), pairs[[i]][2])
    expect_equal(nrow(plots[[i]]$data), 18L)
  }
})

test_that("saved differences validate factor conditioning roles", {
  d <- plot_variables_model(3L)$model
  d$condition <- factor(rep(c("low", "high"), each = nrow(d) / 2))
  model <- mgcv::gam(y ~ f + te(x, z, condition, by = f,
    bs = c("cr", "cr", "re"), k = c(3, 3, 3)), data = d)
  differences <- difference_smooths(model, select = "te(x,z,condition)",
    n_2d = 3, n_3d = 2)
  variables <- c("condition", "x", "z")
  expect_error(difference_smooths(model, select = "te(x,z,condition)",
    variables = variables), "Continuous covariates must precede factor")
  expect_error(draw(differences, variables = variables),
    "Continuous covariates must precede factor")
  expect_silent(ggplot2::ggplot_build(draw(differences,
    variables = c("z", "x", "condition"))[[1]]))
})

test_that("one and two-dimensional smooths retain their plotting behaviour", {
  for (dimension in 1:2) {
    model <- plot_variables_model(dimension)
    vars <- c("x", "z")[seq_len(dimension)]
    differences <- difference_smooths(model, select = plot_variables_label(model),
      n = 5, n_2d = 3)
    expect_equal(nrow(differences), if (dimension == 1L) 5 else 9)
    plot <- draw(differences)[[1]]
    expect_s3_class(plot$facet, "FacetNull")
    expect_equal(rlang::as_label(plot$mapping$x), "x")
    if (dimension == 2L) {
      swapped <- draw(differences, variables = rev(vars))[[1]]
      expect_plot_variables_mapping(swapped, rev(vars))
    } else {
      expect_equal(rlang::as_label(plot$mapping$y), ".diff")
      labelled <- draw(differences, xlab = "Covariate", caption = "Comparison")[[1]]
      expect_identical(labelled$labels$x, "Covariate")
      expect_identical(labelled$labels$caption, "Comparison")
    }
  }
})

test_that("invalid and incompatible variable selections fail clearly", {
  model <- plot_variables_model(3L)
  estimates <- smooth_estimates(model, n_2d = 3, n_3d = 2, dist = 0)
  differences <- difference_smooths(model, select = plot_variables_label(model),
    n_2d = 3, n_3d = 2)
  for (vars in list(c("x", "z"), c("x", "x", "w"),
      c("x", "z", "absent"), c("x", "z", NA_character_), 1:3)) {
    expect_error(smooth_estimates(model, variables = vars), "variables")
    expect_error(difference_smooths(model, select = plot_variables_label(model),
      variables = vars), "variables")
    expect_error(draw(estimates, variables = vars), "variables")
    expect_error(draw(differences, variables = vars), "variables")
  }
  mixed <- mgcv::gam(y ~ s(x, k = 5) + s(w, k = 5), data = model$model)
  expect_error(smooth_estimates(mixed, variables = "x"), "variables")
  expect_error(draw(mixed, variables = "x"), "variables")
  expect_silent(smooth_estimates(mixed, select = "s(x)", variables = "x"))
})

test_that("subsetting saved estimates retains only relevant variable metadata", {
  model <- plot_variables_model(3L)
  estimates <- smooth_estimates(model, variables = c("w", "x", "z"),
    n_2d = 3, n_3d = 2, dist = 0)
  label <- unique(estimates$.smooth)[1]
  subset <- estimates[estimates$.smooth == label, ]
  expect_identical(names(attr(subset, "plot_variables")), label)
  expect_identical(attr(subset, "plot_variables")[[label]], c("w", "x", "z"))
  expect_null(attr(estimates[, c("x", "z", "w")], "plot_variables"))
  expect_plot_variables_mapping(draw(subset)[[1]], c("w", "x", "z"))
})

test_that("swapping saved surface axes also swaps their support coordinates", {
  skip_if_not_installed("ggcube", "0.3.0")
  skip_if_not_installed("sf")
  model <- plot_variables_model(2L)
  estimates <- smooth_estimates(model, select = 1, n_2d = 8, dist = 0.1)
  original <- draw(estimates, surface = "3d")[[1]]
  swapped <- draw(estimates, variables = c("z", "x"), surface = "3d")[[1]]
  original_mesh <- original$layers[[1]]$data
  swapped_mesh <- swapped$layers[[1]]$data
  expect_gt(nrow(original_mesh), 0L)
  expect_equal(nrow(swapped_mesh), nrow(original_mesh))
  expect_equal(sort(swapped_mesh$.surface_z), sort(original_mesh$.surface_z))
  # Saved objects predating the variable metadata still have fitted order in
  # coordinate_info, which must keep their support coordinates aligned too.
  support <- attr(estimates, "surface_support")
  support[[1]]$variables <- NULL
  attr(estimates, "surface_support") <- support
  attr(estimates, "plot_variables") <- NULL
  old_swapped <- draw(estimates, variables = c("z", "x"),
    surface = "3d")[[1]]
  for (column in names(swapped_mesh)) {
    expect_equal(old_swapped$layers[[1]]$data[[column]], swapped_mesh[[column]])
  }
})

test_that("factor-smooth variable roles keep the continuous axis first", {
  model <- mgcv::gam(y ~ s(f, x, bs = "fs", k = 4),
    data = plot_variables_model(3L)$model)
  estimates <- smooth_estimates(model, n = 4)
  expect_identical(attr(estimates, "plot_variables")[[model$smooth[[1]]$label]],
    c("x", "f"))
  expect_equal(rlang::as_label(draw(estimates)[[1]]$mapping$x), "x")
  expect_error(smooth_estimates(model, variables = c("f", "x")), "variables")
  expect_error(draw(estimates, variables = c("f", "x")), "variables")
})

test_that("spherical smooths reject changing longitude and latitude roles", {
  d <- plot_variables_model(3L)$model
  d$longitude <- d$x * 300 - 150
  d$latitude <- d$w * 140 - 70
  model <- mgcv::gam(y ~ s(longitude, latitude, bs = "sos", k = 10), data = d)
  estimates <- smooth_estimates(model, n_2d = 4)
  expect_error(smooth_estimates(model, variables = c("latitude", "longitude")),
    "variables")
  expect_error(draw(estimates, variables = c("latitude", "longitude")),
    "variables")
})

test_that("selecting the second soap component retains explicit variable order", {
  estimates <- smooth_estimates(m_soap_sep, select = 2,
    variables = c("w", "v"), n_2d = 4)
  expect_identical(attr(estimates, "plot_variables")[[m_soap_sep$smooth[[2]]$label]],
    c("w", "v"))
  expect_plot_variables_mapping(draw(estimates)[[1]], c("w", "v"))
})
