label_control_example <- function() {
  data.frame(f = ordered(c("Boat (guided)", "Boat (unguided)", "Shore"),
    levels = c("Boat (guided)", "Boat (unguided)", "Shore")),
    x = c(0.01, 0.1, 1), y = c(1, 2, 3))
}

label_control_plot <- function() {
  ggplot2::ggplot(label_control_example(), ggplot2::aes(f, y, colour = f)) +
    ggplot2::geom_point()
}

label_control_axis <- function(plot, axis = "x") {
  ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$guides$get_params(axis)
}

label_control_text <- function(grob) {
  text <- if (inherits(grob, "text")) grob$label else character()
  unname(c(text, unlist(lapply(c(grob$grobs, grob$children), label_control_text))))
}

test_that("label controls validate groups and individual settings", {
  expect_s3_class(draw_label_control(), "draw_label_control")
  expect_equal(lengths(draw_label_control()), c(axes = 0L, legend = 0L, colourbar = 0L))
  for (bad in list(1, "wrap", list(12), list(wrap = 12, wrap = 15))) {
    expect_error(draw_label_control(axes = bad), "axes")
  }
  expect_error(draw_label_control(axes = list(width = 10)), "Unknown.*axes")
  expect_error(draw_label_control(legend = list(angle = 45)), "Unknown.*legend")
  expect_error(draw_label_control(colourbar = list(n_dodge = 2)), "Unknown.*colourbar")
  for (bad in list(0, -1, 1.5, NA_real_, Inf, TRUE, "12", numeric(), c(1, 2))) {
    expect_error(draw_label_control(axes = list(wrap = bad)), "axes\\$wrap")
    expect_error(draw_label_control(axes = list(n_dodge = bad)), "axes\\$n_dodge")
    expect_error(draw_label_control(legend = list(ncol = bad)), "legend\\$ncol")
    expect_error(draw_label_control(colourbar = list(title_wrap = bad)), "colourbar\\$title_wrap")
  }
  for (bad in list(NA_real_, Inf, 91, -91, c(0, 45), "45", TRUE)) {
    expect_error(draw_label_control(axes = list(angle = bad)), "axes\\$angle")
    expect_error(draw_label_control(colourbar = list(angle = bad)), "colourbar\\$angle")
  }
  for (bad in list(NA, 1, "TRUE", c(TRUE, FALSE))) {
    expect_error(draw_label_control(legend = list(byrow = bad)), "legend\\$byrow")
  }
  for (bad in list(5, grid::unit(c(1, 2), "mm"), grid::unit(-1, "mm"),
      grid::unit(NA_real_, "mm"))) {
    expect_error(draw_label_control(colourbar = list(width = bad)), "colourbar\\$width")
  }
  expect_error(draw(label_control_plot(), label_control = list(unknown = list())),
    "label_control")
  expect_error(validate_draw_label_control(list(axes = list(), axes = list())),
    "label_control")
  expect_equal(validate_draw_label_control(list(axes = list(wrap = 12))),
    draw_label_control(axes = list(wrap = 12)))
})

test_that("wrapping preserves missing labels, expressions and explicit line breaks", {
  labels <- c(a = "Boat (guided)", b = NA_character_, c = "Shore",
    d = "First line\n\nSecond line\n", e = "UnbrokenWordLongerThanTheWidth")
  expect_identical(wrap_draw_labels(labels, 10), c(a = "Boat\n(guided)",
    b = NA_character_, c = "Shore", d = "First line\n\nSecond\nline\n",
    e = "UnbrokenWordLongerThanTheWidth"))
  expr <- expression(alpha[1], beta[2])
  expect_identical(wrap_draw_labels(expr, 4), expr)
  mixed <- list("Boat (guided)", quote(alpha[1]), NA_character_)
  expect_identical(wrap_draw_labels(mixed, 10),
    list("Boat\n(guided)", quote(alpha[1]), NA_character_))
})

test_that("empty controls preserve existing plots exactly", {
  p <- label_control_plot()
  expect_identical(apply_draw_label_control(p, NULL), p)
  expect_identical(apply_draw_label_control(p, draw_label_control()), p)
  expect_identical(apply_draw_label_control(p,
    draw_label_control(axes = list(wrap = NULL))), p)
})

test_that("discrete axes wrap and stagger after scale labelling", {
  p <- label_control_plot() + ggplot2::scale_x_discrete(
    labels = function(x) paste("Type", x),
    guide = ggplot2::guide_axis(angle = 30, cap = "both"))
  control <- draw_label_control(axes = list(wrap = 12, n_dodge = 2, angle = 45))
  q <- apply_draw_label_control(p, control)
  a <- label_control_axis(q)
  expect_equal(a$key$.label,
    c("Type Boat\n(guided)", "Type Boat\n(unguided)", "Type Shore"))
  expect_equal(a$n.dodge, 2L)
  expect_equal(a$angle, 45)
  expect_equal(a$cap, "both")
  expect_equal(as.character(a$key$.value), levels(label_control_example()$f))
  expect_identical(ggplot2::ggplot_build(q)$data, ggplot2::ggplot_build(p)$data)
  expect_equal(label_control_axis(p)$angle, 30)
  expect_equal(label_control_axis(p)$n.dodge, 1)
  expect_silent(ggplot2::ggplotGrob(q))

  flipped <- apply_draw_label_control(p + ggplot2::coord_flip(), control)
  expect_equal(label_control_axis(flipped, "y")$n.dodge, 2L)
  expect_match(label_control_axis(flipped, "y")$key$.label[1], "\n")
  expect_silent(ggplot2::ggplotGrob(flipped))

  continuous <- ggplot2::ggplot(label_control_example(), ggplot2::aes(x, y)) +
    ggplot2::geom_point() + ggplot2::scale_x_log10()
  a <- label_control_axis(apply_draw_label_control(continuous, control))
  original <- label_control_axis(continuous)
  expect_identical(a$key, original$key)
  expect_identical(a$angle, original$angle)
  expect_identical(a$n.dodge, original$n.dodge)
})

test_that("axis titles wrap without changing other annotations", {
  title <- "A long descriptive axis title"
  p <- label_control_plot() + ggplot2::labs(x = title, y = title,
    title = title, subtitle = title, caption = title)
  q <- apply_draw_label_control(p, draw_label_control(axes = list(title_wrap = 12)))
  g <- ggplot2::ggplotGrob(q)
  for (name in c("xlab-b", "ylab-l")) {
    expect_equal(label_control_text(g$grobs[[which(g$layout$name == name)]]),
      wrap_draw_labels(title, 12))
  }
  for (name in c("title", "subtitle", "caption")) {
    expect_equal(label_control_text(g$grobs[[which(g$layout$name == name)]]), title)
  }
})

test_that("legend formatting retains custom scales, guides and merged keys", {
  d <- label_control_example()
  guide <- ggplot2::guide_legend(reverse = TRUE, order = 2,
    override.aes = list(size = 4), theme = ggplot2::theme(legend.text =
      ggplot2::element_text(colour = "red")))
  scale <- ggplot2::scale_colour_manual(
    values = c("navy", "orange", "grey"), name = "A long legend title",
    labels = function(x) paste("Type", x), guide = guide)
  p <- label_control_plot() + scale
  q <- apply_draw_label_control(p, draw_label_control(
    legend = list(wrap = 12, title_wrap = 12, nrow = 2, byrow = TRUE)))
  b <- ggplot2::ggplot_build(q)
  params <- b$plot$guides$params[[1]]
  expect_equal(params$title, "A long\nlegend title")
  expect_equal(params$key$.label,
    c("Type Shore", "Type Boat\n(unguided)", "Type Boat\n(guided)"))
  expect_equal(params$nrow, 2L)
  expect_true(params$theme$legend.byrow)
  expect_equal(params$theme$legend.text$colour, "red")
  expect_equal(params$override.aes$size, 4)
  expect_equal(params$order, 2)
  expect_true(params$reverse)
  expect_equal(guide$params$nrow, NULL)
  expect_equal(scale$labels(levels(d$f)), paste("Type", levels(d$f)))
  expect_null(scale$range$range)
  expect_identical(b$data, ggplot2::ggplot_build(p)$data)
  expect_silent(ggplot2::ggplotGrob(q))

  d$f <- factor(d$f, ordered = FALSE)
  d <- d[rep(seq_len(nrow(d)), each = 2L), ]
  d$x <- rep(1:2, 3)
  merged <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = f, fill = f,
    shape = f, linetype = f)) + ggplot2::geom_point(shape = 21) +
    ggplot2::geom_line() + ggplot2::geom_point()
  q <- apply_draw_label_control(merged,
    draw_label_control(legend = list(wrap = 10, ncol = 2)))
  params <- ggplot2::ggplot_build(q)$plot$guides$params
  expect_length(params, 1L)
  expect_equal(sort(params[[1]]$aesthetic), sort(c("colour", "fill", "shape", "linetype")))
  expect_match(params[[1]]$key$.label[1], "\n")
  expect_silent(ggplot2::ggplotGrob(q))
})

test_that("colour-bar layout preserves numeric formatting and orientation", {
  control <- draw_label_control(colourbar = list(title_wrap = 12, angle = 45,
    width = grid::unit(5, "mm"), height = grid::unit(35, "mm")))
  for (direction in c("horizontal", "vertical")) {
    guide <- ggplot2::guide_colourbar(direction = direction, reverse = TRUE,
      theme = ggplot2::theme(legend.title = ggplot2::element_text(colour = "red")))
    p <- ggplot2::ggplot(label_control_example(), ggplot2::aes(x, y, fill = y)) +
      ggplot2::geom_tile() + ggplot2::scale_fill_viridis_c(
        name = "A long colour bar title", breaks = c(1, 2, 3),
        labels = function(x) paste0(x, " units"), guide = guide)
    q <- apply_draw_label_control(p, control)
    b <- ggplot2::ggplot_build(q)
    params <- b$plot$guides$params[[1]]
    expect_equal(params$title, "A long\ncolour bar\ntitle")
    expect_equal(params$key$.label, c("1 units", "2 units", "3 units"))
    expect_equal(params$angle, 45)
    expect_equal(params$direction, direction)
    expect_true(params$reverse)
    expect_equal(params$theme$legend.key.width, grid::unit(5, "mm"))
    expect_equal(params$theme$legend.key.height, grid::unit(35, "mm"))
    expect_equal(params$theme$legend.title$colour, "red")
    expect_identical(b$data, ggplot2::ggplot_build(p)$data)
    expect_silent(ggplot2::ggplotGrob(q))
  }
})

test_that("hidden guides and later additions retain native precedence", {
  control <- draw_label_control(axes = list(wrap = 10, n_dodge = 2),
    legend = list(wrap = 10, title_wrap = 10), colourbar = list(title_wrap = 10))
  p <- label_control_plot() + ggplot2::guides(colour = "none", x = "none")
  q <- apply_draw_label_control(p, control)
  b <- ggplot2::ggplot_build(q)
  expect_length(b$plot$guides$params, 0L)
  expect_s3_class(b$layout$panel_params[[1]]$guides$get_guide("x"), "GuideNone")
  expect_silent(ggplot2::ggplotGrob(q))

  p <- apply_draw_label_control(label_control_plot(), control)
  q <- p + ggplot2::guides(x = ggplot2::guide_axis(n.dodge = 3))
  expect_equal(label_control_axis(q)$n.dodge, 3)
  expect_false(any(grepl("\n", label_control_axis(q)$key$.label)))
  q <- p + ggplot2::scale_x_discrete(labels = function(x) paste("New", x))
  expect_equal(label_control_axis(q)$key$.label,
    paste("New", levels(label_control_example()$f)))
  expect_equal(label_control_axis(q)$n.dodge, 1)
})

test_that("model generics apply controls to every returned panel", {
  withr::local_seed(431)
  d <- label_control_example()[rep(1:3, 30), ]
  d$x <- runif(nrow(d))
  d$y <- sin(d$x * 5) + as.numeric(d$f) + rnorm(nrow(d))
  m <- mgcv::gam(y ~ f + s(x, k = 5), data = d)
  control <- draw_label_control(axes = list(wrap = 10, n_dodge = 2, angle = 45))
  original <- assemble(m, n = 8)
  for (plots in list(assemble(m, n = 8, label_control = control),
      draw(m, n = 8, wrap = FALSE, label_control = control))) {
    expect_named(plots, names(original))
    expect_equal(label_control_axis(plots$f)$n.dodge, 2L)
    expect_match(label_control_axis(plots$f)$key$.label[1], "\n")
    expect_equal(label_control_axis(plots[[1]])$n.dodge, 1)
    for (i in seq_along(plots)) {
      expect_identical(ggplot2::ggplot_build(plots[[i]])$data,
        ggplot2::ggplot_build(original[[i]])$data)
    }
  }
  p <- draw(m, n = 8, label_control = control)
  expect_length(p, length(original))
  expect_equal(label_control_axis(p[[2]])$n.dodge, 2L)
  expect_silent(patchwork::patchworkGrob(p))
  expect_silent(patchwork::patchworkGrob(draw(parametric_effects(m), label_control = control)))
  expect_silent(patchwork::patchworkGrob(draw(smooth_estimates(m, n = 8),
    label_control = control)))
  expect_silent(patchwork::patchworkGrob(draw(derivatives(m, n = 8),
    label_control = control)))
  components <- variance_comp(m)
  expect_equal(label_control_axis(draw(components, n_dodge = 3,
    label_control = control))$n.dodge, 2L)
  expect_equal(label_control_axis(draw(components, flip = TRUE,
    label_control = control), "y")$n.dodge, 2L)
})

test_that("nested patchworks retain collected and merged legends", {
  p <- label_control_plot()
  nested <- patchwork::wrap_plots(p, patchwork::wrap_plots(p, p), guides = "collect")
  q <- apply_draw_label_control(nested,
    draw_label_control(axes = list(wrap = 10), legend = list(wrap = 10)))
  expect_length(q, length(nested))
  expect_equal(q$patches$layout$guides, "collect")
  for (panel in list(q[[1]], q[[2]][[1]], q[[2]][[2]])) {
    expect_match(label_control_axis(panel)$key$.label[1], "\n")
  }
  expect_silent(g <- patchwork::patchworkGrob(q))
  expect_equal(sum(g$layout$name == "guide-box"), 1L)
})

test_that("label controls work throughout the drawing methods", {
  withr::local_seed(432)
  withr::local_options(lifecycle_verbosity = "quiet")
  d <- label_control_example()[rep(1:3, 40), ]
  d$f <- factor(d$f, ordered = FALSE)
  d$x <- runif(nrow(d))
  d$z <- runif(nrow(d))
  d$y <- sin(d$x * 5) + d$z + as.numeric(d$f) + rnorm(nrow(d), sd = 0.2)
  m <- mgcv::gam(y ~ f + s(x, by = f, k = 4) + s(z, k = 4), data = d)
  control <- draw_label_control(axes = list(wrap = 10, n_dodge = 2, title_wrap = 12),
    legend = list(wrap = 10, title_wrap = 12),
    colourbar = list(title_wrap = 12, height = grid::unit(30, "mm")))
  objects <- list(
    basis = basis(m, select = "s(z)"),
    mgcv_smooth = basis(mgcv::s(x, k = 5), data = d),
    smooth_samples = smooth_samples(m, select = "s(z)", n_draws = 3, n_grid = 8, seed = 2),
    difference_smooth = difference_smooths(m, select = "s(x)", n = 8),
    compare_smooths = compare_smooths(m, alternative = m, select = "s(z)", n = 8),
    conditional_values = conditional_values(m, condition = c("x", "f"), n_vals = 8),
    conditional_differences = conditional_differences(m, by = "f", condition = "x", n_vals = 8),
    conditional_moments = conditional_moments(m, condition = c("x", "f"),
      moments = c("mean", "variance"), n_vals = 8, nuisance = "fixed"),
    overall_concurvity = model_concurvity(m),
    pairwise_concurvity = model_concurvity(m, pairwise = TRUE),
    penalty_df = penalty(m, select = "s(z)"),
    evaluated_parametric_term = evaluate_parametric_term(m, term = "f"),
    ale = ale(m, focal = "z", n_bins = 4)
  )
  for (name in names(objects)) {
    expect_silent(p <- draw(objects[[name]], label_control = control))
    if (inherits(p, "patchwork")) {
      expect_silent(patchwork::patchworkGrob(p))
    } else {
      expect_silent(ggplot2::ggplotGrob(p))
    }
  }
  # A numerical surface exercises partial derivatives and colour bars.
  surface <- su_m_bivar
  pd <- partial_derivatives(surface, focal = "x", n = 5)
  expect_silent(patchwork::patchworkGrob(draw(pd, label_control = control)))
  expect_silent(patchwork::patchworkGrob(draw(surface, n_2d = 5,
    contour = FALSE, label_control = control)))
  counts <- b_pois
  expect_silent(ggplot2::ggplotGrob(draw(rootogram(counts), label_control = control)))
  ids <- data.frame(.draw = 1:16, .chain = rep(1:2, each = 8),
    .iteration = rep(1:8, 2))
  draws <- sampling_finish(cbind(a = rnorm(16), b = rnorm(16)), ids,
    coefficient_names = c("a", "b"))
  for (type in c("trace", "rank")) {
    expect_silent(ggplot2::ggplotGrob(draw(sampling_diagnostics(draws), type = type,
      label_control = control)))
  }
  gm <- m_gamm
  expect_silent(patchwork::patchworkGrob(draw(gm, n = 8, label_control = control)))
})
