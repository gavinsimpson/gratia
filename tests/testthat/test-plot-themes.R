test_that("smooth and interval themes modify completed plots independently", {
  m <- su_m_quick_eg1
  p <- draw(m)
  original <- ggplot2::ggplot_build(p[[1]])$data
  q <- p & ggplot2::theme(
    geom.gratia.smooth = ggplot2::element_geom(colour = "navy", linewidth = 1.1),
    geom.gratia.interval = ggplot2::element_geom(fill = "orange"),
    gratia.alpha.interval = 0.15)
  for (i in seq_len(length(q))) {
    b <- ggplot2::ggplot_build(q[[i]])$data
    expect_equal(unique(b[[1]]$fill), "orange")
    expect_equal(unique(b[[1]]$alpha), .15)
    expect_equal(unique(b[[2]]$colour), "navy")
    expect_equal(unique(b[[2]]$linewidth), 1.1)
    expect_equal(b[[2]]$y, ggplot2::ggplot_build(p[[i]])$data[[2]]$y)
  }
  expect_equal(unique(original[[1]]$alpha), .2)
  expect_equal(unique(original[[1]]$fill), "black")
  expect_equal(unique(original[[2]]$colour), "black")
  expect_equal(ggplot2::ggplot_build(p[[1]])$data, original)
  expect_s3_class(q[[1]]$layers[[2]]$geom, "GeomLine")
  expect_s3_class(q[[1]]$layers[[2]]$geom, "GeomGratiaSmooth")
})

test_that("ordinary elements and complete themes preserve opacity settings", {
  p <- draw(m_1_smooth)[[1]]
  q <- p + ggplot2::theme_minimal() + ggplot2::theme(
    geom.gratia.interval = ggplot2::element_geom(fill = "red"),
    gratia.alpha.interval = .4) + ggplot2::theme(
      geom.gratia.interval = ggplot2::element_geom(colour = "grey50"))
  b <- ggplot2::ggplot_build(q)$data[[1]]
  expect_equal(unique(b$fill), "red")
  expect_equal(unique(b$alpha), .4)
  expect_equal(unique(b$colour), "grey50")
  expect_error(ggplot2::ggplot_build(p + ggplot2::theme(gratia.alpha.interval = 2)),
    "between zero and one")
})

test_that("package overrides are captured and scoped without changing ggplot2", {
  withr::local_options(list(gratia.plot.theme = NULL))
  active <- ggplot2::theme_get()
  parent <- ggplot2::GeomLine$default_aes
  m <- m_1_smooth
  p <- draw(m)
  override <- ggplot2::theme(geom.gratia.smooth = ggplot2::element_geom(colour = "purple"))
  expect_null(gratia_theme_set(override))
  expect_identical(gratia_theme_get(), override)
  q <- draw(m)
  gratia_theme_update(gratia.alpha.interval = .3)
  expect_equal(gratia_theme_get()$gratia.alpha.interval, .3)
  expect_equal(gratia_theme_get()$geom.gratia.smooth$colour, "purple")
  expect_equal(unique(ggplot2::ggplot_build(p[[1]])$data[[2]]$colour), "black")
  expect_equal(unique(ggplot2::ggplot_build(q[[1]])$data[[2]]$colour), "purple")
  gratia_theme_set(NULL)
  expect_equal(unique(ggplot2::ggplot_build(q[[1]])$data[[2]]$colour), "purple")
  expect_error(withr::with_options(list(gratia.plot.theme = override), stop("test error")), "test error")
  expect_null(gratia_theme_get())
  expect_identical(ggplot2::theme_get(), active)
  expect_identical(ggplot2::GeomLine$default_aes, parent)
  expect_error(gratia_theme_set("minimal"), "ggplot2 theme")
  expect_error(gratia_theme_set(ggplot2::theme(gratia.alpha.interval = NA_real_)),
    "between zero and one")
  plain <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2), ggplot2::aes(x, y)) + ggplot2::geom_line()
  expect_equal(unique(ggplot2::ggplot_build(plain)$data[[1]]$colour), "black")
})

test_that("explicit deprecated arguments override themes, even at old defaults", {
  withr::local_options(lifecycle_verbosity = "warning")
  m <- m_1_smooth
  expect_warning(p <- draw(m, smooth_col = "black"), class = "lifecycle_warning_deprecated")
  p <- p & ggplot2::theme(geom.gratia.smooth = ggplot2::element_geom(colour = "red"))
  expect_equal(unique(ggplot2::ggplot_build(p[[1]])$data[[2]]$colour), "black")
  expect_warning(q <- draw(m, ci_alpha = .2), class = "lifecycle_warning_deprecated")
  q <- q & ggplot2::theme(gratia.alpha.interval = .8)
  expect_equal(unique(ggplot2::ggplot_build(q[[1]])$data[[1]]$alpha), .2)
  expect_silent(draw(m))
  expect_warning(r <- qq_plot(m, point_col = "blue"), class = "lifecycle_warning_deprecated")
  r <- r + ggplot2::theme(geom.gratia.diagnostic.point = ggplot2::element_geom(colour = "red"))
  expect_true(any(vapply(ggplot2::ggplot_build(r)$data,
    function(x) "colour" %in% names(x) && all(x$colour == "blue"), logical(1))))
})

test_that("discrete palettes preserve defaults and match grouped fills", {
  okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
    "#D55E00", "#CC79A7", "#999999", "#000000")
  for (n in 0:10) {
    expected <- if (n <= 9) okabe_ito[seq_len(n)] else scales::pal_hue()(n)
    expect_identical(gratia_palette()(n), expected)
    expect_identical(gratia_palette(reverse = TRUE)(n), rev(expected))
  }
  for (name in names(gratia_tol_palettes())) {
    values <- gratia_tol_palettes()[[name]]
    expect_identical(gratia_palette(name)(length(values)), values)
    expect_identical(gratia_palette(name, reverse = TRUE)(length(values)), rev(values))
    expect_error(gratia_palette(name)(length(values) + 1L), "at most")
    expect_identical(gratia_palette(name)(0), character())
  }
  expect_identical(gratia_palette("tol_dark")(3), c("#4477BB", "#CC4466", "#228833"))
  expect_error(gratia_palette()(NA_real_), "non-negative integer")
  expect_error(gratia_palette(reverse = NA), "TRUE or FALSE")
  d <- data.frame(x = rep(1:3, 3), y = seq_len(9), f = factor(rep(letters[1:3], each = 3)))
  p <- gratia_plot(d, ggplot2::aes(x, y, colour = f, group = f)) +
    geom_gratia_smooth_line() + geom_gratia_interval_ribbon(
      ggplot2::aes(ymin = y - 1, ymax = y + 1, fill = f))
  scales <- prepare_smooth_scales(3)
  p <- p + scales$colour + scales$fill
  pal <- gratia_palette("tol_bright")
  b <- ggplot2::ggplot_build(p + ggplot2::theme(
    palette.colour.discrete = pal, palette.fill.discrete = pal))$data
  expect_identical(b[[1]]$colour, b[[2]]$fill)
  expect_setequal(b[[1]]$colour, pal(3))
  for (n in c(9, 10)) {
    x <- prepare_smooth_scales(n)
    d$f <- factor(d$f, levels = letters[seq_len(n)])
    q <- gratia_plot(d, ggplot2::aes(x, y, colour = f)) + geom_gratia_smooth_line() + x$colour
    expect_silent(ggplot2::ggplot_build(q))
  }
})

test_that("Okabe-Ito scales preserve missing values and fixed component colours", {
  colours <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
    "#D55E00", "#CC79A7", "#999999", "#000000")
  for (constructor in list(scale_colour_okabe_ito, scale_fill_okabe_ito)) {
    scale <- constructor(limits = letters[1:9], drop = FALSE)
    scale$train(factor(c("b", "i")))
    expect_identical(scale$map(c(letters[1:9], NA_character_)), c(colours, NA_character_))
    expect_identical(scale$get_limits(), letters[1:9])
    expect_error(scale$palette(10), "10 needed but only 9 provided", fixed = TRUE)
  }
})

test_that("diagnostic, predictive and sample components have separate themes", {
  m <- m_1_smooth
  q <- qq_plot(m) + ggplot2::theme(
    geom.gratia.diagnostic.point = ggplot2::element_geom(colour = "purple"),
    geom.gratia.reference = ggplot2::element_geom(colour = "green"))
  b <- ggplot2::ggplot_build(q)$data
  expect_true(any(vapply(b, function(x) "colour" %in% names(x) && all(x$colour == "purple"), logical(1))))
  expect_true(any(vapply(b, function(x) "colour" %in% names(x) && all(x$colour == "green"), logical(1))))
  p <- posterior_predictive_plot(m, n = 3, seed = 1) + ggplot2::theme(
    geom.gratia.observed = ggplot2::element_geom(colour = "red"),
    geom.gratia.replicated = ggplot2::element_geom(colour = "blue"),
    gratia.alpha.replicated = .1)
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(unique(b[[1]]$colour), "blue")
  expect_equal(unique(b[[1]]$alpha), .1)
  expect_equal(unique(b[[2]]$colour), "red")
  samples <- smooth_samples(m, n_draws = 2, n_grid = 10, seed = 1)
  p <- draw(samples) & ggplot2::theme(gratia.alpha.sample = .3)
  expect_equal(unique(ggplot2::ggplot_build(p[[1]])$data[[1]]$alpha), .3)
})

test_that("3D scene styling preserves geometry and explicit interval overrides", {
  skip_if_not_installed("ggcube")
  withr::local_options(lifecycle_verbosity = "quiet")
  m <- su_m_bivar
  p <- draw(m, n_2d = 8, surface = "3d", ci_3d = TRUE)[[1]]
  q <- p + ggplot2::theme(
    geom.gratia.surface.interval = ggplot2::element_geom(fill = "purple"),
    gratia.alpha.surface.interval = .4,
    geom.gratia.surface.rug = ggplot2::element_geom(colour = "red", pointsize = 3),
    gratia.alpha.surface.rug = .7)
  before <- ggplot2::ggplot_build(p)$data[[1]]
  after <- ggplot2::ggplot_build(q)$data[[1]]
  bounds <- after$surface_role %in% c("lower", "upper")
  rug <- after$surface_role == "surface.rug"
  expect_true(any(bounds))
  expect_equal(unique(after$surface_fixed_fill[bounds]), "purple")
  expect_equal(unique(after$surface_alpha[bounds]), I(.4))
  expect_equal(unique(after$surface_colour[rug]), "red")
  expect_equal(unique(after$surface_alpha[rug]), I(.7))
  expect_equal(unique(after$surface_size[rug]), 3)
  expect_equal(before[c("x", "y", "z", "group")], after[c("x", "y", "z", "group")])
  expect_silent(ggplot2::ggplotGrob(q))
  transparent <- p + ggplot2::theme(
    geom.gratia.surface.interval = ggplot2::element_geom(fill = NA))
  b <- ggplot2::ggplot_build(transparent)$data[[1]]
  interval <- b$surface_role %in% c("lower", "upper")
  expect_true(all(is.na(b$surface_fixed_fill[interval])))
  expect_true(all(b$surface_fill_fixed[interval]))
  expect_silent(ggplot2::ggplotGrob(transparent))
  p <- draw(m, n_2d = 8, surface = "3d", ci_3d = TRUE, ci_3d_alpha = .15)[[1]] +
    ggplot2::theme(gratia.alpha.surface.interval = .8)
  b <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(unique(b$surface_alpha[b$surface_role %in% c("lower", "upper")]), I(.15))
})


test_that("appraisal forwards predictive defaults without deprecating them", {
  withr::local_options(lifecycle_verbosity = "warning")
  m <- m_1_smooth
  expect_silent(p <- appraise(m, ppcheck = TRUE, n_ppc = 2, seed = 42))
  expect_s3_class(p, "patchwork")
})


test_that("forwarded defaults do not hide explicit nested arguments", {
  withr::local_options(lifecycle_verbosity = "warning")
  inner <- function(smooth_col = "black") {
    gratia_plot_arguments("smooth_col", environment())
    gratia_plot(data.frame(x = 1:2, y = 1:2), ggplot2::aes(x, y)) +
      geom_gratia_smooth_line(colour = smooth_col)
  }
  outer <- function(smooth_col = "black", explicit = FALSE) {
    gratia_plot_arguments("smooth_col", environment())
    if (explicit) inner(smooth_col = "black") else inner(smooth_col = smooth_col)
  }
  theme <- ggplot2::theme(geom.gratia.smooth = ggplot2::element_geom(colour = "red"))
  expect_silent(p <- outer())
  expect_equal(unique(ggplot2::ggplot_build(p + theme)$data[[1]]$colour), "red")
  expect_warning(p <- outer(explicit = TRUE), class = "lifecycle_warning_deprecated")
  expect_equal(unique(ggplot2::ggplot_build(p + theme)$data[[1]]$colour), "black")
})

test_that("smooth comparisons capture package theme settings", {
  m <- m_1_smooth
  comparison <- compare_smooths(m, Same = m, n = 10)
  withr::local_options(list(gratia.plot.theme = ggplot2::theme(gratia.alpha.interval = .35)))
  p <- draw(comparison)
  gratia_theme_set(NULL)
  expect_equal(unique(ggplot2::ggplot_build(p[[1]])$data[[1]]$alpha), .35)
})

test_that("diagnostic boxes and MRF intervals have their own roles", {
  d <- data.frame(x = factor(letters[1:2]), y = 1:2)
  p <- gratia_plot(d, ggplot2::aes(x, y)) +
    geom_gratia_diagnostic_boxplot(ggplot2::aes(ymin = y - 1, lower = y - .5,
      middle = y, upper = y + .5, ymax = y + 1), stat = "identity") +
    geom_gratia_smooth_linerange(ggplot2::aes(ymin = y - 1, ymax = y + 1)) +
    geom_gratia_smooth_point()
  p <- p + ggplot2::theme(
    geom.gratia.diagnostic.boxplot = ggplot2::element_geom(colour = "blue"),
    geom.gratia.interval = ggplot2::element_geom(colour = "red"),
    geom.gratia.smooth = ggplot2::element_geom(colour = "green"))
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(unique(b[[1]]$colour), "blue")
  expect_equal(unique(b[[2]]$colour), "red")
  expect_equal(unique(b[[3]]$colour), "green")
})


test_that("component elements retain native ink and border semantics", {
  d <- data.frame(x = 1:3, y = 1:3)
  settings <- ggplot2::element_geom(ink = "navy", paper = "white", borderwidth = 1.7)
  p <- gratia_plot(d, ggplot2::aes(x, y)) +
    geom_gratia_smooth_line(colour = "black") +
    geom_gratia_interval_ribbon(ggplot2::aes(ymin = y - 1, ymax = y + 1), fill = "black") +
    ggplot2::theme(geom.gratia.curve = settings, geom.gratia.interval = settings)
  native <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = y - 1, ymax = y + 1)) +
    ggplot2::theme(geom = settings)
  b <- ggplot2::ggplot_build(p)$data
  reference <- ggplot2::ggplot_build(native)$data
  expect_equal(b[[1]]$colour, reference[[1]]$colour)
  expect_equal(b[[2]]$fill, reference[[2]]$fill)
  expect_equal(b[[2]]$linewidth, reference[[2]]$linewidth)
})


test_that("mapped aesthetics and supplied scales take precedence over themes", {
  d <- data.frame(x = rep(1:3, 2), y = 1:6, group = rep(c("a", "b"), each = 3))
  p <- gratia_plot(d, ggplot2::aes(x, y, colour = group, group = group)) +
    geom_gratia_smooth_line() + geom_gratia_interval_ribbon(
      ggplot2::aes(ymin = y - 1, ymax = y + 1, fill = group))
  supplied <- prepare_smooth_scales(2,
    discrete_colour = ggplot2::scale_colour_manual(values = c("red", "blue")),
    discrete_fill = ggplot2::scale_fill_manual(values = c("red", "blue")))
  p <- p + supplied$colour + supplied$fill + ggplot2::theme(
    geom.gratia.smooth = ggplot2::element_geom(colour = "green"),
    geom.gratia.interval = ggplot2::element_geom(fill = "green"),
    palette.colour.discrete = gratia_palette("tol_muted"),
    palette.fill.discrete = gratia_palette("tol_muted"))
  b <- ggplot2::ggplot_build(p)$data
  expect_setequal(b[[1]]$colour, c("red", "blue"))
  expect_identical(b[[1]]$colour, b[[2]]$fill)
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3)
  pal <- scales::pal_gradient_n(c("red", "blue"))
  p <- gratia_plot(d, ggplot2::aes(x, y, fill = z)) + geom_gratia_surface_tile() +
    gratia_default_scale(ggplot2::scale_fill_distiller(palette = "RdBu")) +
    ggplot2::theme(palette.fill.continuous = pal)
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$plot$scales$get_scales("fill")$palette(c(0, 1)), pal(c(0, 1)))
})


test_that("surface legend defaults respect captured package settings", {
  m <- su_m_bivar
  withr::local_options(list(gratia.plot.theme = ggplot2::theme(legend.position = "bottom")))
  p <- draw(m, n_2d = 6)[[1]]
  expect_equal(p$theme$legend.position, "bottom")
  if (requireNamespace("ggcube", quietly = TRUE)) {
    p <- draw(m, n_2d = 6, surface = "3d")[[1]]
    expect_equal(p$theme$legend.position, "bottom")
  }
})


test_that("independent fixed-colour layers do not join a mapped colour legend", {
  d <- data.frame(x = rep(1:3, 2), y = 1:6,
    f = factor(rep(c("a", "b"), each = 3)))
  points <- data.frame(x = 1:3, y = 2:4, f = factor(c("a", "b", "a")))
  for (mapping in list(ggplot2::aes(x, y), ggplot2::aes(x, y, colour = f))) {
    p <- gratia_plot(d, ggplot2::aes(x, y, colour = f)) +
      ggplot2::geom_line() + geom_gratia_partial_residual_point(
        data = points, mapping = mapping, inherit.aes = FALSE,
        colour = "steelblue3", alpha = .5)
    for (style in list(ggplot2::theme(), ggplot2::theme(
      geom.gratia.partial.residual = ggplot2::element_geom(colour = "purple"),
      gratia.alpha.partial.residual = .7))) {
      b <- ggplot2::ggplot_build(p + style)
      expect_length(b$plot$guides$params, 1L)
      expect_length(b$plot$guides$params[[1L]]$decor, 1L)
      expected_colour <- if (is.null(style$geom.gratia.partial.residual)) "steelblue3" else "purple"
      expected_alpha <- if (is.null(style$gratia.alpha.partial.residual)) .5 else .7
      expect_equal(unique(b$data[[2L]]$colour), expected_colour)
      expect_equal(unique(b$data[[2L]]$alpha), expected_alpha)
      expect_equal(b$data[[2L]]$y, points$y)
    }
  }
})
