# Small inputs characterize the renderer without fitting models. Integration
# coverage through both draw methods lives in the existing draw tests.
curve_data <- function(n = 3L) {
  d <- tibble::tibble(
    x = rep(c(1, 2, 3), n), f = factor(rep(seq_len(n), each = 3)),
    .estimate = rep(c(-1, 0, 1), n), .lower_ci = .estimate - 0.2,
    .upper_ci = .estimate + 0.2, .smooth = "s(x)", .term = "s(x)",
    .type = "TPRS", .by = NA_character_
  )
  class(d) <- c("mgcv_smooth", class(d))
  d
}

curve_geoms <- function(p) {
  unname(vapply(p$layers, function(l) class(l$geom)[1L], character(1L)))
}

test_that("ordinary curves preserve transformations, layers and styling", {
  d <- curve_data(1)
  names(d)[names(d) == "x"] <- "log(x)"
  r <- tibble::tibble(`log(x)` = 1:3, partial_residual = c(4, 5, 6))
  d$.increase <- c(NA, 0, 1)
  d$.decrease <- c(-1, NA, NA)
  p <- plot_smooth.mgcv_smooth(d, variables = "log(x)", constant = 2, fun = exp,
    partial_residuals = r, rug = r, ylim = c(-10, 20), angle = 45,
    ci_col = "pink", smooth_col = "red", resid_col = "blue",
    increase_col = "green", decrease_col = "purple", change_lwd = 2,
    caption = FALSE, xlab = "X", ylab = "Y", title = "Title",
    subtitle = "Subtitle")
  expect_equal(curve_geoms(p), c("GeomPoint", "GeomRibbon", "GeomLine",
    "GeomLine", "GeomLine", "GeomRug", "GeomBlank"))
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(b[[1]]$y, r$partial_residual)
  expect_equal(b[[2]]$ymin, exp(d$.lower_ci + 2))
  expect_equal(b[[2]]$ymax, exp(d$.upper_ci + 2))
  expect_equal(b[[3]]$y, exp(d$.estimate + 2))
  expect_equal(b[[4]]$y, d$.increase)
  expect_equal(b[[5]]$y, d$.decrease)
  expect_equal(unique(b[[2]]$fill), "pink")
  expect_equal(unique(b[[3]]$colour), "red")
  expect_equal(unique(b[[4]]$colour), "green")
  expect_equal(unique(b[[5]]$colour), "purple")
  expect_equal(p$labels$x, "X")
  expect_equal(p$labels$y, "Y")
  expect_equal(p$labels$title, "Title")
  expect_equal(p$labels$subtitle, "Subtitle")
  expect_null(p$labels$caption)
  expect_equal(p$guides$guides$x$params$angle, 45)
  expect_false(p$layers[[6]]$inherit.aes)
  d$.change <- d$.estimate
  p <- plot_smooth.mgcv_smooth(d, variables = "log(x)")
  expect_equal(curve_geoms(p), c("GeomRibbon", "GeomLine", "GeomLine"))
  expect_false(p$layers[[3]]$show.legend)
})

test_that("grouped by curves preserve palettes, rugs and change legends", {
  for (n in c(3L, 10L)) {
    d <- curve_data(n)
    d$.by <- "f"
    d$.change <- d$.estimate
    p <- plot_smooth.mgcv_smooth(d, variables = "x", grouped_by = TRUE, rug = d)
    b <- ggplot2::ggplot_build(p)$data
    expect_equal(length(unique(b[[2]]$group)), n)
    pal <- if (n > 9L) ggplot2::scale_colour_hue() else
      ggokabeito::scale_colour_okabe_ito()
    expect_equal(unique(b[[2]]$colour), pal$palette(n)[seq_len(n)])
    expect_equal(unique(b[[4]]$colour), "black")
    expect_true(is.na(p$layers[[3]]$show.legend))
    expect_equal(p$labels$subtitle, "By: f")
    expect_equal(p$labels$title, "s(x)")
    d$.change <- NULL
    d$.increase <- d$.estimate
    d$.decrease <- NA_real_
    p <- plot_smooth.mgcv_smooth(d, variables = "x", grouped_by = TRUE)
    expect_false(p$layers[[3]]$show.legend)
    expect_false(p$layers[[4]]$show.legend)
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("factor curves keep no intervals, hidden legends and label semantics", {
  d <- curve_data()
  class(d) <- c("factor_smooth", class(d))
  p <- plot_smooth.factor_smooth(d, variables = c("f", "x"), rug = d,
    constant = 2, fun = exp,
    discrete_colour = ggplot2::scale_colour_manual(values = c("red", "blue", "green")))
  expect_equal(curve_geoms(p), c("GeomLine", "GeomRug"))
  expect_equal(p$theme$legend.position, "none")
  expect_equal(p$labels$x, "x")
  expect_equal(p$labels$y, "Partial effect")
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(b[[1]]$y, exp(d$.estimate + 2))
  expect_equal(unique(b[[1]]$colour), c("red", "blue", "green"))
  expect_equal(unique(b[[2]]$colour), "black")
  p <- plot_smooth.factor_smooth(d, variables = c("x", "f"), xlab = NULL, ylab = NULL)
  expect_equal(p$labels$x, "x")
  expect_equal(p$labels$y, "Partial effect")
})

test_that("sz curves retain intervals, coloured rugs and interaction labels", {
  for (n in c(3L, 10L)) {
    d <- curve_data(n)
    class(d) <- c("sz_factor_smooth", class(d))
    p <- plot_smooth.sz_factor_smooth(d, variables = c("f", "x"), rug = d)
    expect_equal(curve_geoms(p), c("GeomRibbon", "GeomLine", "GeomRug"))
    b <- ggplot2::ggplot_build(p)$data
    expect_equal(b[[1]]$ymin, d$.lower_ci)
    expect_equal(b[[1]]$ymax, d$.upper_ci)
    expect_equal(b[[3]]$colour, b[[2]]$colour)
    expect_equal(p$labels$colour, "f")
    expect_equal(p$labels$fill, "f")
    d$g <- factor(rep(c("a", "b", "a"), n))
    p <- plot_smooth.sz_factor_smooth(d, variables = c("x", "f", "g"), rug = d)
    expect_equal(p$data$.sz_var, interaction(d[c("f", "g")],
      sep = ":", lex.order = TRUE))
    expect_equal(p$labels$colour, c("f", "g"))
    expect_equal(p$layers[[3]]$data$.sz_var, p$data$.sz_var)
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("by labels, disabled grouping and caption switches are preserved", {
  d <- curve_data(1)
  d$z <- 2
  d$.by <- "z"
  d$.smooth <- "s(x):z"
  p <- plot_smooth.mgcv_smooth(d, variables = "x", grouped_by = TRUE)
  expect_equal(p$labels$title, "s(x)")
  expect_equal(p$labels$subtitle, "By: z")
  expect_null(p$mapping$group)
  expect_equal(p$labels$caption, rep("Basis: TPRS", nrow(d)))
  for (caption in list(FALSE, "custom caption")) {
    p <- plot_smooth.mgcv_smooth(d, variables = "x", caption = caption)
    expect_null(p$labels$caption)
  }
  d$.by <- "f"
  d$f <- ordered(d$f)
  d$.smooth <- "s(x):f1"
  p <- plot_smooth.mgcv_smooth(d, variables = "x")
  expect_equal(p$labels$subtitle, "By: f; 1")
})

test_that("sz supports custom scales and transforms without enabling SiZer", {
  d <- curve_data()
  d$.change <- d$.estimate
  p <- plot_univariate_sz_smooth(d, variables = c("x", "f"),
    constant = 1, fun = exp, ci_alpha = 0.4,
    discrete_colour = ggplot2::scale_colour_manual(values = c("red", "blue", "green")),
    discrete_fill = ggplot2::scale_fill_manual(values = c("pink", "grey", "orange")))
  expect_equal(curve_geoms(p), c("GeomRibbon", "GeomLine"))
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(b[[1]]$ymin, exp(d$.lower_ci + 1))
  expect_equal(b[[1]]$ymax, exp(d$.upper_ci + 1))
  expect_equal(unique(b[[1]]$alpha), 0.4)
  expect_true(all(is.na(b[[1]]$colour)))
  expect_equal(unique(b[[1]]$fill), c("pink", "grey", "orange"))
  expect_equal(unique(b[[2]]$colour), c("red", "blue", "green"))
})

test_that("continuous by curves build through both draw entry points", {
  withr::local_seed(308)
  d <- data.frame(x = runif(60), z = runif(60))
  d$y <- sin(4 * d$x) * d$z + rnorm(60, sd = 0.1)
  m <- mgcv::gam(y ~ s(x, by = z, k = 5), data = d)
  sm <- smooth_estimates(m, n = 8)
  for (p in list(draw(m, n = 8, rug = FALSE, grouped_by = TRUE),
    draw(sm, grouped_by = TRUE))) {
    panel <- p[[1]]
    expect_equal(curve_geoms(panel), c("GeomRibbon", "GeomLine"))
    expect_equal(panel$labels$subtitle, "By: z")
    expect_silent(b <- ggplot2::ggplot_build(panel))
    expect_equal(b$data[[2]]$y, sm$.estimate)
    expect_equal(b$data[[1]]$ymin, panel$data$.lower_ci)
  }
})

test_that("curve methods preserve common label overrides and caption handling", {
  d <- curve_data(1)
  d$.by <- "f"
  d$.smooth <- "s(x):f1"
  methods <- list(plot_smooth.mgcv_smooth, plot_smooth.factor_smooth,
    plot_univariate_sz_smooth)
  for (i in seq_along(methods)) {
    variables <- if (i == 1L) "x" else c("x", "f")
    for (caption in list(NULL, TRUE, FALSE, "custom caption")) {
      p <- methods[[i]](d, variables = variables, title = "Custom: suffix",
        subtitle = "Custom subtitle", caption = caption, xlab = "X", ylab = "Y")
      expect_equal(p$labels$title, "Custom")
      expect_equal(p$labels$subtitle, "Custom subtitle")
      expect_equal(p$labels$x, "X")
      expect_equal(p$labels$y, "Y")
      if (is.null(caption) || isTRUE(caption)) {
        expect_equal(p$labels$caption, rep("Basis: TPRS", nrow(d)))
      } else {
        expect_null(p$labels$caption)
      }
    }
  }
  d$.term <- "Grouped term"
  p <- plot_smooth.mgcv_smooth(d, variables = "x", grouped_by = TRUE)
  expect_equal(p$labels$title, "Grouped term")
  p <- plot_smooth.mgcv_smooth(d, variables = "x", grouped_by = TRUE,
    title = "Custom: suffix", subtitle = "Custom subtitle")
  expect_equal(p$labels$title, "Custom: suffix")
  expect_equal(p$labels$subtitle, "Custom subtitle")
})

test_that("sz scale overrides leave the other aesthetic's default intact", {
  for (n in c(9L, 10L)) {
    d <- curve_data(n)
    default <- plot_univariate_sz_smooth(d, variables = c("x", "f"))
    custom_colour <- plot_univariate_sz_smooth(d, variables = c("x", "f"),
      discrete_colour = ggplot2::scale_colour_manual(values = rep("red", n)))
    custom_fill <- plot_univariate_sz_smooth(d, variables = c("x", "f"),
      discrete_fill = ggplot2::scale_fill_manual(values = rep("pink", n)))
    b <- ggplot2::ggplot_build(default)$data
    bc <- ggplot2::ggplot_build(custom_colour)$data
    bf <- ggplot2::ggplot_build(custom_fill)$data
    expect_equal(unique(bc[[2]]$colour), "red")
    expect_equal(bc[[1]]$fill, b[[1]]$fill)
    expect_equal(unique(bf[[1]]$fill), "pink")
    expect_equal(bf[[2]]$colour, b[[2]]$colour)
  }
})

test_that("continuous-x methods use shared axis defaults and keep overrides", {
  d <- curve_data()
  methods <- list(plot_smooth.mgcv_smooth, plot_smooth.factor_smooth,
    plot_univariate_sz_smooth)
  for (i in seq_along(methods)) {
    variables <- if (i == 1L) "x" else c("x", "f")
    omitted <- methods[[i]](d, variables = variables)
    defaults <- methods[[i]](d, variables = variables, xlab = NULL, ylab = NULL)
    custom <- methods[[i]](d, variables = variables, xlab = "Time", ylab = "Effect")
    blank <- methods[[i]](d, variables = variables, xlab = "", ylab = "")
    expect_equal(defaults$labels$x, "x")
    expect_equal(defaults$labels$y, "Partial effect")
    expect_equal(defaults$labels, omitted$labels)
    expect_equal(custom$labels$x, "Time")
    expect_equal(custom$labels$y, "Effect")
    expect_equal(blank$labels$x, "")
    expect_equal(blank$labels$y, "")
  }
})

test_that("fs axis defaults work through both draw entry points", {
  withr::local_seed(308)
  d <- data.frame(x = runif(60), f = factor(rep(1:3, each = 20)))
  d$y <- sin(4 * d$x) + as.numeric(d$f) + rnorm(60, sd = 0.1)
  m <- mgcv::gam(y ~ s(x, f, bs = "fs", k = 4), data = d)
  sm <- smooth_estimates(m, n = 8)
  # draw.gam() sends ... to wrap_plots(), not to the smooth methods.
  # Test its defaults, then exercise label overrides via draw.smooth_estimates().
  model_plot <- draw(m, n = 8, rug = FALSE)[[1]]
  omitted <- draw(sm)[[1]]
  defaults <- draw(sm, xlab = NULL, ylab = NULL)[[1]]
  custom <- draw(sm, xlab = "Time", ylab = "Effect")[[1]]
  blank <- draw(sm, xlab = "", ylab = "")[[1]]
  expect_equal(model_plot$labels$x, "x")
  expect_equal(model_plot$labels$y, "Partial effect")
  expect_equal(defaults$labels$x, "x")
  expect_equal(defaults$labels$y, "Partial effect")
  expect_equal(defaults$labels, omitted$labels)
  expect_equal(custom$labels$x, "Time")
  expect_equal(custom$labels$y, "Effect")
  expect_equal(blank$labels$x, "")
  expect_equal(blank$labels$y, "")
  expect_silent(ggplot2::ggplot_build(model_plot))
  expect_silent(ggplot2::ggplot_build(defaults))
})

test_that("factor-only point ranges retain explicit NULL axis labels", {
  d <- curve_data()
  d$x <- factor(d$x)
  omitted <- plot_smooth.factor_smooth(d, variables = c("x", "f"))
  suppressed <- plot_smooth.factor_smooth(d, variables = c("x", "f"),
    xlab = NULL, ylab = NULL)
  expect_equal(omitted$labels$x, "x")
  expect_equal(omitted$labels$y, "Partial effect")
  expect_true(all(c("x", "y") %in% names(suppressed$labels)))
  expect_null(suppressed$labels$x)
  expect_null(suppressed$labels$y)
  expect_equal(curve_geoms(suppressed), "GeomPointrange")
  expect_silent(ggplot2::ggplot_build(suppressed))
})
