time_varying_plot_data <- function(time = 2001:2005) {
  label <- "time_varying(x)"
  out <- tibble::tibble(.term = label, .type = "time_varying", .value = time,
    .partial = c(.2, .5, .6, .3, .1), .se = c(.1, .12, .08, .09, .1))
  attr(out, "time_varying_info") <- stats::setNames(list(list(
    time = "year", coefficient = "x", model_label = "Delta component 2")), label)
  attr(out, "back_transform") <- FALSE
  class(out) <- c("parametric_effects", class(out))
  out
}

test_that("time-varying coefficient styles use the stored estimates", {
  d <- time_varying_plot_data()
  for (style in c("ribbon", "pointrange")) {
    expect_silent(p <- draw(d, time_varying_style = style)[[1L]])
    expect_silent(b <- ggplot2::ggplot_build(p))
    expect_identical(p$labels$x, "year")
    expect_identical(p$labels$y, "Coefficient")
    expect_identical(p$labels$title, "time_varying(x)")
    expect_identical(p$labels$subtitle, "Delta component 2")
    expect_null(p$labels$caption)
    expect_equal(p$data$.partial, d$.partial)
    expect_equal(p$data$.lower_ci, d$.partial - qnorm(.975) * d$.se)
    expect_equal(p$data$.upper_ci, d$.partial + qnorm(.975) * d$.se)
    expect_false(any(vapply(p$layers,
      function(x) inherits(x$geom, "GeomRug"), logical(1))))
    if (style == "ribbon") {
      expect_true(any(vapply(p$layers,
        function(x) inherits(x$geom, "GeomRibbon"), logical(1))))
    } else {
      expect_true(any(vapply(p$layers,
        function(x) inherits(x$geom, "GeomLinerange"), logical(1))))
    }
  }
  expect_error(draw(d, time_varying_style = "smooth"), "arg")
})

test_that("time-varying coefficient intervals follow level and transformations", {
  d <- time_varying_plot_data()
  for (style in c("ribbon", "pointrange")) {
    p <- draw(d, time_varying_style = style,
      ci_level = .8, constant = .5, fun = exp)[[1L]]
    expect_equal(p$data$.partial, exp(d$.partial + .5))
    expect_equal(p$data$.lower_ci, exp(d$.partial + .5 - qnorm(.9) * d$.se))
    expect_equal(p$data$.upper_ci, exp(d$.partial + .5 + qnorm(.9) * d$.se))
    expect_silent(ggplot2::ggplot_build(p))
  }
})

test_that("time-varying coefficient plots preserve modelled time scales", {
  times <- list(c(2001, 2002, 2005, 2008, 2010),
    as.Date("2001-01-01") + c(0, 2, 5, 7, 11),
    ordered(c("a", "b", "c", "d", "e")))
  for (time in times) {
    for (style in c("ribbon", "pointrange")) {
      d <- time_varying_plot_data(time)
      p <- draw(d, time_varying_style = style)[[1L]]
      expect_identical(p$data$.value, time)
      expect_silent(ggplot2::ggplot_build(p))
      expect_silent(ggplot2::ggplotGrob(p))
    }
  }
})

test_that("nested time-varying estimates keep labels and support both styles", {
  d <- time_varying_plot_data()
  info <- attr(d, "time_varying_info")
  nested <- tidyr::nest(d, data = c(".value", ".partial", ".se"))
  attr(nested, "time_varying_info") <- info
  class(nested) <- c("parametric_effects", class(nested))
  for (style in c("ribbon", "pointrange")) {
    p <- draw(nested, time_varying_style = style)[[1L]]
    expect_identical(p$labels$x, "year")
    expect_identical(p$labels$subtitle, "Delta component 2")
    expect_silent(ggplot2::ggplotGrob(p))
  }
  single <- d[1L, ]
  expect_silent(ggplot2::ggplotGrob(draw(single)[[1L]]))
})

test_that("time-varying coefficients use effect and interval theme settings", {
  d <- time_varying_plot_data()
  component_theme <- ggplot2::theme(
    geom.gratia.effect = ggplot2::element_geom(colour = "navy", pointsize = 3),
    geom.gratia.interval = ggplot2::element_geom(colour = "firebrick", fill = "pink"),
    gratia.alpha.interval = .3)
  for (style in c("ribbon", "pointrange")) {
    p <- draw(d, time_varying_style = style)[[1L]] + component_theme
    b <- ggplot2::ggplot_build(p)
    expect_identical(unique(b$data[[3L]]$colour), "navy")
    expect_equal(unique(b$data[[3L]]$size), 3)
    if (style == "ribbon") {
      expect_identical(unique(b$data[[1L]]$fill), "pink")
      expect_equal(unique(b$data[[1L]]$alpha), .3)
    } else {
      expect_identical(unique(b$data[[2L]]$colour), "firebrick")
      expect_equal(unique(b$data[[2L]]$alpha), .3)
    }
  }
})
