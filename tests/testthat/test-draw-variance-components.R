variance_plot_fixture <- function() {
  structure(tibble::tibble(.component = c("s(long_covariate)", "group:(Intercept)", "scale"),
    .variance = c(4, 1, 0.25), .std_dev = c(2, 1, 0.5),
    .lower_ci = c(1, NA_real_, 0), .upper_ci = c(3, NA_real_, Inf)),
    class = c("variance_comp", "tbl_df", "tbl", "data.frame"))
}

test_that("variance plots transform intervals to the plotted scale", {
  x <- variance_plot_fixture()
  original <- x
  expect_silent(p <- draw(x))
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$.estimate, x$.variance)
  expect_equal(p$data$.lower, x$.lower_ci^2)
  expect_equal(p$data$.upper, x$.upper_ci^2)
  expect_equal(p$labels$y, "Variance")
  expect_null(p$labels$x)
  expect_null(p$labels$title)
  expect_null(p$labels$subtitle)
  expect_null(p$labels$caption)
  expect_silent(b <- ggplot2::ggplot_build(p))
  expect_equal(b$data[[2]]$ymin, 1)
  expect_equal(b$data[[2]]$ymax, 9)
  expect_equal(b$data[[3]]$y, x$.variance)
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), x$.component,
    ignore_attr = TRUE)
  expect_silent(ggplot2::ggplotGrob(p))
  sd <- draw(x, type = "std_dev")
  expect_equal(sd$data$.estimate, x$.std_dev)
  expect_equal(sd$data$.lower, x$.lower_ci)
  expect_equal(sd$data$.upper, x$.upper_ci)
  expect_equal(sd$labels$y, "Standard deviation")
  expect_silent(ggplot2::ggplotGrob(sd))
  expect_identical(x, original)
  expect_error(draw(x, type = "proportion"), "type = .*terms")
})

test_that("component axes can be staggered, angled and flipped", {
  x <- variance_plot_fixture()
  selected <- c(3, 1)
  for (flip in c(FALSE, TRUE)) {
    for (angle in list(NULL, 45, -45)) {
      p <- draw(x, select = selected, angle = angle, n_dodge = 2, flip = flip)
      expect_equal(p$scales$get_scales("x")$guide$params$angle, angle)
      expect_equal(p$scales$get_scales("x")$guide$params$n.dodge, 2L)
      expect_silent(b <- ggplot2::ggplot_build(p))
      if (flip) {
        expect_s3_class(p$coordinates, "CoordFlip")
        expect_equal(rev(b$layout$panel_params[[1]]$y$get_labels()),
          x$.component[selected])
      } else {
        expect_equal(b$layout$panel_params[[1]]$x$get_labels(), x$.component[selected],
          ignore_attr = TRUE)
      }
      expect_equal(b$data[[3]]$y, x$.variance[selected])
      expect_equal(b$data[[2]]$ymin, 1)
      expect_equal(b$data[[2]]$ymax, 9)
      expect_silent(ggplot2::ggplotGrob(p))
    }
  }
  for (bad in list(NA, NULL, c(TRUE, FALSE), 1, "true")) {
    expect_error(draw(x, flip = bad), "'flip' must be TRUE or FALSE", fixed = TRUE)
  }
  for (bad in list(NA_real_, Inf, 91, -91, TRUE, c(0, 45), "45")) {
    expect_error(draw(x, angle = bad), "'angle' must be NULL", fixed = TRUE)
  }
  for (bad in list(0, -1, 1.5, Inf, NA_real_, NULL, TRUE, c(1, 2))) {
    expect_error(draw(x, n_dodge = bad), "n_dodge")
  }
})

test_that("component selection preserves order and handles repeated labels", {
  x <- variance_plot_fixture()
  p <- draw(x, select = c("scale", "s(long_covariate)"))
  expect_equal(p$data$.component, x$.component[c(3, 1)])
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), x$.component[c(3, 1)],
    ignore_attr = TRUE)
  expect_equal(draw(x, select = c(3, 1))$data, p$data)
  expect_equal(draw(x, select = c(TRUE, FALSE, TRUE))$data$.component,
    x$.component[c(1, 3)])
  x$.component[2] <- x$.component[1]
  expect_silent(b <- ggplot2::ggplot_build(draw(x)))
  expect_equal(length(unique(b$data[[3]]$x)), 3L)
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), x$.component,
    ignore_attr = TRUE)
  expect_error(draw(x, select = x$.component[1]), "ambiguous.*positions")
  expect_silent(draw(x, select = 2))
  expect_error(draw(x, select = "unknown"), "Unknown name")
  for (bad in list(0, -1, 0.5, Inf, NA_real_, TRUE, c(TRUE, NA, FALSE))) {
    expect_error(draw(x, select = bad), "select")
  }
  expect_error(draw(x, select = integer()), "at least one")
  expect_error(draw(x, select = c(1, 1)), "without duplicates")
  expect_error(draw(x, type = "unknown"), "arg")
  expect_error(draw(x, include_total = NA), "include_total")
  expect_error(draw(x, predictor = 1), "only used")
  expect_error(draw(x, ncol = 0), "ncol")
  expect_error(draw(x, nrow = 1.5), "nrow")
  expect_error(draw(x, colour = "red"), "Additional arguments")
  expect_error(draw(x[FALSE, ]), "No variance components")
})

test_that("proportion plots preserve signed estimates, bounds and total selection", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$total <- d$x
  d$z <- d$x + runif(nrow(d), -0.05, 0.05)
  d$y <- 2 * d$x - d$z + rnorm(nrow(d), sd = 0.05)
  m <- gam(y ~ total + z, data = d)
  x <- variance_comp(m, type = "terms", n_draws = 30, seed = 83)
  expect_equal(attr(x, "term_groups"), list(total = c("total.1", "z")))
  p <- draw(x, type = "proportion")
  expect_equal(p$data$.estimate, x$.proportion)
  expect_equal(p$data$.lower, x$.proportion_lower_ci)
  expect_equal(p$data$.upper, x$.proportion_upper_ci)
  expect_equal(p$labels$y, "Proportion of total variance")
  expect_silent(b <- ggplot2::ggplot_build(p))
  expect_true(any(b$data[[3]]$y < 0))
  expect_true(any(b$data[[3]]$y > 1))
  expect_true(any(b$data[[2]]$ymin < 0))
  expect_true(any(b$data[[2]]$ymax > 1))
  expect_equal(draw(x, include_total = FALSE)$data$.component, c("total.1", "z"))
  expect_equal(draw(x, type = "proportion", include_total = FALSE)$data$.component,
    c("total.1", "z"))
  expect_error(draw(x, select = "total", include_total = FALSE), "No components")
  # Drawing uses stored quantities and must neither sample nor refit.
  state <- .Random.seed
  testthat::local_mocked_bindings(variance_comp = function(...) stop("refitting"),
    gaussian_draws = function(...) stop("sampling"))
  expect_silent(ggplot2::ggplotGrob(draw(x, type = "proportion")))
  expect_identical(.Random.seed, state)
})

test_that("proportion panels contain the correct shared terms and totals", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- gam(list(y ~ x, ~z, 1 + 2 ~ 0 + s(w, k = 6)), data = d, family = gaulss())
  x <- variance_comp(m, type = "terms", n_draws = 20, seed = 83)
  groups <- list(total = c("x", "s.2(w)"), total.1 = c("z.1", "s.2(w)"))
  expect_equal(attr(x, "term_groups"), groups)
  p <- draw(x, type = "proportion", ncol = 1)
  expect_silent(b <- ggplot2::ggplot_build(p))
  expect_equal(nrow(b$layout$layout), 2L)
  for (k in 1:2) {
    panel <- p$data[as.character(p$data$.predictor) == paste("Linear predictor", k), ]
    expect_equal(panel$.component, c(groups[[k]], names(groups)[k]))
    expect_equal(b$layout$panel_params[[k]]$x$get_labels(), panel$.component,
      ignore_attr = TRUE)
    index <- match(panel$.component, x$.component)
    suffix <- if (k == 1) "" else ".1"
    expect_equal(panel$.estimate, x[[paste0(".proportion", suffix)]][index])
    expect_equal(panel$.lower, x[[paste0(".proportion_lower_ci", suffix)]][index])
    expect_equal(panel$.upper, x[[paste0(".proportion_upper_ci", suffix)]][index])
  }
  expect_silent(ggplot2::ggplotGrob(p))
  flipped <- draw(x, type = "proportion", flip = TRUE, n_dodge = 2)
  expect_silent(flipped_build <- ggplot2::ggplot_build(flipped))
  for (k in 1:2) {
    expect_equal(rev(flipped_build$layout$panel_params[[k]]$y$get_labels()),
      c(groups[[k]], names(groups)[k]))
  }
  expect_silent(ggplot2::ggplotGrob(flipped))
  second <- draw(x, type = "proportion", predictor = 2, include_total = FALSE)
  expect_equal(second$data$.component, groups$total.1)
  expect_equal(second$data, draw(x, type = "proportion", predictor = "total.1",
    include_total = FALSE)$data)
  expect_silent(ggplot2::ggplotGrob(second))
  expect_error(draw(x, type = "proportion", predictor = 3), "predictor")
  expect_error(draw(x, type = "proportion", predictor = TRUE), "predictor")
  expect_error(draw(x, type = "proportion", predictor = "total.8"), "Unknown name")
  # Subsetting rows keeps the original grouping metadata, but no dropped rows
  # should be reintroduced by the plot method.
  subset <- x[x$.component != "x", ]
  expect_false("x" %in% draw(subset, type = "proportion")$data$.component)
})

test_that("missing intervals and undefined proportions retain component labels", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- gam(list(y ~ x, ~1), data = d, family = gaulss())
  x <- variance_comp(m, type = "terms", n_draws = 0)
  p <- draw(x)
  expect_silent(b <- ggplot2::ggplot_build(p))
  expect_equal(nrow(b$data[[2]]), 0L)
  expect_equal(nrow(b$data[[3]]), nrow(x))
  prop <- draw(x, type = "proportion")
  expect_silent(b <- ggplot2::ggplot_build(prop))
  expect_equal(nrow(b$layout$layout), 2L)
  expect_equal(as.character(prop$data$.component), c("x", "total", "total.1"))
  expect_equal(nrow(b$data[[3]]), 2L)
  expect_silent(ggplot2::ggplotGrob(prop))
  m <- gam(y ~ 1, data = d)
  x <- variance_comp(m, type = "terms", n_draws = 10, seed = 83)
  expect_silent(b <- ggplot2::ggplot_build(draw(x, type = "proportion")))
  expect_equal(nrow(b$data[[2]]), 0L)
  expect_equal(nrow(b$data[[3]]), 0L)
  expect_equal(b$layout$panel_params[[1]]$x$get_labels(), "total", ignore_attr = TRUE)
  expect_silent(ggplot2::ggplotGrob(draw(x, type = "proportion")))
  # A stored interval can be defined even when the fitted proportion is not.
  interval_only <- x
  interval_only$.proportion_lower_ci <- 0.8
  interval_only$.proportion_upper_ci <- 1.2
  expect_silent(b <- ggplot2::ggplot_build(draw(interval_only, type = "proportion")))
  expect_equal(b$data[[2]]$ymin, 0.8)
  expect_equal(b$data[[2]]$ymax, 1.2)
  expect_equal(nrow(b$data[[3]]), 0L)
  # Saved results from earlier versions have no term_groups attribute.
  attr(x, "term_groups") <- NULL
  expect_silent(ggplot2::ggplotGrob(draw(x, type = "proportion")))
  raw <- variance_plot_fixture()
  raw$.component[1] <- "total"
  expect_equal(nrow(draw(raw, include_total = FALSE)$data), 3L)
  raw$.lower_ci <- raw$.upper_ci <- NULL
  expect_silent(ggplot2::ggplotGrob(draw(raw)))
})

test_that("variance component points and intervals use the package theme system", {
  withr::local_options(list(gratia.plot.theme = NULL))
  x <- variance_plot_fixture()
  p <- draw(x) + ggplot2::theme(
    geom.gratia.effect = ggplot2::element_geom(colour = "navy", pointsize = 3),
    geom.gratia.interval = ggplot2::element_geom(colour = "firebrick", linewidth = 1),
    gratia.alpha.interval = 0.4)
  b <- ggplot2::ggplot_build(p)$data
  expect_equal(unique(b[[2]]$colour), "firebrick")
  expect_equal(unique(b[[2]]$linewidth), 1)
  expect_equal(unique(b[[2]]$alpha), 0.4)
  expect_equal(unique(b[[3]]$colour), "navy")
  expect_equal(unique(b[[3]]$size), 3)
  withr::local_options(list(gratia.plot.theme = ggplot2::theme(
    geom.gratia.effect = ggplot2::element_geom(colour = "purple"))))
  expect_equal(unique(ggplot2::ggplot_build(draw(x))$data[[3]]$colour), "purple")
})
