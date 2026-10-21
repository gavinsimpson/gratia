factor_control_models <- local({
  models <- NULL
  function() {
    if (!is.null(models)) return(models)
    withr::local_seed(244)
    d <- data.frame(x = runif(180), z = runif(180), w = runif(180),
      f = factor(rep(letters[1:3], 60)),
      g = factor(rep(c("u", "v", "w"), each = 60)))
    d$o <- ordered(d$f, levels = c("c", "a", "b"))
    d$y <- sin(5 * d$x) + cos(3 * d$z) + as.numeric(d$f) + rnorm(180, sd = .3)
    models <<- list(data = d,
      mixed = mgcv::gam(y ~ s(w, k = 4) + s(x, by = f, k = 4) +
        s(z, f, bs = "fs", k = 4) + s(w, g, bs = "sz", k = 4), data = d),
      ordered = mgcv::gam(y ~ s(x, k = 4) + s(x, by = o, k = 4), data = d),
      both = mgcv::gam(y ~ s(x, k = 4) + s(x, by = o, k = 4) +
        s(z, by = f, k = 4), data = d),
      two = mgcv::gam(y ~ s(x, k = 4) + s(x, f, bs = "sz", k = 4) +
        s(x, g, bs = "sz", k = 4) + s(x, f, g, bs = "sz", k = 4), data = d))
    models
  }
})

factor_control_sets <- function() {
  types <- c("factor-by", "sz", "fs")
  lapply(0:7, function(i) types[as.logical(intToBits(i)[1:3])])
}

factor_control_type <- function(p) {
  type <- unique(p$data$.type)
  if (type == "Factor smooth") return("fs")
  if (type == "Constr. factor smooth") return("sz")
  by <- unique(p$data$.by)
  if (length(by) == 1L && !is.na(by) && is.factor(p$data[[by]])) "factor-by" else "ordinary"
}

factor_control_ribbon <- function(p) {
  any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomRibbon"), logical(1)))
}

test_that("every grouping and ribbon subset works for mixed factor smooths", {
  m <- factor_control_models()$mixed
  estimates <- smooth_estimates(m, n = 7)
  entries <- list(
    model = function(grouped, show_ci) assemble(m, grouped = grouped,
      show_ci = show_ci, n = 7, rug = FALSE, parametric = FALSE),
    saved = function(grouped, show_ci) draw(estimates,
      grouped = grouped, show_ci = show_ci),
    nested = function(grouped, show_ci) draw(
      smooth_estimates(m, n = 7, unnest = FALSE),
      grouped = grouped, show_ci = show_ci))
  for (entry in entries) {
    for (types in factor_control_sets()) {
      p <- entry(types, types)
      expect_length(p, 1L + sum(ifelse(c("factor-by", "fs", "sz") %in% types, 1L, 3L)))
      for (i in seq_len(length(p))) {
        panel <- p[[i]]
        type <- factor_control_type(panel)
        expect_identical(factor_control_ribbon(panel),
          type == "ordinary" || type %in% types)
        expect_silent(ggplot2::ggplot_build(panel))
      }
    }
    for (types in factor_control_sets()) {
      p <- entry(c("sz", "fs"), types)
      expect_length(p, 6L)
      expect_identical(vapply(seq_len(length(p)), function(i) {
        factor_control_ribbon(p[[i]])
      }, logical(1)), c(TRUE, rep("factor-by" %in% types, 3),
        "fs" %in% types, "sz" %in% types))
    }
  }
  default <- assemble(m, n = 7, rug = FALSE, parametric = FALSE)
  expect_length(default, 6L)
  expect_identical(unname(vapply(default, factor_control_ribbon, logical(1))),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  separate_fs <- assemble(m, select = 5, n = 7, grouped = character())
  for (panel in separate_fs) {
    ref <- estimates[estimates$.type == "Factor smooth" &
      estimates$f == panel$data$f[1], ]
    expect_equal(panel$data$.estimate, ref$.estimate)
  }
})

test_that("factor plot controls are sets and reject invalid values", {
  m <- factor_control_models()$mixed
  expected <- assemble(m, n = 5, rug = FALSE, parametric = FALSE,
    grouped = c("fs", "sz"), show_ci = "sz")
  actual <- assemble(m, n = 5, rug = FALSE, parametric = FALSE,
    grouped = c("sz", "fs", "sz"), show_ci = c("sz", "sz"))
  expect_identical(names(actual), names(expected))
  expect_identical(vapply(actual, factor_control_ribbon, logical(1)),
    vapply(expected, factor_control_ribbon, logical(1)))
  for (invalid in list(TRUE, FALSE, NA_character_, "ordered", NULL, 1)) {
    expect_error(draw(m, grouped = invalid), "grouped.*character vector")
    expect_error(draw(m, show_ci = invalid), "show_ci.*character vector")
    sm <- smooth_estimates(m, n = 5)
    expect_error(draw(sm, grouped = invalid), "grouped.*character vector")
    expect_error(draw(sm, show_ci = invalid), "show_ci.*character vector")
  }
})

test_that("ordered deviations keep their fitted levels and reference smooth separate", {
  models <- factor_control_models()
  for (m in list(models$ordered, models$both)) {
    for (nested in c(FALSE, TRUE)) {
      sm <- smooth_estimates(m, n = 7, unnest = !nested)
      flat <- if (nested) tidyr::unnest(sm, cols = "data") else sm
      for (grouped in list(character(), "factor-by")) {
        plots <- list(
          assemble(m, n = 7, grouped = grouped, parametric = FALSE),
          draw(sm, grouped = grouped))
        for (p in plots) {
          expected <- if (length(m$smooth) == 3L) {
            if (length(grouped)) 2L else 3L
          } else if (length(grouped)) 3L else 6L
          expect_length(p, expected)
          expect_equal(unique(p[[1]]$data$.smooth), "s(x)")
          ordered_panels <- which(vapply(seq_len(length(p)), function(i) {
            identical(unique(p[[i]]$data$.by), "o")
          }, logical(1)))
          expect_length(ordered_panels, if (length(grouped)) 1L else 2L)
          for (i in ordered_panels) {
            values <- p[[i]]$data
            expect_false(any(as.character(values$o) == "c"))
            expect_true(is.ordered(values$o))
            ref <- flat[flat$.smooth %in% values$.smooth, ]
            expect_equal(values$.estimate, ref$.estimate)
          }
          if (!length(grouped)) {
            expect_equal(vapply(ordered_panels, function(i) {
              as.character(unique(p[[i]]$data$o))
            }, character(1)), c("a", "b"))
          }
        }
      }
    }
  }
  d <- models$data
  d$o <- ordered(ifelse(d$f == "a", "a", "b"), levels = c("b", "a"))
  m <- mgcv::gam(y ~ s(x, k = 4) + s(x, by = o, k = 4), data = d)
  expect_length(assemble(m, n = 5, grouped = "factor-by"), 2L)
  expect_length(assemble(m, n = 5, grouped = character()), 2L)
  selected <- assemble(models$ordered, select = 3, n = 5, grouped = "factor-by")
  expect_length(selected, 1L)
  expect_equal(unique(selected[[1]]$data$o), ordered("b", levels = levels(models$data$o)))
})

test_that("sz combinations and their rugs are split without changing estimates", {
  m <- factor_control_models()$two
  sm <- smooth_estimates(m, select = 4, n = 6, interval = "simultaneous",
    n_sim = 50, seed = 244)
  for (entry in list(
    function() assemble(m, select = 4, n = 6, grouped = character(),
      interval = "simultaneous", n_sim = 50, seed = 244),
    function() draw(sm, grouped = character()),
    function() draw(smooth_estimates(m, select = 4, n = 6, unnest = FALSE,
      interval = "simultaneous", n_sim = 50, seed = 244), grouped = character()))) {
    p <- entry()
    expect_length(p, 9L)
    for (i in seq_len(length(p))) {
      panel <- p[[i]]
      d <- panel$data
      expect_equal(nrow(unique(d[c("f", "g")])), 1L)
      ref <- sm[sm$f == d$f[1] & sm$g == d$g[1], ]
      expect_equal(d$.estimate, ref$.estimate)
      expect_equal(d$.lower_ci, ref$.lower_ci)
      expect_equal(d$.upper_ci, ref$.upper_ci)
      expect_match(panel$labels$subtitle, "f = .+, g =")
      expect_identical(panel$theme$legend.position, "none")
      rugs <- Filter(function(layer) inherits(layer$geom, "GeomRug"), panel$layers)
      for (rug in rugs) {
        expect_true(all(rug$data$f == d$f[1]))
        expect_true(all(rug$data$g == d$g[1]))
      }
      expect_silent(ggplot2::ggplot_build(panel))
    }
  }
})

test_that("hidden ribbons leave curves, bounds and visible ranges intact", {
  m <- factor_control_models()$mixed
  sm <- smooth_estimates(m, n = 6, interval = "simultaneous", n_sim = 50, seed = 244)
  before <- sm
  hidden <- draw(sm, show_ci = character())
  shown <- draw(sm, show_ci = c("factor-by", "sz", "fs"))
  for (i in seq_len(length(hidden))) {
    expect_equal(hidden[[i]]$data$.estimate, shown[[i]]$data$.estimate)
    expect_equal(hidden[[i]]$data$.lower_ci, shown[[i]]$data$.lower_ci)
    if (factor_control_type(hidden[[i]]) != "ordinary") {
      expect_false(grepl("simultaneous band", hidden[[i]]$labels$caption, fixed = TRUE)[1])
    }
  }
  expect_identical(sm, before)
  p <- assemble(m, select = c(2:6), n = 6, grouped = character(),
    show_ci = character(), constant = 1, fun = exp, scales = "fixed")
  limits <- lapply(p, function(panel) {
    ggplot2::ggplot_build(panel)$layout$panel_scales_y[[1]]$range$range
  })
  expect_true(all(vapply(limits, function(x) isTRUE(all.equal(x, limits[[1]])), logical(1))))
  expected <- smooth_estimates(m, select = 2:6, n = 6)
  expect_equal(limits[[1]], range(exp(expected$.estimate + 1)))
  for (panel in p) expect_true(all(panel$data$.estimate > 0))
  custom <- assemble(m, select = 6, n = 6, grouped = character(),
    title = "Title", subtitle = "Subtitle", xlab = "X", ylab = "Y",
    discrete_colour = ggplot2::scale_colour_manual(values = c("red", "blue", "green")),
    discrete_fill = ggplot2::scale_fill_manual(values = c("pink", "grey", "orange")))
  for (panel in custom) {
    expect_equal(panel$labels$title, "Title")
    expect_equal(panel$labels$subtitle, "Subtitle")
    expect_equal(panel$labels$x, "X")
    expect_equal(panel$labels$y, "Y")
    expect_silent(ggplot2::ggplot_build(panel))
  }
})

test_that("deprecated grouped_by changes only factor-by membership", {
  withr::local_options(lifecycle_verbosity = "warning")
  m <- factor_control_models()$mixed
  sm <- smooth_estimates(m, n = 5)
  for (entry in list(
    function(...) assemble(m, n = 5, rug = FALSE, parametric = FALSE, ...),
    function(...) draw(m, n = 5, rug = FALSE, parametric = FALSE, ...),
    function(...) draw(sm, ...))) {
    for (legacy in c(FALSE, TRUE)) {
      expected <- if (legacy) 8L else 10L
      warnings <- character()
      p <- withCallingHandlers(entry(grouped = character(), grouped_by = legacy),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      expect_length(warnings, 1L)
      expect_match(warnings, "grouped_by.*deprecated")
      expect_length(p, expected)
      expect_warning(p <- entry(grouped_by = legacy), "deprecated")
      expect_length(p, if (legacy) 4L else 6L)
      expect_warning(p <- entry(grouped = c("factor-by", "fs"),
        grouped_by = legacy), "deprecated")
      expect_length(p, if (legacy) 6L else 8L)
    }
    for (bad in list(NA, c(TRUE, FALSE), 1, "TRUE", NULL)) {
      expect_error(entry(grouped_by = bad), "single TRUE or FALSE")
    }
  }
})

test_that("grouped factor-by plots retain palette overrides and all observations", {
  m <- factor_control_models()$both
  for (by in c("o", "f")) {
    ids <- which(vapply(m$smooth, function(sm) identical(sm$by, by), logical(1)))
    colours <- c(a = "red", b = "blue", c = "green")
    fills <- c(a = "pink", b = "grey", c = "orange")
    p <- assemble(m, select = ids, n = 6, grouped = "factor-by",
      residuals = TRUE,
      discrete_colour = ggplot2::scale_colour_manual(values = colours),
      discrete_fill = ggplot2::scale_fill_manual(values = fills))[[1]]
    b <- ggplot2::ggplot_build(p)
    levels <- unique(as.character(p$data[[by]]))
    lines <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
    ribbons <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1)))
    expect_equal(unique(b$data[[lines]]$colour), unname(colours[levels]))
    expect_equal(unique(b$data[[ribbons]]$fill), unname(fills[levels]))
    observations <- m$model[as.character(m$model[[by]]) %in% levels, ]
    rugs <- Filter(function(l) inherits(l$geom, "GeomRug"), p$layers)
    points <- Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers)
    expect_equal(nrow(rugs[[1]]$data), nrow(observations))
    expect_setequal(as.character(rugs[[1]]$data[[by]]), levels)
    expect_equal(nrow(points[[1]]$data), nrow(observations))
    expect_setequal(as.character(points[[1]]$data[[by]]), levels)
  }
})

test_that("factor-by titles preserve colons in covariates and custom labels", {
  d <- factor_control_models()$data
  m <- mgcv::gam(y ~ f + s(I(x + nchar(":f")), by = f, k = 4), data = d)
  expected <- 's(I(x + nchar(":f")))'
  for (entry in list(
    function(...) assemble(m, select = 1:3, n = 5, ...),
    function(...) draw(smooth_estimates(m, n = 5), ...))) {
    for (grouped in list(character(), "factor-by")) {
      p <- entry(grouped = grouped)
      for (i in seq_len(length(p))) expect_equal(p[[i]]$labels$title, expected)
      p <- entry(grouped = grouped, title = "Custom: title", subtitle = "Custom: subtitle")
      for (i in seq_len(length(p))) {
        expect_equal(p[[i]]$labels$title, "Custom: title")
        expect_equal(p[[i]]$labels$subtitle, "Custom: subtitle")
      }
    }
  }
})
