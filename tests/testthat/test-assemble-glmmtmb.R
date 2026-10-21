skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")

assemble_tmb_fixture <- local({
  models <- list()
  function(kind = c("mixed", "multiple", "parametric")) {
    kind <- match.arg(kind)
    if (is.null(models[[kind]])) {
      d <- withr::with_seed(1051, {
        d <- data.frame(x = runif(240, -1, 1), w = runif(240, .1, .9),
          g = factor(rep(seq_len(12), each = 20)))
        b <- matrix(rnorm(24), nrow = 12)
        d$y <- 4 + 2 * d$x + sin(2 * pi * d$w) +
          b[d$g, 1] + b[d$g, 2] * d$x + rnorm(240, sd = .5)
        d
      })
      models[[kind]] <<- switch(kind,
        mixed = glmmTMB::glmmTMB(y ~ x + s(w, bs = "cc", k = 6) +
          (1 + x | g), data = d, REML = TRUE),
        multiple = glmmTMB::glmmTMB(y ~ x + s(w, bs = "cc", k = 6),
          dispformula = ~ x, data = d, REML = TRUE),
        parametric = glmmTMB::glmmTMB(y ~ x, data = d)
      )
    }
    models[[kind]]
  }
})

expect_assembled_tmb_equal <- function(assembled, drawn) {
  expect_s3_class(assembled, "assembled_plots")
  expect_identical(names(assembled), names(drawn))
  expect_true(all(vapply(assembled, inherits, logical(1), "ggplot")))
  for (i in seq_along(assembled)) {
    expect_equal(assembled[[i]]$data, drawn[[i]]$data)
    expect_equal(assembled[[i]]$labels, drawn[[i]]$labels)
    expect_equal(vapply(assembled[[i]]$layers, function(x) class(x$geom)[1L],
      character(1)), vapply(drawn[[i]]$layers, function(x) class(x$geom)[1L],
      character(1)))
    expect_equal(ggplot2::ggplot_build(assembled[[i]])$data,
      ggplot2::ggplot_build(drawn[[i]])$data)
  }
}

test_that("assemble dispatches on glmmTMB and retains the complete plot list", {
  expect_true(is.function(utils::getS3method("assemble", "glmmTMB")))
  m <- assemble_tmb_fixture()
  assembled <- assemble(m, n = 9, rug = FALSE)
  drawn <- draw(m, n = 9, rug = FALSE, wrap = FALSE)
  expect_identical(names(assembled), c("s(w)", "(1 | g)", "(x | g)", "x"))
  expect_assembled_tmb_equal(assembled, drawn)
})

test_that("assemble glmmTMB selection distinguishes smooths native and parametric terms", {
  m <- assemble_tmb_fixture()
  for (selection in list("s(w)", c("s(w)", "(x | g)"), c(1, 3))) {
    assembled <- assemble(m, select = selection, n = 9, rug = FALSE)
    drawn <- draw(m, select = selection, n = 9, rug = FALSE, wrap = FALSE)
    expect_assembled_tmb_equal(assembled, drawn)
  }
  expect_identical(names(assemble(m, parametric = FALSE, n = 9, rug = FALSE)),
    c("s(w)", "(1 | g)", "(x | g)"))
  expect_identical(names(assemble(m, select = "(x | g)", parametric = TRUE,
    terms = "x", n = 9, rug = FALSE)), c("(x | g)", "x"))
  expect_identical(names(assemble(m, select = " | g)", partial_match = TRUE)),
    c("(1 | g)", "(x | g)"))
  expect_assembled_tmb_equal(
    assemble(m, select = "s(w)", parametric = TRUE, terms = "x",
      n = 9, rug = FALSE),
    draw(m, select = "s(w)", parametric = TRUE, terms = "x",
      n = 9, rug = FALSE, wrap = FALSE))
})

test_that("assemble glmmTMB uses supplied data for native random lines", {
  m <- assemble_tmb_fixture()
  nd <- data.frame(x = c(-.75, 0, .5))
  selected <- c("(1 | g)", "(x | g)")
  assembled <- assemble(m, select = selected, ranef_lines = TRUE, data = nd)
  drawn <- draw(m, select = selected, ranef_lines = TRUE, data = nd, wrap = FALSE)
  expect_assembled_tmb_equal(assembled, drawn)
  expect_length(assembled, 1L)
  values <- assembled[[1L]]$data
  expect_equal(sort(unique(values$.x)), nd$x)
  native <- glmmTMB::ranef(m, condVar = FALSE)$cond$g
  for (level in rownames(native)) {
    group <- values[values$.group == level, ]
    expect_equal(group$.estimate,
      native[level, "(Intercept)"] + group$.x * native[level, "x"])
  }
  expect_silent(ggplot2::ggplotGrob(assembled[[1L]]))
})

test_that("assemble glmmTMB recovers missing reports without modifying the fit", {
  m <- assemble_tmb_fixture()
  absent <- m
  absent$sdr <- NULL
  expected <- assemble(m, n = 9, rug = FALSE, ranef_lines = TRUE)
  recovered <- assemble(absent, n = 9, rug = FALSE, ranef_lines = TRUE)
  expect_assembled_tmb_equal(recovered, expected)
  expect_null(absent$sdr)
  expect_false(is.null(m$sdr))
})

test_that("assemble and draw glmmTMB validate the same plotting controls", {
  m <- assemble_tmb_fixture()
  message_for <- function(fun, arguments) {
    tryCatch(do.call(fun, c(list(object = m), arguments)),
      error = conditionMessage)
  }
  for (arguments in list(list(residuals = TRUE), list(unconditional = TRUE),
    list(frequentist = TRUE), list(ranef_lines = NA), list(ranef_lines = 1),
    list(ranef_lines = c(TRUE, FALSE)), list(select = "missing"),
    list(terms = "missing"), list(cyclic = "invalid"),
    list(cyclic = "polar", polar_args = list(theta = "y")))) {
    actual <- message_for(assemble, arguments)
    reference <- message_for(draw, arguments)
    expect_type(actual, "character")
    expect_identical(actual, reference)
  }
  expect_error(assemble(m, select = c("(1 | g)", "(x | g)"),
    ranef_lines = TRUE, data = data.frame(w = 0)), "numeric 'x'")
})

test_that("assemble glmmTMB forwards polar coordinates and environment controls", {
  m <- assemble_tmb_fixture()
  args <- list(start = -pi / 2, reverse = "theta", inner.radius = .15)
  supplied_environment <- new.env(parent = environment())
  assembled <- assemble(m, select = "s(w)", cyclic = "polar",
    polar_args = args, n = 9, envir = supplied_environment)
  drawn <- draw(m, select = "s(w)", cyclic = "polar", polar_args = args,
    n = 9, envir = supplied_environment, wrap = FALSE)
  expect_assembled_tmb_equal(assembled, drawn)
  expect_s3_class(assembled[[1L]]$coordinates, "CoordRadial")
  reference <- do.call(ggplot2::coord_radial, args)
  expect_equal(assembled[[1L]]$coordinates$arc, reference$arc)
  expect_equal(assembled[[1L]]$coordinates$inner_radius, reference$inner_radius)
  expect_silent(ggplot2::ggplotGrob(assembled[[1L]]))
  default <- assemble(m, select = "s(w)", n = 9, rug = FALSE)
  expect_s3_class(default[[1L]]$coordinates, "CoordCartesian")
})

test_that("assemble glmmTMB handles multiple predictors and parametric-only fits", {
  m <- assemble_tmb_fixture("multiple")
  assembled <- assemble(m, n = 9, rug = FALSE)
  expect_identical(names(assembled), c("s(w)", "x", "x.1"))
  expect_assembled_tmb_equal(assembled, draw(m, n = 9, rug = FALSE, wrap = FALSE))
  expect_match(assembled[["s(w)"]]$labels$subtitle, "Conditional model")
  expect_identical(assembled[["x.1"]]$labels$title, "x.1")
  dispersion <- assembled[["x.1"]]$data
  expect_equal(dispersion$.partial, dispersion$.value *
    unname(glmmTMB::fixef(m)$disp[["x"]]))
  expect_identical(names(assemble(m, terms = "x.1", n = 9, rug = FALSE)),
    c("s(w)", "x.1"))
  parametric <- assemble_tmb_fixture("parametric")
  expect_assembled_tmb_equal(assemble(parametric, n = 9, rug = FALSE),
    draw(parametric, n = 9, rug = FALSE, wrap = FALSE))
  expect_identical(names(assemble(parametric)), "x")
})
