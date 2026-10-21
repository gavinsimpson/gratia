assemble_gamm_fixture <- local({
  models <- list()
  function(parametric = FALSE) {
    key <- as.character(parametric)
    if (is.null(models[[key]])) {
      d <- withr::with_seed(1053, {
        d <- data.frame(x = runif(160), z = runif(160), a = runif(160),
          g = factor(rep(seq_len(16), each = 10)))
        d$y <- sin(2 * pi * d$x) + d$z^2 + d$a +
          rnorm(16)[d$g] + rnorm(160, sd = .4)
        d
      })
      formula <- if (parametric) y ~ a else
        y ~ a + s(x, bs = "cc", k = 6) + s(z, k = 5)
      models[[key]] <<- mgcv::gamm(formula, random = list(g = ~ 1),
        data = d, knots = list(x = c(0, 1)), method = "REML")
    }
    models[[key]]
  }
})

test_that("assemble gamm returns named plots of the fitted GAM component", {
  m <- assemble_gamm_fixture()
  original <- m
  expect_identical(getS3method("assemble", "gamm"), assemble.gamm)
  plots <- assemble(m, n = 9, rug = FALSE)
  expect_s3_class(plots, "assembled_plots")
  expect_identical(names(plots), c("s(x)", "s(z)", "a"))
  expect_true(all(vapply(plots, inherits, logical(1), "ggplot")))
  drawn <- draw(m, n = 9, rug = FALSE, wrap = FALSE)
  expect_identical(names(drawn), names(plots))
  estimates <- smooth_estimates(m$gam, n = 9)
  for (i in 1:2) {
    expect_equal(plots[[i]]$data$.estimate,
      estimates$.estimate[estimates$.smooth == names(plots)[i]])
    expect_equal(plots[[i]]$data, drawn[[i]]$data)
    expect_equal(plots[[i]]$labels, drawn[[i]]$labels)
  }
  expect_s3_class(draw(m, n = 9, rug = FALSE), "patchwork")
  expect_identical(m, original)
})

test_that("assemble gamm forwards smooth and parametric selection", {
  m <- assemble_gamm_fixture()
  for (selection in list("s(z)", 2, c(FALSE, TRUE))) {
    plots <- assemble(m, select = selection, n = 9)
    expect_identical(names(plots), "s(z)")
    expect_equal(plots[[1]]$data,
      assemble(m$gam, select = "s(z)", n = 9)[[1]]$data)
  }
  expect_identical(names(assemble(m, select = "s(z", partial_match = TRUE,
    parametric = TRUE, terms = "a", n = 9)), c("s(z)", "a"))
  expect_identical(names(assemble(m, parametric = FALSE, n = 9)),
    c("s(x)", "s(z)"))
  parametric <- assemble_gamm_fixture(TRUE)
  expect_identical(names(assemble(parametric, n = 9)), "a")
})

test_that("assemble gamm retains supplied rows and simultaneous intervals", {
  m <- assemble_gamm_fixture()
  data <- data.frame(x = c(.8, .2, .8, .4))
  plots <- assemble(m, select = "s(x)", data = data, n = 2,
    envir = environment(), interval = "simultaneous", n_sim = 100,
    seed = 28, rug = FALSE)
  expected <- smooth_estimates(m$gam, select = "s(x)", data = data,
    interval = "simultaneous", n_sim = 100, seed = 28)
  expect_equal(plots[[1]]$data$x, data$x)
  for (column in c(".estimate", ".lower_ci", ".upper_ci")) {
    expect_equal(plots[[1]]$data[[column]], expected[[column]])
  }
})

test_that("assemble gamm forwards polar coordinates and observation overlays", {
  m <- assemble_gamm_fixture()
  plots <- assemble(m, cyclic = "polar", n = 11, residuals = TRUE,
    scales = "fixed", polar_args = list(r.axis.inside = FALSE))
  expect_s3_class(plots[[1]]$coordinates, "CoordRadial")
  expect_false(plots[[1]]$coordinates$r_axis_inside)
  expect_s3_class(plots[[2]]$coordinates, "CoordCartesian")
  expect_equal(range(plots[[1]]$data$x), c(0, 1))
  expect_equal(plots[[1]]$scales$get_scales("y")$limits,
    plots[[2]]$scales$get_scales("y")$limits)
  point <- Filter(function(layer) inherits(layer$geom, "GeomPoint"),
    plots[[1]]$layers)
  expect_length(point, 1L)
  expect_equal(nrow(point[[1]]$data), nrow(m$gam$model))
  expect_silent(patchwork::patchworkGrob(patchwork::wrap_plots(plots)))
})

test_that("assemble and draw gamm validate model components and controls", {
  for (bad in list(structure(list(), class = "gamm"),
    structure(list(gam = 1), class = "gamm"), structure(1, class = "gamm"))) {
    expect_error(assemble(bad), "valid 'gam' component", fixed = TRUE)
    expect_error(draw(bad), "valid 'gam' component", fixed = TRUE)
  }
  m <- assemble_gamm_fixture()
  for (fun in list(assemble, draw)) {
    expect_error(fun(m, select = "missing"), "Failed to match")
    expect_error(fun(m, cyclic = "polar", polar_args = list(theta = "y")),
      "polar_args")
  }
})
