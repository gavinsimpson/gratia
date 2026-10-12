test_that("assemble and draw respect parametric term selection", {
  withr::local_seed(42)
  d <- data.frame(x = runif(200), z = runif(200), w = runif(200))
  d$y <- sin(6 * d$x) + d$z - d$w + rnorm(200, sd = 0.3)
  m <- gam(y ~ s(x, k = 6) + z + w, data = d, method = "REML")
  for (term in c("z", "w")) {
    assembled <- assemble(m, data = d, parametric = TRUE, terms = term)
    expect_setequal(names(assembled), c("s(x)", term))
    expect_equal(unique(assembled[[term]]$data$.term), term)
    drawn <- draw(m, data = d, parametric = TRUE, terms = term)
    expect_length(drawn, 2L)
    plot_terms <- unlist(lapply(seq_len(length(drawn)), function(i) {
      unique(drawn[[i]]$data$.term)
    }), use.names = FALSE)
    expect_setequal(plot_terms, c("s(x)", term))
  }
  expect_setequal(
    names(assemble(m, data = d, parametric = TRUE)), c("s(x)", "z", "w")
  )
  expect_setequal(
    names(assemble(m, data = d, parametric = TRUE, terms = c("z", "w"))),
    c("s(x)", "z", "w")
  )
})

test_that("parametric defaults respect smooth selection and explicit overrides", {
  withr::local_seed(43)
  d <- data.frame(x = runif(100), z = runif(100), w = runif(100))
  d$y <- sin(6 * d$x) + d$z - d$w + rnorm(100, sd = 0.3)
  m <- gam(y ~ s(x, k = 6) + z + w, data = d, method = "REML")

  for (plotter in list(assemble, function(...) draw(..., wrap = FALSE))) {
    expect_named(plotter(m), c("s(x)", "z", "w"), ignore.order = TRUE)
    expect_named(plotter(m, parametric = NULL), c("s(x)", "z", "w"),
      ignore.order = TRUE)
    expect_named(plotter(m, parametric = FALSE), "s(x)")
    expect_named(plotter(m, terms = "z"), c("s(x)", "z"))
    for (selection in list("s(x)", 1, TRUE)) {
      expect_named(plotter(m, select = selection), "s(x)")
      expect_named(plotter(m, select = selection, parametric = NULL), "s(x)")
      expect_named(plotter(m, select = selection, parametric = TRUE),
        c("s(x)", "z", "w"), ignore.order = TRUE)
      expect_named(plotter(m, select = selection, parametric = FALSE), "s(x)")
      expect_named(plotter(m, select = selection, parametric = TRUE, terms = "w"),
        c("s(x)", "w"))
    }
  }
  expect_length(draw(m), 3L)
  expect_length(draw(m, select = "s(x)"), 1L)
})

test_that("default plotting handles smooth-only and parametric-only models", {
  withr::local_seed(44)
  d <- data.frame(x = runif(100), z = runif(100))
  d$y <- sin(6 * d$x) + d$z + rnorm(100, sd = 0.3)
  smooth_model <- gam(y ~ s(x, k = 6), data = d, method = "REML")
  parametric_model <- gam(y ~ z, data = d, method = "REML")

  for (plotter in list(assemble, function(...) draw(..., wrap = FALSE))) {
    expect_silent(plots <- plotter(smooth_model))
    expect_named(plots, "s(x)")
    expect_silent(plots <- plotter(parametric_model))
    expect_named(plots, "z")
  }
})
