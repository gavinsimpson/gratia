test_that("assemble and draw respect parametric term selection", {
  m <- gamm4_gaussian$gam
  d <- m$model
  for (term in c("z", "fac")) {
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
    names(assemble(m, data = d, parametric = TRUE)), c("s(x)", "z", "fac")
  )
  expect_setequal(
    names(assemble(m, data = d, parametric = TRUE, terms = c("z", "fac"))),
    c("s(x)", "z", "fac")
  )
})

test_that("parametric defaults respect smooth selection and explicit overrides", {
  m <- gamm4_gaussian$gam
  d <- m$model

  for (plotter in list(assemble, function(...) draw(..., wrap = FALSE))) {
    expect_named(plotter(m), c("s(x)", "z", "fac"), ignore.order = TRUE)
    expect_named(plotter(m, parametric = NULL), c("s(x)", "z", "fac"),
      ignore.order = TRUE)
    expect_named(plotter(m, parametric = FALSE), "s(x)")
    expect_named(plotter(m, terms = "z"), c("s(x)", "z"))
    for (selection in list("s(x)", 1, TRUE)) {
      expect_named(plotter(m, select = selection), "s(x)")
      expect_named(plotter(m, select = selection, parametric = NULL), "s(x)")
      expect_named(plotter(m, select = selection, parametric = TRUE),
        c("s(x)", "z", "fac"), ignore.order = TRUE)
      expect_named(plotter(m, select = selection, parametric = FALSE), "s(x)")
      expect_named(plotter(m, select = selection, parametric = TRUE, terms = "fac"),
        c("s(x)", "fac"))
    }
  }
  expect_length(draw(m), 3L)
  expect_length(draw(m, select = "s(x)"), 1L)
})

test_that("default plotting handles smooth-only and parametric-only models", {
  smooth_model <- m_1_smooth
  parametric_model <- sampling_linear_model

  for (plotter in list(assemble, function(...) draw(..., wrap = FALSE))) {
    expect_silent(plots <- plotter(smooth_model))
    expect_named(plots, "s(x0)")
    expect_silent(plots <- plotter(parametric_model))
    expect_named(plots, "wt")
  }
})
