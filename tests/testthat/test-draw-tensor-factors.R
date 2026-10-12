# Regression tests for factor margins in tensor products (#395).
test_that("two-factor tensors show every combination with its uncertainty", {
  withr::local_pdf(NULL)
  withr::local_seed(395)
  d <- expand.grid(fac1 = factor(letters[1:3]),
    fac2 = ordered(LETTERS[1:4]), replicate = 1:5)
  d$y <- as.numeric(d$fac1) * as.numeric(d$fac2) + rnorm(nrow(d))

  for (constructor in c("ti", "te", "t2")) {
    for (terms in c("fac1, fac2", "fac2, fac1")) {
      form <- as.formula(paste0("y ~ s(fac1, bs = 're') + ",
        "s(fac2, bs = 're') + ", constructor, "(", terms, ", bs = 're')"))
      m <- gam(form, data = d)
      sm <- smooth_estimates(m, select = 3)
      reference <- predict(m, newdata = sm, type = "terms", se.fit = TRUE)
      expect_equal(sm$.estimate, unname(reference$fit[, 3]))
      expect_equal(sm$.se, unname(reference$se.fit[, 3]))
      expect_equal(nrow(sm), 12L)

      for (p in list(draw(m, select = 3, rug = FALSE), draw(sm))) {
        panel <- p[[1]]
        expect_s3_class(panel$layers[[1]]$geom, "GeomPointrange")
        # Rendering, rather than just building, triggers singleton-line messages.
        expect_silent(ggplot2::ggplotGrob(panel))
        b <- ggplot2::ggplot_build(panel)
        expect_equal(nrow(b$data[[1]]), 12L)
        expect_equal(b$data[[1]]$y, panel$data$.estimate)
        expect_equal(b$data[[1]]$ymin, panel$data$.lower_ci)
        expect_equal(b$data[[1]]$ymax, panel$data$.upper_ci)
        vars <- strsplit(terms, ", ", fixed = TRUE)[[1]]
        expect_equal(nrow(b$layout$layout), nlevels(d[[vars[2]]]))
        expect_equal(as.character(b$layout$layout[[vars[2]]]),
          levels(d[[vars[2]]]))
        expect_equal(length(unique(b$data[[1]]$x)), nlevels(d[[vars[1]]]))
      }
    }
  }
})

test_that("mixed tensor margins use the continuous covariate on x in either order", {
  withr::local_pdf(NULL)
  withr::local_seed(396)
  d <- data.frame(x = runif(90), f = factor(rep(letters[1:3], 30)))
  d$y <- sin(4 * d$x) + as.numeric(d$f) + rnorm(90)
  for (constructor in c("ti", "te", "t2")) {
    for (term in c("x, f, bs = c('tp', 're')",
      "f, x, bs = c('re', 'tp')")) {
      m <- gam(as.formula(paste0("y ~ ", constructor, "(", term, ")")), data = d)
      sm <- smooth_estimates(m, n = 8)
      for (p in list(draw(m, n = 8, rug = FALSE), draw(sm))) {
        panel <- p[[1]]
        expect_s3_class(panel$layers[[1]]$geom, "GeomLine")
        expect_silent(ggplot2::ggplotGrob(panel))
        b <- ggplot2::ggplot_build(panel)
        expect_equal(sort(unique(b$data[[1]]$x)), sort(unique(sm$x)))
        expect_equal(as.integer(table(b$data[[1]]$group)), rep(8L, 3))
      }
    }
  }
})

test_that("three-variable T2 terms retain surfaces faceted by a factor", {
  withr::local_pdf(NULL)
  withr::local_seed(412)
  d <- data.frame(week = runif(180, 0, 52), latitude = runif(180),
    species = factor(rep(letters[1:3], 60)))
  d$y <- sin(2 * pi * d$week / 52) +
    d$latitude * as.numeric(d$species) + rnorm(180, sd = 0.2)

  # Cover the T2 specifications in both bird models 2 and 4 without the
  # expensive paper-sized fits or visual snapshots.
  for (full in c(FALSE, TRUE)) {
    m <- gam(y ~ t2(week, latitude, species,
      bs = c("cc", "tp", "re"), k = c(4, 4, 3), full = full),
      data = d, knots = list(week = c(0, 52)), method = "REML")
    sm <- smooth_estimates(m, n_2d = 8, dist = 0)
    expect_silent(model_plot <- draw(m, n_2d = 8, dist = 0, rug = FALSE,
      contour = FALSE))
    expect_silent(estimates_plot <- draw(sm, contour = FALSE))

    for (p in list(model_plot, estimates_plot)) {
      panel <- p[[1]]
      expect_s3_class(panel$layers[[1]]$geom, "GeomRaster")
      expect_silent(ggplot2::ggplotGrob(panel))
      b <- ggplot2::ggplot_build(panel)
      expect_equal(as.character(b$layout$layout$species), levels(d$species))
      expect_equal(as.integer(table(b$data[[1]]$PANEL)), rep(64L, 3))
      expect_equal(sort(unique(b$data[[1]]$x)), sort(unique(sm$week)))
      expect_equal(sort(unique(b$data[[1]]$y)), sort(unique(sm$latitude)))
      expect_equal(panel$data$.estimate, sm$.estimate)
    }
  }
})
