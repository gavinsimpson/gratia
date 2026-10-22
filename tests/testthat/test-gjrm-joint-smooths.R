skip_if_not_installed("GJRM", minimum_version = "0.2-6")

test_that("joint GJRM smooths use the final equation coefficients and covariance", {
  m <- gjrm_joint_fixture()
  d <- data.frame(x = c(.7, .2, .7, .4), z = c(.8, .3, .1, .6),
    a = .5, group = factor(c("a", "b", "c", "a"), levels = letters[1:9]))
  labels <- smooths(m)
  expect_identical(labels,
    c("s(x)", "s(group)", "s.1(x)", "s(z)", "s.3(z)", "s.4(z)"))
  offset <- 0L
  smooth_index <- 0L
  for (equation in seq_len(5)) {
    g <- m[[paste0("gam", equation)]]
    columns <- offset + seq_len(m[[paste0("X", equation, ".d2")]])
    for (sm in g$smooth) {
      smooth_index <- smooth_index + 1L
      X <- mgcv::PredictMat(sm, d)
      j <- columns[sm$first.para:sm$last.para]
      for (frequentist in c(FALSE, TRUE)) {
        values <- smooth_estimates(m, select = smooth_index, data = d,
          overall_uncertainty = FALSE, frequentist = frequentist)
        V <- if (frequentist) m$Ve else m$Vb
        expect_equal(values$.estimate, drop(X %*% m$coefficients[j]))
        expect_equal(values$.se,
          unname(sqrt(rowSums((X %*% V[j, j, drop = FALSE]) * X))))
        expect_identical(unique(values$.smooth), labels[smooth_index])
      }
    }
    offset <- offset + length(columns)
  }
  # Intermediate GAM coefficients must have no effect on final-fit smooths.
  altered <- m
  for (i in seq_len(5)) {
    altered[[paste0("gam", i)]]$coefficients[] <- 100
    altered[[paste0("gam", i)]]$Vp[] <- 100
  }
  expect_equal(smooth_estimates(altered, data = d),
    smooth_estimates(m, data = d))
})

test_that("joint smooth selection and saved estimates retain equation identity", {
  m <- gjrm_joint_fixture()
  labels <- smooths(m)
  all <- smooth_estimates(m, n = 8)
  expect_s3_class(all, "smooth_estimates")
  expect_identical(unique(all$.smooth), labels)
  expected <- c("Response 1: Mean (mu)", "Response 1: Mean (mu)",
    "Response 2: Mean (mu)", "Response 1: Standard deviation (sigma)",
    "Response 2: Standard deviation (sigma)", "Dependence (theta)")
  expect_identical(vapply(labels, function(label)
    unique(all$.model[all$.smooth == label]), character(1)) |> unname(), expected)
  for (select in list(labels[5], 5, seq_along(labels) == 5L)) {
    selected <- smooth_estimates(m, select = select, n = 8)
    expect_equal(selected, all[all$.smooth == labels[5], names(selected)],
      ignore_attr = TRUE)
    panels <- assemble(m, select = select, n = 8, rug = FALSE)
    expect_identical(names(panels), labels[5])
    expect_identical(panels[[1]]$labels$subtitle, expected[5])
  }
  expect_identical(unique(smooth_estimates(m, select = "s.4", n = 8,
    partial_match = TRUE)$.smooth), labels[6])
  expect_identical(unique(smooth_estimates(m, select = 6:1, n = 8)$.smooth), labels)
  for (unnest in c(TRUE, FALSE)) {
    saved <- smooth_estimates(m, select = 5, n = 8, unnest = unnest)
    expect_identical(draw(saved)[[1]]$labels$subtitle, expected[5])
  }
  for (select in list(0, -1, 1.5, Inf, NA_integer_, NA_character_)) {
    expect_error(smooth_estimates(m, select = select), "positive integers")
    expect_error(assemble(m, select = select), "positive integers")
  }
  expect_error(assemble(m, select = "absent"), "Failed to match")
  expect_error(smooth_estimates(m, select = rep(FALSE, 6)), "No smooths selected")
})

test_that("joint assemble and draw share the established smooth and random effect plots", {
  m <- gjrm_joint_fixture()
  components <- gjrm_gamlss_components(m)
  all <- assemble(m, n = 8, rug = FALSE, parametric = FALSE)
  expect_identical(names(all), smooths(m))
  expect_true(all(vapply(all, inherits, logical(1), "ggplot")))
  local <- assemble(components[[1]], n = 8, rug = FALSE, parametric = FALSE)
  expect_equal(all[[1]]$data, local[[1]]$data)
  expect_equal(all[[2]]$data, local[[2]]$data)
  expect_equal(all[[2]]$layers, local[[2]]$layers)
  expect_equal(draw(m, n = 8, rug = FALSE, parametric = FALSE, wrap = FALSE), all)
  expect_silent(ggplot2::ggplotGrob(all[[2]]))
  wrapped <- draw(m, n = 8, rug = FALSE, ncol = 2)
  expect_s3_class(wrapped, "patchwork")
  expect_length(wrapped, 7L)
  expect_silent(patchwork::patchworkGrob(wrapped))
  expect_identical(names(assemble(m, select = 6, parametric = TRUE, terms = "a",
    n = 8)), c("a", smooths(m)[6]))
  custom <- draw(m, select = 6, subtitle = "My subtitle", caption = FALSE,
    wrap = FALSE, n = 8)[[1]]
  expect_identical(custom$labels$subtitle, "My subtitle")
  expect_warning(assemble(m, select = 1, residuals = TRUE, n = 8),
    "working residuals and working weights")
  expect_warning(corrected <- smooth_estimates(m, select = 1, n = 8,
    unconditional = TRUE), "Covariance corrected for smoothness uncertainty not available")
  expect_equal(corrected, smooth_estimates(m, select = 1, n = 8))
})

test_that("joint plotting dispatch covers binary, mixed and trivariate fits", {
  for (kind in c("binary", "binary_cont", "trivariate")) {
    m <- gjrm_joint_fixture(kind)
    expect_s3_class(m, "gjrm")
    estimates <- smooth_estimates(m, n = 8)
    expect_true(all(is.finite(estimates$.estimate)))
    expect_true(all(is.finite(estimates$.se)))
    plots <- assemble(m, n = 8, parametric = FALSE)
    expect_identical(names(plots), smooths(m))
    expect_silent(patchwork::patchworkGrob(draw(m, n = 8)))
  }
  m <- gjrm_joint_fixture("parametric")
  expect_identical(smooths(m), character())
  expect_identical(names(assemble(m, n = 8)), "a")
  expect_error(smooth_estimates(m), "No smooths")
  for (fun in list(assemble, draw)) {
    result <- withVisible(fun(gjrm_joint_fixture("empty")))
    expect_null(result$value)
    expect_false(result$visible)
  }
})

test_that("ordinal and fixed-margin smooths use the correct final coefficient positions", {
  d <- data.frame(x = c(.2, .7, .4), z = c(.8, .1, .6))
  m <- gjrm_joint_fixture("ordinal")
  thresholds <- m$VC$K1 - 1L
  for (equation in 1:2) {
    sm <- m[[paste0("gam", equation)]]$smooth[[1]]
    j <- if (equation == 1L) thresholds + seq_len(m$X1.d2) else
      thresholds + m$X1.d2 + sm$first.para:sm$last.para
    X <- mgcv::PredictMat(sm, d)
    e <- smooth_estimates(m, select = equation, data = d,
      overall_uncertainty = FALSE)
    expect_equal(e$.estimate, drop(X %*% m$coefficients[j]))
    expect_equal(e$.se, unname(sqrt(rowSums((X %*% m$Vb[j, j]) * X))))
    expect_silent(ggplot2::ggplotGrob(assemble(m, select = equation, n = 8)[[1]]))
  }
  component <- gjrm_gamlss_components(m)[[1]]
  expect_identical(unname(component$coefficients[1]), 0)
  expect_true(all(component$Vp[1, ] == 0))
  expect_true(all(component$Vp[, 1] == 0))
  m <- gjrm_joint_fixture("fixed")
  expect_true(m$VC$fixed.margins)
  expect_length(gjrm_gamlss_components(m), 1L)
  sm <- m$gam5$smooth[[1]]
  X <- mgcv::PredictMat(sm, d)
  j <- sm$first.para:sm$last.para
  e <- smooth_estimates(m, data = d, overall_uncertainty = FALSE)
  expect_equal(e$.estimate, drop(X %*% m$coefficients[j]))
  expect_equal(e$.se, unname(sqrt(rowSums((X %*% m$Vb[j, j]) * X))))
  expect_identical(names(assemble(m, n = 8)), "s(x)")
  expect_silent(patchwork::patchworkGrob(draw(m, n = 8)))
})

test_that("selection smooths use each equation's observed covariates and final fit", {
  m <- gjrm_joint_fixture("selection")
  expect_s3_class(m, "copulaSampleSel")
  expect_lt(nrow(m$gam2$model), nrow(m$gam1$model))
  d <- data.frame(x = c(.3, .7), z = c(.6, .2))
  sm <- m$gam2$smooth[[1]]
  X <- mgcv::PredictMat(sm, d)
  j <- m$X1.d2 + sm$first.para:sm$last.para
  e <- smooth_estimates(m, select = 2, data = d,
    overall_uncertainty = FALSE)
  expect_equal(e$.estimate, drop(X %*% m$coefficients[j]))
  expect_equal(e$.se, unname(sqrt(rowSums((X %*% m$Vb[j, j]) * X))))
  expect_silent(patchwork::patchworkGrob(draw(m, n = 8)))
  expect_error(appraise(m), "bivariate B and trivariate binary T")
  altered <- m
  class(altered$gam1) <- c("scam", class(altered$gam1))
  expect_error(smooth_estimates(altered), "SCAM|scam")
})

test_that("two ordinal responses skip both sets of thresholds when estimating smooths", {
  m <- gjrm_joint_fixture("ordinal_pair")
  thresholds <- m$VC$K1 + m$VC$K2 - 2L
  d <- data.frame(x = c(.2, .7, .4), z = c(.8, .1, .6))
  for (equation in 1:2) {
    sm <- m[[paste0("gam", equation)]]$smooth[[1]]
    start <- thresholds + if (equation == 2L) m$X1.d2 else 0L
    j <- start + seq_len(m[[paste0("X", equation, ".d2")]])
    X <- mgcv::PredictMat(sm, d)
    e <- smooth_estimates(m, select = equation, data = d,
      overall_uncertainty = FALSE)
    expect_equal(e$.estimate, drop(X %*% m$coefficients[j]))
    expect_equal(e$.se, unname(sqrt(rowSums((X %*% m$Vb[j, j]) * X))))
    component <- gjrm_gamlss_components(m)[[equation]]
    expect_identical(unname(component$coefficients[1]), 0)
    expect_true(all(component$Vp[1, ] == 0))
  }
  expect_silent(patchwork::patchworkGrob(draw(m, n = 8)))
})
