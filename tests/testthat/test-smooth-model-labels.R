subtitle_data <- function() {
  withr::with_seed(821, {
    d <- data.frame(x = runif(240), z = runif(240),
      fac = factor(rep(c("a", "b"), 120)))
    d$y <- sin(6 * d$x) + rnorm(nrow(d), sd = exp(.4 * d$z))
    d
  })
}

plot_subtitles <- function(plots) {
  vapply(plots, function(p) p$labels$subtitle, character(1)) |> unname()
}

test_that("distributional smooth labels survive selection and saved estimates", {
  d <- subtitle_data()
  m <- gam(list(y ~ s(x, k = 5), ~ s(x, k = 5)),
    family = gaulss(), data = d)
  expected <- c("Mean (mu)", "Standard deviation (sigma)")
  plots <- draw(m, wrap = FALSE)
  expect_identical(plot_subtitles(plots), expected)
  expect_identical(names(plots), smooths(m))
  for (select in list(2, c(FALSE, TRUE), "s.1(x)")) {
    p <- draw(m, select = select, wrap = FALSE)
    expect_identical(plot_subtitles(p), expected[2])
    expect_identical(names(p), "s.1(x)")
  }
  expect_identical(plot_subtitles(draw(m, select = "s.1", partial_match = TRUE,
    wrap = FALSE)), expected[2])
  for (unnest in c(TRUE, FALSE)) {
    e <- smooth_estimates(m, unnest = unnest)
    expect_identical(unique(e$.model), expected)
    p <- draw(e)
    expect_identical(vapply(seq_len(length(p)), function(i)
      p[[i]]$labels$subtitle, character(1)), expected)
  }
  # Predictor one has no smooths; the only smooth still belongs to sigma.
  m <- gam(list(y ~ 1, ~ s(x, k = 5)), family = gaulss(), data = d)
  expect_identical(plot_subtitles(draw(m, wrap = FALSE)), expected[2])
  # Membership must not depend on the public smooth label.
  m$smooth[[1]]$label <- "arbitrary label"
  expect_identical(smooth_model_labels(m, m$smooth), expected[2])
})

test_that("model subtitles coexist with by labels and respect user labels", {
  d <- subtitle_data()
  m <- gam(list(y ~ fac + s(x, by = fac, k = 5), ~ s(z, k = 5)),
    family = gaulss(), data = d)
  expect_identical(plot_subtitles(draw(m, parametric = FALSE, wrap = FALSE)),
    c("Mean (mu); By: fac; a", "Mean (mu); By: fac; b",
      "Standard deviation (sigma)"))
  expected <- c("Mean (mu); By: fac", "Standard deviation (sigma)")
  expect_identical(plot_subtitles(draw(m, grouped_by = TRUE,
    parametric = FALSE, wrap = FALSE)), expected)
  p <- draw(smooth_estimates(m), grouped_by = TRUE)
  expect_identical(vapply(seq_len(length(p)), function(i)
    p[[i]]$labels$subtitle, character(1)), expected)
  for (subtitle in list("Custom subtitle", "", expression(sigma))) {
    p <- draw(m, select = 3, subtitle = subtitle, title = "Custom title",
      xlab = "Covariate", ylab = "Effect", wrap = FALSE)[[1]]
    expect_identical(p$labels$subtitle, subtitle)
    expect_identical(p$labels$title, "Custom title")
    expect_identical(p$labels$x, "Covariate")
    expect_identical(p$labels$y, "Effect")
  }
  expect_identical(draw(m, select = 3, subtitle = "Mine")[[1]]$labels$subtitle,
    "Mine")
  expect_identical(draw(smooth_estimates(m, select = 3),
    subtitle = "Saved estimates")[[1]]$labels$subtitle, "Saved estimates")
  plain <- gam(y ~ s(x, by = fac, k = 5), data = d)
  expect_false(".model" %in% names(smooth_estimates(plain)))
  expect_identical(plot_subtitles(draw(plain, wrap = FALSE)),
    c("By: fac; a", "By: fac; b"))
  plain <- gam(y ~ s(x, k = 5) + s(z, k = 5), data = d)
  expect_true(all(vapply(draw(plain, wrap = FALSE),
    function(p) is.null(p$labels$subtitle), logical(1))))
  expect_null(draw(smooth_estimates(plain))[[1]]$labels$subtitle)
  expect_identical(draw(plain, select = 1, subtitle = "Mine",
    wrap = FALSE)[[1]]$labels$subtitle, "Mine")
})

test_that("surfaces, shared smooths and unknown families retain predictor identity", {
  d <- subtitle_data()
  m <- gam(list(y ~ s(x, z, k = 8), ~ s(z, k = 5)),
    family = gaulss(), data = d)
  expect_identical(plot_subtitles(draw(m, n_2d = 5, wrap = FALSE)),
    c("Mean (mu)", "Standard deviation (sigma)"))
  m$family$family <- "custom distributional family"
  expect_identical(smooth_model_labels(m, m$smooth),
    c("Linear predictor 1", "Linear predictor 2"))
  # mgcv supports sharing coefficients across linear predictors.
  d$y <- withr::with_seed(13, sample(0:2, nrow(d), replace = TRUE))
  m <- gam(list(y ~ 1, ~ 1, 1 + 2 ~ s(x, k = 5) - 1),
    family = multinom(K = 2), data = d)
  expect_identical(plot_subtitles(draw(m, wrap = FALSE)),
    "Category 1 (log odds); Category 2 (log odds)")
  d$y2 <- withr::with_seed(19, cos(6 * d$z) + rnorm(nrow(d)))
  m <- gam(list(y ~ s(x, k = 4), y2 ~ s(z, k = 4)),
    family = mvn(d = 2), data = d)
  expect_identical(plot_subtitles(draw(m, wrap = FALSE)),
    c("Mean (response 1)", "Mean (response 2)"))
})

test_that("Tweedie and hurdle smooths identify the actual parameter order", {
  d <- subtitle_data()
  d$y <- withr::with_seed(14, mgcv::rTweedie(exp(d$x), p = 1.5, phi = .8))
  m <- gam(list(y ~ s(x, k = 4), ~ s(z, k = 4), ~ s(x, k = 4)),
    family = twlss(), data = d, method = "REML")
  expect_identical(plot_subtitles(draw(m, wrap = FALSE)),
    c("Mean (mu)", "Power (p)", "Dispersion (phi)"))
  d$y <- withr::with_seed(15, rpois(nrow(d), exp(d$x)))
  m <- gam(list(y ~ s(x, k = 4), ~ s(z, k = 4)),
    family = ziplss(), data = d)
  expect_identical(plot_subtitles(draw(m, wrap = FALSE)),
    c("Poisson mean (mu)", "Presence probability"))
})

test_that("glmmTMB smooths use the agreed component subtitles", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  d <- subtitle_data()
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 5), dispformula = ~s(z, k = 4),
    data = d, REML = TRUE)
  expect_identical(plot_subtitles(draw(m, wrap = FALSE)),
    c("Conditional model", "Dispersion model"))
  expect_identical(draw(m, select = 2, wrap = FALSE)[[1]]$labels$subtitle,
    "Dispersion model")
  p <- draw(smooth_estimates(m))
  expect_identical(p[[2]]$labels$subtitle, "Dispersion model")
  d$y <- withr::with_seed(17, rpois(nrow(d), exp(d$x)) *
    rbinom(nrow(d), 1, .65))
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 4), ziformula = ~s(z, k = 4),
    family = poisson(), data = d)
  expect_identical(plot_subtitles(draw(m, wrap = FALSE)),
    c("Conditional model", "Zero-inflation model"))
  expect_identical(draw(m, select = 2, subtitle = "Mine", wrap = FALSE)[[1]]$labels$subtitle,
    "Mine")
  # Intercept-only zero-inflation still defines a second predictor.
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 4), ziformula = ~1,
    family = poisson(), data = d)
  expect_identical(draw(m, wrap = FALSE)[[1]]$labels$subtitle,
    "Conditional model")
  # A parametric-only dispersion predictor also triggers the label.
  m <- glmmTMB::glmmTMB(y ~ s(x, k = 4), dispformula = ~z,
    family = glmmTMB::nbinom2(), data = d)
  expect_identical(draw(m, wrap = FALSE)[[1]]$labels$subtitle,
    "Conditional model")
})

test_that("single-predictor glmmTMB smooths omit model subtitles", {
  skip_if_not_installed("glmmTMB", minimum_version = "1.1.15.2")
  d <- subtitle_data()
  for (fam in list(gaussian(), poisson(), glmmTMB::nbinom2())) {
    if (fam$family != "gaussian") {
      d$y <- withr::with_seed(29, rnbinom(nrow(d), mu = exp(d$x), size = 2))
    }
    m <- glmmTMB::glmmTMB(y ~ s(x, k = 4) + s(z, k = 4),
      family = fam, data = d)
    expect_false(".model" %in% names(smooth_estimates(m)))
    expect_true(all(vapply(draw(m, wrap = FALSE),
      function(p) is.null(p$labels$subtitle), logical(1))))
    expect_null(draw(smooth_estimates(m))[[1]]$labels$subtitle)
    expect_identical(draw(m, select = 1, subtitle = "Mine",
      wrap = FALSE)[[1]]$labels$subtitle, "Mine")
  }
})

test_that("GJRM embedded GAMs use parent distribution parameters", {
  skip_if_not_installed("GJRM", minimum_version = "0.2-6")
  d <- subtitle_data()
  single <- GJRM::gamlss(list(y ~ s(x, k = 5) + s(z, k = 5)), data = d)
  p <- draw(single, parametric = FALSE)
  expect_length(p, 2L)
  expect_null(p[[1]]$labels$subtitle)
  expect_null(p[[2]]$labels$subtitle)
  expect_identical(draw(single, subtitle = "Mine",
    parametric = FALSE)[[1]]$labels$subtitle, "Mine")
  m <- GJRM::gamlss(list(y ~ s(x, k = 5), ~ s(x, k = 5)), data = d)
  p <- draw(m, parametric = FALSE)
  expect_length(p, 2L)
  expect_identical(p[[1]]$labels$title, "s(x)")
  expect_identical(p[[2]]$labels$title, "s(x)")
  expect_identical(p[[1]]$labels$subtitle, "Mean (mu)")
  expect_identical(p[[2]]$labels$subtitle, "Standard deviation (sigma)")
  p <- draw(m, subtitle = "Mine", parametric = FALSE)
  expect_identical(p[[2]]$labels$subtitle, "Mine")
  # A parametric-only first equation must not shift the second equation's label.
  m <- GJRM::gamlss(list(y ~ x, ~ s(z, k = 5)), data = d)
  expect_identical(draw(m, parametric = FALSE)[[1]]$labels$subtitle,
    "Standard deviation (sigma)")
  d$y <- withr::with_seed(18, rweibull(nrow(d), shape = 2, scale = exp(d$x)))
  distribution_arg <- if ("family" %in% names(formals(GJRM::gamlss))) {
    "family"
  } else "margin"
  m <- do.call(GJRM::gamlss, c(list(
    formula = list(y ~ s(x, k = 4), ~ s(z, k = 4)), data = d),
    setNames(list("WEI"), distribution_arg)))
  p <- draw(m, parametric = FALSE)
  expect_identical(p[[1]]$labels$subtitle, "Scale (mu)")
  expect_identical(p[[2]]$labels$subtitle, "Shape (sigma)")
  d$y <- withr::with_seed(14, mgcv::rTweedie(exp(d$x), p = 1.5, phi = .8))
  m <- do.call(GJRM::gamlss, c(list(formula = list(y ~ s(x, k = 4),
    ~ s(z, k = 4), ~ s(x, k = 4)), data = d),
    setNames(list("TW"), distribution_arg)))
  p <- draw(m, parametric = FALSE)
  expect_identical(vapply(seq_len(length(p)), function(i)
    p[[i]]$labels$subtitle, character(1)),
    c("Mean (mu)", "Dispersion (sigma)", "Power (nu)"))
})
