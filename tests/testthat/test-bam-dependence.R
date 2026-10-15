make_gee_bam_models <- function() {
  withr::with_seed(38, {
    d <- data.frame(x = seq(-1, 1, length.out = 120), start = FALSE, n = 5)
    d$start[c(1, 61)] <- TRUE
    mu <- exp(.2 + .4 * d$x)
    specs <- list(
      poisson = list(family = poisson(), y = rpois(120, mu)),
      binomial = list(family = binomial(), y = rbinom(120, 5, plogis(d$x)) / 5),
      Gamma = list(family = Gamma(link = "log"), y = rgamma(120, 3, scale = mu / 3)),
      nb = list(family = mgcv::nb(theta = 2), y = rnbinom(120, 2, mu = mu)),
      betar = list(family = mgcv::betar(theta = 12), y = rbeta(120, 4, 8)),
      scat = list(family = mgcv::scat(theta = c(5, .7)), y = d$x + rt(120, 5) * .7),
      tweedie = list(family = mgcv::tw(theta = 1.4),
        y = mgcv::rTweedie(mu, p = 1.4, phi = .6)))
    lapply(names(specs), function(name) {
      spec <- specs[[name]]
      data <- transform(d, y = spec$y)
      model <- mgcv::bam(y ~ s(x, k = 5), family = spec$family,
        data = data, weights = if (name == "binomial") rep(5, 120) else rep(1, 120),
        discrete = TRUE, rho = .4, AR.start = start)
      model
    })
  })
}

test_that("discrete nonGaussian and extended-family BAM retain marginal diagnostics", {
  models <- make_gee_bam_models()
  for (model in models) {
    d <- response_distribution(model)
    expect_false(d$independent)
    expect_false(d$joint_available)
    expect_null(d$rd)
    expect_true(is.function(d$cdf))
    expect_true(is.function(d$qf))
    expect_true(is.function(d$marginal_rd))
    for (type in c("pit", "quantile")) {
      residual <- quantile_residuals(model, type, seed = 19)
      expect_length(residual, 120)
      expect_true(all(is.finite(residual)))
      p <- qq_plot(model, type = type, seed = 19)
      expect_false(any(c("lower", "upper") %in% names(p$data)))
      expect_identical(p$labels$subtitle,
        if (type == "pit") "Method: uniform" else "Method: std. normal")
      expect_identical(p$labels$caption, "bands omitted")
      expect_equal(p$data$residuals, sort(residual))
    }
    for (fun in list(qq_plot, worm_plot, scalable_qq_plot)) {
      p <- fun(model, n_simulate = 3, seed = 19)
      expect_false(any(c("lower", "upper") %in% names(p$data)))
      expect_identical(p$labels$subtitle, "Method: marginal")
      expect_identical(p$labels$caption, "bands omitted")
      expect_silent(ggplot2::ggplot_build(p))
    }
    p <- qq_plot(model, method = "uniform", n_simulate = 3, seed = 19)
    expect_identical(p$labels$subtitle, "Method: marginal")
    expect_identical(p$labels$caption, "bands omitted")
    p <- qq_plot(model, method = "normal")
    expect_false("lower" %in% names(p$data))
    expect_error(qq_plot(model, method = "simulate"), "Joint response")
    expect_error(simulate(model, nsim = 2), "does not define a joint")
    expect_error(posterior_samples(model, n = 2), "does not define a joint")
    expect_error(posterior_predictive_plot(model, n = 2, scalable = TRUE), "does not define a joint")
    # Family augmentation remains independently useful for the marginal law.
    family <- fix_family_rd(fix_family_cdf(fix_family_qf(family(model))))
    expect_true(all(vapply(family[c("rd", "cdf", "qf")], is.function, logical(1))))
  }
})

test_that("BAM standardized and IRLS residual correlations are explicitly approximate", {
  model <- make_gee_bam_models()[[4L]]
  r <- residual_correlation(model, lag_max = 3)
  expect_identical(r$n_pairs, c(120L, 118L, 116L, 114L))
  expect_equal(r$correlation[1], 1)
  expect_match(attr(r, "description"), "Approximate BAM")
  expected <- ordered_lag_correlations(model$std.rsd,
    cumsum(model$model[["(AR.start)"]]), 3)
  expect_equal(r, expected, ignore_attr = TRUE)
  used <- model_used_rows(model)
  mu <- used$fitted.values
  theta <- used$family$getTheta(TRUE)
  working <- (used$y - mu) / sqrt(mu + mu^2 / theta)
  expect_equal(bam_weighted_working_residuals(used), working, tolerance = 1e-8)
  whitened <- ar1_whiten(working, .4, used$model[["(AR.start)"]])
  expected <- ordered_lag_correlations(whitened,
    cumsum(used$model[["(AR.start)"]]), 3)
  expect_equal(residual_correlation(model, type = "working.standardized", lag_max = 3),
    expected, ignore_attr = TRUE)
  p <- residual_correlation_plot(model, type = "working.standardized", lag_max = 3)
  expect_null(p$labels$subtitle)
  expect_match(attr(p$data, "description"), "Approximate BAM")
  expect_false(any(c("lower", "upper") %in% names(p$data)))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("ordered correlations preserve gaps and segment boundaries", {
  values <- c(1, NA, 2, 3, 10, 11, 12)
  out <- ordered_lag_correlations(values, c(1, 1, 1, 1, 2, 2, 2), 5)
  expect_identical(out$n_pairs, c(6L, 3L, 2L, 1L, 0L, 0L))
  expect_true(all(is.na(out$correlation[4:6])))
})
