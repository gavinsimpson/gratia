test_that("gamm4 diagnostic defaults use the full conditional response law", {
  for (model in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson, gamm4_missing)) {
    prepared <- mixed_diagnostic_model(model)
    expect_identical(prepared$conditioning, "conditional")
    native <- as.numeric(fitted(model$mer))
    native <- native[is.finite(native)]
    expect_equal(prepared$fitted.values, native)
    expect_gt(max(abs(prepared$fitted.values -
      model_used_rows(gamm4_gam(model))$fitted.values)), .01)
    expect_equal(quantile_residuals(model, seed = 17),
      quantile_residuals(model, conditioning = "conditional", seed = 17))
    expect_equal(qq_plot(model, seed = 17)$data,
      qq_plot(model, conditioning = "conditional", seed = 17)$data)
    expect_equal(scalable_qq_plot(model, seed = 17)$data,
      scalable_qq_plot(model, conditioning = "conditional", seed = 17)$data)
    expect_equal(worm_plot(model, seed = 17)$data,
      worm_plot(model, conditioning = "conditional", seed = 17)$data)
    expect_equal(appraise(model, seed = 17)[[4]]$data,
      appraise(model, conditioning = "conditional", seed = 17)[[4]]$data)
    expect_equal(simulate(model, nsim = 2, seed = 17),
      simulate(model, nsim = 2, conditioning = "conditional", seed = 17))
    expect_equal(mixed_diagnostic_model(model, "gam_component"), gamm4_gam(model))
    expect_equal(quantile_residuals(model, conditioning = "gam_component", seed = 17),
      quantile_residuals(gamm4_gam(model), seed = 17))
  }
})

test_that("gamm4 posterior defaults preserve fitted-effect uncertainty across plot modes", {
  for (model in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson, gamm4_missing)) {
    raw <- posterior_samples(model, n_draws = 3L, seed = 17)
    expect_equal(raw, posterior_samples(model, n_draws = 3L, seed = 17,
      conditioning = "conditional"))
    observed <- diagnostic_posterior_observed(model)
    expect_equal(observed, diagnostic_posterior_observed(model, conditioning = "conditional"))
    expect_match(attr(observed, "diagnostic_description"), "fixed/random-effect")
    for (scalable in c(FALSE, TRUE)) {
      default <- posterior_predictive_plot(model, n = 3L, seed = 17, scalable = scalable)
      explicit <- posterior_predictive_plot(model, n = 3L, seed = 17,
        conditioning = "conditional", scalable = scalable)
      expect_equal(default$data, explicit$data)
      expect_equal(lapply(default$layers, `[[`, "data"), lapply(explicit$layers, `[[`, "data"))
      expect_equal(default$labels, explicit$labels)
      expect_match(gsub("\n", " ", default$labels$subtitle), "fixed/random-effect")
      appraisal <- appraise(model, ppcheck = TRUE, n_ppc = 3L, seed = 17,
        scalable = scalable)[[3]]
      expect_equal(appraisal$data, default$data)
      expect_equal(lapply(appraisal$layers, `[[`, "data"), lapply(default$layers, `[[`, "data"))
    }
    expect_equal(posterior_samples(model, n_draws = 3L, seed = 17, conditioning = "gam_component"),
      posterior_samples(gamm4_gam(model), n_draws = 3L, seed = 17))
    expect_equal(posterior_predictive_plot(model, n = 3L, seed = 17,
      conditioning = "gam_component")$data,
      posterior_predictive_plot(gamm4_gam(model), n = 3L, seed = 17)$data)
    expect_error(posterior_samples(model, data = gamm4_data[1:3, ], n_draws = 1L), "fitting observations")
    beta <- matrix(coef(model$gam), nrow = 1)
    expect_equal(posterior_samples(model, n_draws = 1L, seed = 17, method = "user", draws = beta,
      conditioning = "gam_component"), posterior_samples(gamm4_gam(model), n_draws = 1L,
        seed = 17, method = "user", draws = beta))
  }
})

test_that("gamm4 rootograms use full fitted effects and retain the explicit GAM option", {
  for (model in list(gamm4_gaussian, gamm4_poisson, gamm4_missing)) {
    plot_data <- rootogram(model)
    expect_equal(plot_data, rootogram(model, conditioning = "conditional"))
    expect_equal(rootogram(model, conditioning = "gam_component"),
      rootogram(gamm4_gam(model)))
    mu <- as.numeric(fitted(model$mer)); mu <- mu[is.finite(mu)]
    if (family_name(model) == "poisson") {
      expect_equal(rootogram(model, 12), rootogram(model, max_count = 12))
      expected <- vapply(plot_data$.bin, function(k) sum(dpois(k, mu)), numeric(1))
    } else {
      edges <- c(plot_data$.bin, tail(plot_data$.bin, 1) + tail(attr(plot_data, "width"), 1))
      weights <- as.numeric(stats::weights(model$mer, type = "prior"))
      expected <- vapply(seq_along(plot_data$.bin), function(i)
        sum(pnorm(edges[i + 1], mu, sigma(model$mer) / sqrt(weights)) -
          pnorm(edges[i], mu, sigma(model$mer) / sqrt(weights))), numeric(1))
    }
    expect_equal(plot_data$.fitted, expected)
    expect_gt(max(abs(plot_data$.fitted - rootogram(gamm4_gam(model))$.fitted)), .01)
    expect_s3_class(draw(plot_data), "ggplot")
  }
})
