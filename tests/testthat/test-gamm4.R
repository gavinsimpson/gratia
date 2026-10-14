test_that("current gamm4 objects validate for Gaussian and generalized fits", {
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    expect_s3_class(m, "gamm4")
    expect_true(is_gamm4(m))
    expect_true(is.gamm4(m))
    expect_false(is_gamm4(unclass(m)))
    expect_gamm4_equivalent(m, "family_name")
    expect_equal(family(m), family(m$gam))
    expect_equal(link(m)(0.5), link(m$gam)(0.5))
    expect_equal(inv_link(m)(0.5), inv_link(m$gam)(0.5))
  }
  malformed <- gamm4_gaussian
  malformed$mer <- NULL
  expect_false(is_gamm4(malformed))
  expect_error(fitted_values(malformed), "Expected a gamm4 model")
  malformed <- gamm4_gaussian
  malformed$gam <- list()
  expect_false(is_gamm4(malformed))
  expect_error(smooths(malformed), "Expected a gamm4 model")
  expect_error(fixed_effects(list()), "No fixed-effects method")
  expect_error(is_factor_term(list(1), "fac"), "generic list")
  expect_true(is_factor_term(list(terms(gamm4_gaussian$gam)), "fac"))
})

test_that("model utilities dispatch on gamm4 and preserve GAM results", {
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    for (operation in c("basis_size", "smooths", "smooth_dim", "smooth_terms",
      "n_smooths", "term_names", "parametric_terms", "model_vars", "model_terms",
      "n_eta", "dispersion", "fixed_effects", "model_constant", "penalty",
      "model_concurvity", "typical_values", "factor_combos", "data_combos")) {
      expect_gamm4_equivalent(m, operation)
    }
    expect_gamm4_equivalent(m, "lp_matrix", data = gamm4_data[1:8, ])
    expect_gamm4_equivalent(m, "edf")
    expect_gamm4_equivalent(m, "smooth_label", id = 1L)
    expect_gamm4_equivalent(m, "which_smooths", terms = "s(x)")
    expect_gamm4_equivalent(m, "get_smooths_by_id", id = 1L)
    expect_gamm4_equivalent(m, "smooth_coefs", select = "s(x)")
    expect_gamm4_equivalent(m, "term_variables", term = "fac")
    expect_gamm4_equivalent(m, "is_factor_term", term = "fac")
    expect_gamm4_equivalent(m, "data_slice", x = evenly(x, n = 8))
    expect_equal(get_vcov(m), m$gam$Vp)
    expect_equal(get_vcov(m, frequentist = TRUE), m$gam$Ve)
    expect_equal(model_context(m)$formula, formula(m$gam))
    expect_equal(model_context(m)$data, model.frame(m$gam))
    actual <- overview(m)
    expected <- overview(m$gam)
    expect_equal(actual, expected)
  }
})

test_that("gamm4 smooths, effects, intervals and plots work across families", {
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    expect_gamm4_equivalent(m, "smooth_estimates", n = 8)
    expect_gamm4_equivalent(m, "basis", n = 8)
    suppressWarnings(expect_gamm4_equivalent(m, "datagen", smooth = "s(x)", n = 8))
    expect_gamm4_equivalent(m, "partial_residuals", select = "s(x)")
    expect_gamm4_equivalent(m, "parametric_effects", n = 8)
    suppressWarnings(expect_gamm4_equivalent(m, "evaluate_parametric_term", term = "fac"))
    expect_equal(confint(m, parm = "s(x)", n = 8),
      confint(m$gam, parm = "s(x)", n = 8))
    expect_s3_class(draw(m, n = 8, rug = FALSE), "patchwork")
    expect_s3_class(assemble(m, n = 8, rug = FALSE), "assembled_plots")
  }
  expect_gamm4_equivalent(gamm4_by, "difference_smooths", select = "s(x)", n = 8)
  expect_gamm4_equivalent(gamm4_by, "smooth_estimates", n = 8)
  expect_gamm4_equivalent(gamm4_surface, "smooth_estimates", n = 5, dist = 0)
  expect_gamm4_equivalent(gamm4_surface, "partial_derivatives", focal = "x",
    data = data.frame(x = seq(0.1, 0.9, length.out = 8), z = 0.5, fac = "a"))
})

test_that("gamm4 predictions and derivatives describe the population GAM", {
  nd <- gamm4_data[1:8, ]
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    expect_gamm4_equivalent(m, "fitted_values", data = nd)
    expect_gamm4_equivalent(m, "conditional_values", condition = "x", n_vals = 8)
    expect_gamm4_equivalent(m, "conditional_differences", by = "fac",
      condition = "x", n_vals = 8)
    expect_gamm4_equivalent(m, "derivatives", n = 8)
    expect_gamm4_equivalent(m, "response_derivatives", focal = "x", data = nd,
      n_sim = 5, seed = 11)
    expect_gamm4_equivalent(m, "derivative_samples", focal = "x", data = nd,
      n_sim = 5, seed = 11)
    expect_equal(add_fitted(nd, m), add_fitted(nd, m$gam))
    expect_equal(add_residuals(gamm4_data, m), add_residuals(gamm4_data, m$gam))
    component <- m$gam
    component$residuals <- residuals(m$mer, type = "working")
    expect_equal(add_partial_residuals(gamm4_data, m, select = "s(x)"),
      add_partial_residuals(gamm4_data, component, select = "s(x)"))
    # Group effects supplied only via random= are absent from the GAM matrix.
    changed <- nd
    changed$group <- "new group"
    expect_equal(fitted_values(m, data = changed)$.fitted,
      fitted_values(m, data = nd)$.fitted)
    expect_equal(fitted_values(m, data = nd[, setdiff(names(nd), "group")])$.fitted,
      fitted_values(m, data = nd)$.fitted)
  }
  # Population and conditional predictions differ when group modes are nonzero.
  m <- gamm4_gaussian
  population <- predict(m$gam, newdata = gamm4_data)
  conditional <- predict(m$mer)
  expect_gt(max(abs(population - conditional)), 0.1)
})

test_that("Gaussian and user gamm4 posterior draws work and preserve RNG", {
  nd <- gamm4_data[1:8, ]
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    for (operation in c("posterior_samples", "fitted_samples", "predicted_samples")) {
      set.seed(112)
      old_seed <- .Random.seed
      expect_gamm4_equivalent(m, operation, n = 3, data = nd, seed = 13,
        conditioning = if (operation == "posterior_samples") "gam_component" else NULL)
      expect_identical(.Random.seed, old_seed)
    }
    expect_gamm4_equivalent(m, "smooth_samples", select = "s(x)", n = 3,
      n_vals = 8, seed = 13)
    expect_gamm4_equivalent(m, "post_draws", n = 3, seed = 13)
    expect_gamm4_equivalent(m, "generate_draws", n = 3, seed = 13)
    for (engine in c("mgcv", "mvnfast")) {
      a <- withr::with_seed(14, gaussian_draws(m, n = 3, mvn_method = engine))
      b <- withr::with_seed(14, gaussian_draws(m$gam, n = 3, mvn_method = engine))
      expect_equal(a, b)
      expect_equal(dim(a), c(3L, length(coef(m$gam))))
    }
    draws <- rbind(coef(m$gam), coef(m$gam) + 0.01)
    expect_equal(user_draws(m, draws, index = 1:2), draws[, 1:2])
    expect_equal(post_draws(m, n = 2, method = "user", draws = draws), draws)
    expect_gamm4_equivalent(m, "fitted_samples", n = 2, data = nd,
      method = "user", draws = draws)
    expect_gamm4_equivalent(m, "smooth_samples", n = 2, n_vals = 8,
      method = "user", draws = draws)
    expect_error(user_draws(m, draws[, 1:2]), "number of model coefficients")
    expect_error(user_draws(m, as.vector(draws)), "not a matrix")
    expect_error(mh_draws(m, n = 2), "not supported for gamm4")
    for (operation in c("post_draws", "generate_draws", "posterior_samples",
      "fitted_samples", "smooth_samples")) {
      fun <- get(operation, asNamespace("gratia"))
      expect_error(do.call(fun, c(list(m, n = 2, method = "mh"),
        if (operation == "posterior_samples") list(conditioning = "gam_component"))),
        "not supported for gamm4")
    }
  }
})

test_that("actual gamm4 draws tolerate rank-deficient covariance", {
  m <- gamm4_gaussian
  # Retain a fitted gamm4 model; make one covariance direction exactly null.
  m$gam$Vp[nrow(m$gam$Vp), ] <- 0
  m$gam$Vp[, ncol(m$gam$Vp)] <- 0
  expect_warning(a <- post_draws(m, n = 5, seed = 13), "Cholesky decomposition")
  expect_true(all(is.finite(a)))
  expect_equal(a[, ncol(a)], rep(tail(coef(m$gam), 1), 5), ignore_attr = TRUE)
  expect_equal(a, post_draws(m, n = 5, seed = 13, mvn_method = "mgcv"))
  expect_warning(posterior_samples(m, n = 2, data = gamm4_data[1:4, ], seed = 13,
      conditioning = "gam_component"),
    "Cholesky decomposition")
})

test_that("gamm4 diagnostics and simulations retain family-specific behavior", {
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    expect_equal(simulate(m, nsim = 3, seed = 14, conditioning = "gam_component"),
      simulate.gam(m$gam, nsim = 3, seed = 14))
    expect_gamm4_equivalent(m, "quantile_residuals", seed = 14, conditioning = "gam_component")
    expect_s3_class(qq_plot(m, type = "response", method = "simulate", n_simulate = 5, seed = 14), "ggplot")
    expect_s3_class(worm_plot(m, type = "response", method = "simulate", n_simulate = 5, seed = 14), "ggplot")
    expect_s3_class(appraise(m, method = "normal"), "patchwork")
    for (operation in c("residuals_linpred_plot", "observed_fitted_plot", "residuals_hist_plot")) {
      fun <- get(operation, asNamespace("gratia"))
      expect_equal(fun(m, conditioning = "gam_component")$data, fun(m$gam)$data)
    }
    if (family_name(m) == "binomial") {
      expect_error(rootogram(m), "models supported")
    } else {
      expect_equal(rootogram(m, conditioning = "gam_component"), rootogram(m$gam))
    }
  }
})

test_that("gamm4 variance components include smooths and genuine random effects", {
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson, gamm4_surface)) {
    v <- variance_comp(m)
    expected <- as.data.frame(lme4::VarCorr(m$mer))
    take <- is.na(expected$var2)
    expect_s3_class(v, "variance_comp")
    expect_equal(v$.variance, expected$vcov[take])
    expect_equal(v$.std_dev, expected$sdcor[take])
    expect_true(all(is.na(v$.lower_ci)))
    expect_true(all(is.na(v$.upper_ci)))
    expect_true("group:(Intercept)" %in% v$.component)
    expect_true(any(grepl("s\\(x\\)|t2\\(x,z\\)", v$.component)))
    expect_equal(variance_comp(m, rescale = FALSE), v)
    expect_error(variance_comp(m, coverage = 1), "between zero and one")
  }
  expect_warning(v <- get_vcov(gamm4_binomial, unconditional = TRUE),
    "uncertainty not available")
  expect_equal(v, gamm4_binomial$gam$Vp)
  for (type in c("alternative", "unconditional")) {
    expect_warning(e <- edf(gamm4_binomial, type = type), "unavailable")
    expect_equal(e, edf(gamm4_binomial))
    expect_warning(e <- model_edf(gamm4_binomial, type = type), "unavailable")
    expect_equal(e$.edf, sum(gamm4_binomial$gam$edf))
  }
})

test_that("weighted gamm4 fits preserve excluded and subset rows", {
  m <- gamm4_missing
  expect_equal(NROW(model.frame(m$gam)), 158L)
  expect_equal(nrow(fitted_values(m)), 160L)
  expect_true(all(is.na(fitted_values(m)$.fitted[c(8, 14)])))
  for (operation in c("posterior_samples", "fitted_samples", "predicted_samples")) {
    ans <- expect_gamm4_equivalent(m, operation, n = 3, seed = 14,
      conditioning = if (operation == "posterior_samples") "gam_component" else NULL)
    expect_equal(nrow(ans), 480L)
    val <- intersect(c(".fitted", ".response"), names(ans))
    expect_true(all(is.na(ans[ans$.row %in% c(8, 14), val, drop = FALSE])))
  }
  expect_equal(simulate(m, nsim = 3, seed = 14, conditioning = "gam_component"),
    simulate.gam(m$gam, nsim = 3, seed = 14))
  expect_equal(add_residuals(gamm4_na_data[1:160, ], m),
    add_residuals(gamm4_na_data[1:160, ], m$gam))
  nd <- gamm4_data[1:8, ]
  nd$x[3] <- NA_real_
  for (operation in c("fitted_values", "posterior_samples", "fitted_samples", "predicted_samples")) {
    fun <- get(operation, asNamespace("gratia"))
    args <- if (operation == "fitted_values") list() else list(n = 2, seed = 14)
    expect_equal(do.call(fun, c(list(m, data = nd), args,
      if (operation == "posterior_samples") list(conditioning = "gam_component"))),
      do.call(fun, c(list(m$gam, data = nd), args)))
  }
  changed <- nd
  changed$off <- nd$off + 1
  expect_equal(fitted_values(gamm4_poisson, data = changed)$.fitted /
    fitted_values(gamm4_poisson, data = nd)$.fitted,
    replace(rep(exp(1), 8), 3, NA_real_), ignore_attr = TRUE)
})


test_that("gamm4 estimates and user draws agree with prediction algebra", {
  nd <- gamm4_data[1:8, ]
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    X <- predict(m$gam, newdata = nd, type = "lpmatrix")
    beta <- coef(m$gam)
    offset <- if (family_name(m) == "poisson") nd$off else rep(0, 8)
    eta <- drop(X %*% beta) + offset
    fit <- fitted_values(m, data = nd, scale = "link")
    expect_equal(fit$.fitted, eta, ignore_attr = TRUE)
    expect_equal(fit$.se, sqrt(rowSums((X %*% m$gam$Vp) * X)), ignore_attr = TRUE)
    draws <- rbind(beta, beta + 0.05)
    sample <- fitted_samples(m, n = 2, data = nd, method = "user", draws = draws)
    expected <- family(m)$linkinv(sweep(X %*% t(draws), 1L, offset, "+"))
    expect_equal(sample$.fitted, as.vector(expected))
    d <- derivatives(m, select = "s(x)", data = nd, type = "central", eps = 1e-5)
    hi <- lo <- nd
    hi$x <- nd$x + 5e-6
    lo$x <- nd$x - 5e-6
    delta <- (predict(m$gam, newdata = hi, type = "terms")[, "s(x)"] -
      predict(m$gam, newdata = lo, type = "terms")[, "s(x)"]) / 1e-5
    expect_equal(unname(d$.derivative), unname(delta), tolerance = 1e-6)
  }
})

test_that("gamm4 simultaneous intervals and parallel smooth computations agree", {
  m <- gamm4_gaussian
  nd <- gamm4_data[1:8, ]
  set.seed(913)
  rng <- .Random.seed
  expect_gamm4_equivalent(m, "fitted_values", data = nd,
    interval = "simultaneous", n_sim = 100, seed = 14)
  expect_identical(.Random.seed, rng)
  actual <- withr::with_seed(14, derivatives(m, n = 8,
    interval = "simultaneous", n_sim = 100))
  expected <- withr::with_seed(14, derivatives(m$gam, n = 8,
    interval = "simultaneous", n_sim = 100))
  expect_equal(actual, expected)
  # A real process pool exercises serialization of gamm4 model components.
  skip_if_not_installed("mirai")
  skip_if_not_installed("carrier")
  mirai::daemons(2L, autoexit = NA)
  withr::defer(mirai::daemons(0L))
  actual <- smooth_estimates(m, n = 8)
  expect_equal(actual, smooth_estimates(m$gam, n = 8))
})


test_that("gamm4 family-specific utilities keep useful unsupported errors", {
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    expect_false(has_theta(m))
    expect_error(theta(m), "No additional parameters")
    expect_error(nb_theta(m), "Only negative binomial")
  }
  expect_error(posterior_samples(unclass(gamm4_gaussian)), "Don't know how to sample")
  expect_error(fitted_values(unclass(gamm4_gaussian)), "no applicable method")
})


test_that("generalized gamm4 simulations and QQ data remain valid", {
  for (m in list(gamm4_binomial, gamm4_poisson)) {
    expect_equal(diagnostic_weights(m$gam), m$gam$prior.weights)
    qq <- qq_plot(m, type = "response", method = "simulate", n_simulate = 5, seed = 15)
    expect_true(all(c("theoretical", "residuals") %in% names(qq$data)))
    expect_equal(nrow(qq$data), nrow(m$gam$model))
    expect_true(all(is.finite(qq$data$theoretical)))
    sim <- simulate(m, nsim = 3, seed = 15)
    values <- unlist(sim, use.names = FALSE)
    expect_true(all(is.finite(values)))
    expect_true(all(values >= 0))
    expect_true(all(values == floor(values)))
    if (family_name(m) == "binomial") expect_true(all(values <= 1))
  }
})


test_that("generalized gamm4 partial residuals use conditional working residuals", {
  for (m in list(gamm4_gaussian, gamm4_binomial, gamm4_poisson)) {
    conditional <- residuals(m$mer, type = "working")
    partial <- predict(m$gam, type = "terms")[, "s(x)"] +
      conditional * sqrt(m$gam$weights)
    actual <- partial_residuals(m, select = "s(x)")
    expect_equal(actual[["s(x)"]], unname(partial))
    expect_equal(add_residuals(gamm4_data, m, type = "working")$.residual,
      conditional)
    # All residuals already come from the retained mixed-model frame.
    expect_equal(NROW(gamm4_gam(m)$residuals), NROW(model.frame(m$gam)))
  }
  expect_gt(max(abs(gamm4_binomial$gam$residuals -
    residuals(gamm4_binomial$mer, type = "working"))), 0.1)
})
