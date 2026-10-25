# The full suite checks combinations across models, families and plot options.
# Small function checks remain in test-function-core.R and focused test files.
skip_on_cran()

test_that("native smoothing conditionals cover ordinary and extended families", {
  families <- c("poisson", "binomial", "Gamma", "inverse.gaussian", "negbin",
    "Tweedie", "nb", "betar", "scat", "tw", "cnorm", "clog", "cpois", "bcg",
    "ocat", "ziP", "gfam", "gfam_ocat", "quasipoisson", "quasibinomial",
    "gaussian_log", "poisson_sqrt", "binomial_probit", "binomial_matrix",
    "binomial_factor")
  for (name in families) {
    # Every family retains a native numerical reference. Repeat across engines
    # for ordinary, extended, grouped and matrix-response families on CRAN.
    engines <- test_cases_for_cran(c("gam", "bam"),
      if (name %in% c("poisson", "betar", "gfam", "binomial_matrix"))
        c("gam", "bam") else "gam")
    for (engine in engines) {
      if (name == "cpois" && engine == "bam") next
      fit <- smoothing_family_fixture(name, engine,
        if (engine == "gam") "ML" else "fREML",
        censored = !(name == "cpois" && engine == "bam"))$model
      original <- if (is.function(fit$family$getTheta)) fit$family$getTheta() else NULL
      setup <- sampling_smoothing_setup(fit, block_size = 37L)
      expect_identical(setup$conditional_backend, "family", info = paste(name, engine))
      expect_lt(setup$audit$mean_error, 1e-4)
      expect_lt(setup$audit$covariance_error, 1e-3)
      expect_false(setup$audit$dispersion_sampled)
      expect_false(setup$audit$family_parameters$sampled)
      changed <- sampling_smoothing_conditional(setup, setup$log_sp + .15)
      expect_true(all(is.finite(changed$mean)))
      expect_gt(min(eigen(changed$covariance, symmetric = TRUE)$values), 0)
      draws <- sampling_smoothing_draws(setup, n_draws = 3L, seed = 719)
      expect_identical(dim(draws$bs), c(3L, length(coef(fit))))
      expect_identical(colnames(draws$bs), names(coef(fit)))
      if (!is.null(original)) {
        expect_identical(fit$family$getTheta(), original)
        expect_equal(setup$family$getTheta(), original)
      }
    }
  }
})

test_that("non-Gaussian mappings and fixed-sp refits agree across methods", {
  configurations <- list(list(), list(linked = TRUE), list(fixed = TRUE),
    list(tensor = TRUE), list(missing = TRUE, call_offset = TRUE))
  for (engine in c("gam", "bam")) {
    for (method in if (engine == "gam") c("ML", "REML") else c("ML", "REML", "fREML")) {
      cases <- test_cases_for_cran(configurations,
        if ((engine == "gam" && method == "REML") || method == "fREML")
          configurations else configurations[1L])
      for (configuration in cases) {
        fixture <- do.call(smoothing_family_fixture,
          c(list(name = "nb", engine = engine, method = method), configuration))
        fit <- fixture$model
        setup <- sampling_smoothing_setup(fit)
        info <- sampling_smoothing_info(fit)
        prefix <- if (engine == "gam") 1L else 0L
        expect_identical(info$V_sp_layout$family, seq_len(prefix))
        expect_identical(info$V_sp_layout$log_sp, prefix + seq_along(fit$sp))
        expect_equal(setup$log_sp_cov,
          fit$V.sp[info$V_sp_layout$log_sp, info$V_sp_layout$log_sp, drop = FALSE])
        expect_equal(unname(exp(drop(setup$L %*% setup$log_sp) + setup$lsp0)),
          unname(fit$full.sp %||% fit$sp), tolerance = 1e-10)
        rho <- setup$log_sp + .3
        conditional <- sampling_smoothing_conditional(setup, rho)
        # Rebuild the original smooths independently, with the fitted theta fixed.
        refit_data <- fixture$data
        refit_data$.extra_offset <- if (isTRUE(configuration$call_offset))
          refit_data$o else rep(0, nrow(refit_data))
        refit <- mgcv::gam(fixture$formula, data = refit_data, weights = w,
          family = mgcv::nb(theta = fit$family$getTheta(TRUE)), method = "REML",
          sp = if (isTRUE(configuration$fixed)) c(.1, exp(rho)) else exp(rho),
          scale = fit$sig2, offset = .extra_offset, na.action = na.exclude)
        # Rebuilt smooths can use a different coefficient basis in gam and
        # bam. Compare predictions and their joint covariance on common rows.
        evaluation <- fixture$data[stats::complete.cases(fixture$data[c("x", "z")]), ][1:35, ]
        X <- predict(fit, newdata = evaluation, type = "lpmatrix")
        Xr <- predict(refit, newdata = evaluation, type = "lpmatrix")
        expect_equal(drop(X %*% conditional$mean), drop(Xr %*% coef(refit)),
          tolerance = 1e-6)
        expect_equal(unname(X %*% conditional$covariance %*% t(X)),
          unname(Xr %*% vcov(refit) %*% t(Xr)), tolerance = 1e-6)
        fit$call$data <- quote(data_no_longer_available)
        fit$call$weights <- quote(weights_no_longer_available)
        expect_silent(sampling_smoothing_setup(fit))
      }
    }
  }
})

test_that("native updates retain censoring and extended-family parameters", {
  for (name in c("betar", "scat", "tw", "cnorm", "clog", "cpois", "bcg", "ocat", "ziP")) {
    fixture <- smoothing_family_fixture(name)
    fit <- fixture$model
    theta <- fit$family$getTheta()
    family <- switch(name,
      betar = mgcv::betar(theta = fit$family$getTheta(TRUE)),
      scat = mgcv::scat(theta = fit$family$getTheta(TRUE)),
      tw = mgcv::tw(theta = fit$family$getTheta(TRUE)),
      cnorm = mgcv::cnorm(theta = exp(theta)),
      clog = mgcv::clog(theta = exp(theta)), cpois = mgcv::cpois(),
      bcg = {f <- mgcv::bcg(); f$n.theta <- 0L; f$putTheta(theta); f},
      ocat = mgcv::ocat(theta = exp(theta)), ziP = mgcv::ziP(theta = theta, b = .3))
    # Native helpers must work even when mgcv was not attached by the caller.
    family <- sampling_smoothing_family_bindings(family)
    setup <- sampling_smoothing_setup(fit)
    rho <- setup$log_sp + c(.2, -.15)
    conditional <- sampling_smoothing_conditional(setup, rho)
    refit <- mgcv::gam(fixture$formula, data = fixture$data, weights = w,
      family = family, method = "REML", sp = exp(rho), scale = fit$sig2)
    expect_equal(conditional$mean, unname(coef(refit)), tolerance = 1e-6, info = name)
    expect_equal(conditional$covariance, unname(vcov(refit)), tolerance = 1e-6,
      info = name)
    expect_identical(fit$family$getTheta(), theta)
  }
})

test_that("binomial initialisation uses raw responses and trial weights once", {
  proportion <- smoothing_family_fixture("binomial")$model
  matrix <- smoothing_family_fixture("binomial_matrix")$model
  a <- sampling_smoothing_setup(proportion)
  b <- sampling_smoothing_setup(matrix)
  expect_equal(a$beta, b$beta, tolerance = 1e-5)
  expect_equal(a$log_sp, b$log_sp, tolerance = 1e-4)
  expect_equal(sampling_smoothing_conditional(a, a$log_sp + .2),
    sampling_smoothing_conditional(b, a$log_sp + .2), tolerance = 1e-8)
})

test_that("native mixtures are reproducible and retain fixed parameters", {
  setup <- sampling_smoothing_setup(smoothing_family_fixture("betar")$model)
  withr::local_seed(921)
  rng <- .Random.seed
  first <- sampling_smoothing_draws(setup, n_draws = 8L, seed = 610)
  expect_identical(.Random.seed, rng)
  expect_identical(first, sampling_smoothing_draws(setup, n_draws = 8L, seed = 610))
  fixed <- sampling_smoothing_conditional(setup, setup$log_sp)
  zero <- sampling_smoothing_draws(setup, n_draws = 4L, seed = 610,
    log_sp_cov = matrix(0, 2, 2))
  expect_equal(zero$mean_conditional_covariance, fixed$covariance)
  expect_equal(zero$conditional_means[1, ], fixed$mean, ignore_attr = TRUE)
  expect_equal(unname(zero$covariance_conditional_means), matrix(0, 9, 9))
  expect_equal(first$total_covariance, first$mean_conditional_covariance +
    first$covariance_conditional_means)
})

test_that("native conditional failures remain explicit without clipping draws", {
  fit <- smoothing_family_fixture("poisson")$model
  expect_error(sampling_smoothing_setup(fit, dense_limit = 1L), "dense fitting design")
  expect_error(sampling_smoothing_setup(fit, dense_limit = NA), "dense_limit")
  setup <- sampling_smoothing_setup(fit)
  setup$log_sp[] <- 1000
  expect_error(sampling_smoothing_draws(setup, n_draws = 2L, seed = 1),
    "Smoothing draw 1 failed:.*numerically supported range")
  fit$V.sp <- NULL
  expect_error(sampling_smoothing_setup(fit), "covariance and ordering")
})

test_that("copied native family helpers work without attaching mgcv", {
  original <- mgcv::tw()
  env <- environment(original$ls)
  # Isolate the private environment from any mgcv attachment in the test runner.
  parent.env(env) <- new.env(parent = baseenv())
  before <- ls(env, all.names = TRUE)
  copy <- sampling_smoothing_freeze_family(original)
  expect_true(exists("ldTweedie", environment(copy$ls), inherits = FALSE))
  expect_identical(ls(env, all.names = TRUE), before)
  expect_false(identical(environment(copy$ls), env))
  expect_identical(copy$getTheta(), original$getTheta())
})

test_that("unverified native censored-Poisson bam conditionals are rejected", {
  # The installed native fitter can report convergence for a different mode;
  # check agreement instead of relaxing the reconstruction requirement.
  fit <- suppressWarnings(smoothing_family_fixture("cpois", "bam", "fREML")$model)
  expect_error(sampling_smoothing_setup(fit), "does not reproduce")
})

test_that("extended covariance layouts distinguish family and global scales", {
  for (engine in c("gam", "bam")) for (method in
    if (engine == "gam") c("ML", "REML") else c("ML", "REML", "fREML")) {
    fit <- smoothing_family_fixture("tw", engine, method)$model
    info <- sampling_smoothing_info(fit)
    prefix <- if (engine == "gam") 1L else 0L
    expect_identical(info$V_sp_layout$family, seq_len(prefix))
    expect_identical(info$V_sp_layout$log_sp, prefix + 1:2)
    expect_identical(info$V_sp_layout$log_scale,
      if (engine == "bam" && method == "fREML") integer() else prefix + 3L)
    expect_identical(info$sp_vcov_layout$log_scale, prefix + 3L)
    expect_false(info$family_parameters$sampled)
    expect_silent(sampling_smoothing_setup(fit))
  }
  # These families have component/nuisance scales, not a separate global phi.
  for (name in c("nb", "cnorm", "gfam")) {
    fit <- smoothing_family_fixture(name, "bam", "ML")$model
    info <- sampling_smoothing_info(fit)
    expect_length(info$V_sp_layout$family, 0L)
    expect_length(info$V_sp_layout$log_scale, 0L)
    expect_false(info$family_parameters$covariance_includes_family_parameters)
    expect_silent(sampling_smoothing_setup(fit))
  }
})

test_that("reconstruction checks reject inconsistent fitted coefficients", {
  fixture <- smoothing_family_fixture("betar", "bam", "ML")
  fit <- fixture$model
  fit$coefficients[1L] <- fit$coefficients[1L] + .05
  expect_error(sampling_smoothing_setup(fit), "Refit with tighter convergence tolerances")
  expect_silent(sampling_smoothing_setup(fixture$model))
})

test_that("family covariance flags require a retained matrix with known ordering", {
  fixture <- smoothing_family_fixture("nb")
  fit <- mgcv::gam(fixture$formula, data = fixture$data, weights = w,
    family = mgcv::nb(), method = "REML", optimizer = "efs")
  info <- sampling_smoothing_info(fit)
  expect_null(fit$V.sp)
  expect_null(info$V_sp_layout)
  expect_false(info$family_parameters$covariance_includes_family_parameters)
  info <- sampling_smoothing_info(fixture$model)
  expect_true(info$family_parameters$covariance_includes_family_parameters)
  fit <- fixture$model
  fit$V.sp <- NULL
  expect_false(sampling_smoothing_info(fit)$family_parameters$covariance_includes_family_parameters)
  fit$V.sp <- matrix(0, 2, 3)
  expect_false(sampling_smoothing_info(fit)$family_parameters$covariance_includes_family_parameters)
})
