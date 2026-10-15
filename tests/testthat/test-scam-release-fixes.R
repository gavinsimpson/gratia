skip_if_not_installed("scam", minimum_version = "1.2-21")

scam_release_models <- local({
  s <- mgcv::s
  d <- withr::with_seed(426, {
    d <- data.frame(x = runif(180, 0.05, 0.95), z = runif(180),
      by = rep(c(1, 2), 90), fac = factor(rep(c("a", "b"), 90)))
    d$y <- 2 * d$x + 0.4 * d$z + rnorm(180, sd = 0.1)
    d
  })
  models <- lapply(c("mpi", "mpd", "mdcx", "mdcv", "micx", "micv",
    "mifo", "miso"), function(bs) {
    scam::scam(y ~ z + s(x, bs = bs, k = 6), data = d)
  })
  names(models) <- c("mpi", "mpd", "mdcx", "mdcv", "micx", "micv",
    "mifo", "miso")
  models$factor_by <- scam::scam(y ~ fac + s(x, by = fac, bs = "mpi", k = 6),
    data = d)
  models$numeric_by <- scam::scam(y ~ by + s(x, by = by, bs = "mpi", k = 6),
    data = d)
  models$no_intercept <- scam::scam(y ~ 0 + s(x, bs = "mpi", k = 6), data = d)
  models$ordinary <- scam::scam(y ~ s(x, bs = "cr", k = 6), data = d)
  list(models = models, data = d)
})

test_that("SCAM smooths retain the native fitted function on changed grids", {
  reference_data <- scam_release_models$data[seq_len(20), ]
  for (model in scam_release_models$models) {
    for (label in smooths(model)) {
      reference <- predict(model, reference_data, type = "terms", se.fit = TRUE)
      j <- match(label, colnames(reference$fit))
      sm <- get_smooths_by_id(model, match(label, smooths(model)))[[1L]]
      eligible <- if (is_factor_by_smooth(sm)) {
        which(reference_data[[sm$by]] == by_level(sm))
      } else seq_len(nrow(reference_data))
      for (n in c(1, 2, 5, length(eligible))) {
        rows <- head(eligible, n)
        nd <- reference_data[rows, ]
        result <- smooth_estimates(model, select = label, data = nd,
          overall_uncertainty = FALSE)
        expect_equal(result$.estimate, as.numeric(reference$fit[rows, j]),
          tolerance = 1e-9)
        expect_equal(result$.se, as.numeric(reference$se.fit[rows, j]),
          tolerance = 1e-9)
        reverse <- smooth_estimates(model, select = label,
          data = nd[rev(seq_len(nrow(nd))), ], overall_uncertainty = FALSE)
        expect_equal(reverse$.estimate, rev(result$.estimate), tolerance = 1e-9)
        expect_equal(reverse$.se, rev(result$.se), tolerance = 1e-9)
      }
    }
  }
  model <- scam_release_models$models$mpi
  full <- data.frame(x = seq(0.1, 0.9, length.out = 20), z = 0.5)
  restricted <- data.frame(x = seq(0.1, 0.5, length.out = 20), z = 0.5)
  a <- smooth_estimates(model, data = full, overall_uncertainty = FALSE)
  b <- smooth_estimates(model, data = restricted, overall_uncertainty = FALSE)
  expect_equal(a$.estimate[1L], b$.estimate[1L], tolerance = 1e-9)
  expect_equal(a$.se[1L], b$.se[1L], tolerance = 1e-9)
  expect_true(all(diff(a$.estimate) >= 0))
  overall_full <- smooth_estimates(model, data = full, overall_uncertainty = TRUE)
  overall_restricted <- smooth_estimates(model, data = restricted,
    overall_uncertainty = TRUE)
  expect_equal(overall_full$.estimate[1L], overall_restricted$.estimate[1L],
    tolerance = 1e-9)
  expect_equal(overall_full$.se[1L], overall_restricted$.se[1L], tolerance = 1e-9)
  # A selected smooth requires its own predictors, not an unrelated z column.
  expect_equal(smooth_estimates(model, data = restricted["x"],
    overall_uncertainty = FALSE)$.estimate, b$.estimate)
})

test_that("SCAM smooth uncertainty includes the intercept only when requested", {
  nd <- scam_release_models$data[1:8, ]
  for (model in scam_release_models$models[c("mpi", "mifo",
    "no_intercept")]) {
    sm <- model$smooth[[1L]]
    columns <- smooth_coef_indices(sm)
    X <- predict(model, nd, type = "lpmatrix")
    intercept <- which(names(coef(model)) == "(Intercept)")
    for (frequentist in c(FALSE, TRUE)) {
      columns <- smooth_coef_indices(sm)
      V <- if (frequentist) model$Ve.t else model$Vp.t
      expect_equal(get_vcov(model, frequentist = frequentist), V)
      smooth_only <- smooth_estimates(model, data = nd,
        overall_uncertainty = FALSE, frequentist = frequentist)
      overall <- smooth_estimates(model, data = nd,
        overall_uncertainty = TRUE, frequentist = frequentist,
        interval = "confidence")
      expect_equal(overall$.estimate, smooth_only$.estimate)
      G <- X[, columns, drop = FALSE]
      expect_equal(smooth_only$.se,
        sqrt(pmax(0, rowSums((G %*% V[columns, columns, drop = FALSE]) * G))))
      if (length(intercept)) {
        columns <- c(intercept, columns)
        G <- X[, columns, drop = FALSE]
      }
      expected <- sqrt(pmax(0, rowSums((G %*% V[columns, columns, drop = FALSE]) * G)))
      expect_equal(overall$.se, expected)
      expect_equal(overall$.lower_ci, overall$.estimate - qnorm(0.975) * expected)
      expect_equal(overall$.upper_ci, overall$.estimate + qnorm(0.975) * expected)
    }
  }
  model <- scam_release_models$models$mpi
  plain <- smooth_estimates(model, data = nd, overall_uncertainty = FALSE)
  overall <- smooth_estimates(model, data = nd, overall_uncertainty = TRUE)
  expect_gt(max(abs(plain$.se - overall$.se)), 1e-5)
  # Intercept uncertainty alone is insufficient: include its covariance.
  naive <- sqrt(plain$.se^2 + model$Vp.t[1L, 1L])
  expect_gt(max(abs(naive - overall$.se)), 1e-5)
  plots <- draw(model, data = nd, overall_uncertainty = TRUE,
    parametric = FALSE, rug = FALSE, wrap = FALSE)
  expect_equal(plots[[1L]]$data$.se, overall$.se)
})

test_that("SCAM smooths retain missing, repeated and short evaluation rows", {
  for (model in scam_release_models$models[c("mpi", "mifo")]) {
    nd <- scam_release_models$data[c(1, 1, 2, 3, 4), ]
    reference <- smooth_estimates(model, data = nd, overall_uncertainty = FALSE)
    nd$x[3:5] <- NA_real_
    result <- smooth_estimates(model, data = nd, overall_uncertainty = FALSE)
    expect_equal(nrow(result), nrow(nd))
    expect_equal(result$.estimate[1:2], reference$.estimate[1:2])
    expect_equal(result$.se[1:2], reference$.se[1:2])
    expect_true(all(is.na(result$.estimate[3:5])))
    expect_true(all(is.na(result$.se[3:5])))
    nd$x[] <- NA_real_
    empty <- smooth_estimates(model, data = nd)
    expect_equal(nrow(empty), nrow(nd))
    expect_true(all(is.na(empty$.estimate)))
    expect_true(all(is.na(empty$.se)))
  }
})

test_that("SCAM smoothing-parameter uncertainty is rejected consistently", {
  model <- scam_release_models$models$mpi
  nd <- scam_release_models$data[1:2, ]
  message <- paste("Smoothing-parameter uncertainty is not currently supported",
    "for SCAM models. Use unconditional = FALSE.")
  operations <- list(
    function() get_vcov(model, unconditional = TRUE),
    function() get_vcov(model, unconditional = TRUE, frequentist = TRUE),
    function() spline_values(model$smooth[[1L]], data = nd, model = model,
      unconditional = TRUE),
    function() spline_values2(model$smooth[[1L]], data = nd, model = model,
      unconditional = TRUE),
    function() smooth_estimates(model, unconditional = TRUE),
    function() draw(model, unconditional = TRUE),
    function() fitted_values(model, data = nd, unconditional = TRUE),
    function() gaussian_draws(model, n_draws = 2, unconditional = TRUE),
    function() post_draws(model, n_draws = 2, unconditional = TRUE),
    function() fitted_samples(model, n_draws = 2, unconditional = TRUE),
    function() posterior_samples(model, n_draws = 2, unconditional = TRUE),
    function() smooth_samples(model, n_draws = 2, unconditional = TRUE),
    function() derivative_samples(model, focal = "x", n_draws = 2, unconditional = TRUE),
    function() response_derivatives(model, focal = "x", n_sim = 2, unconditional = TRUE),
    function() response_derivatives(model, focal = "x", uncertainty = "delta",
      unconditional = TRUE),
    function() posterior_predictive_plot(model, n = 2, scalable = FALSE,
      unconditional = TRUE),
    function() posterior_predictive_plot(model, n = 2, scalable = TRUE,
      unconditional = TRUE)
  )
  for (operation in operations) expect_error(operation(), message, fixed = TRUE)
  expect_error(post_draws(model, n_draws = 2, method = "user", unconditional = TRUE),
    "unconditional.*not used")
  expect_error(predicted_samples(model, n_draws = 2, unconditional = TRUE),
    "fitted parameters")
})

test_that("SCAM posterior predictive plots report the scalable limitation", {
  model <- scam_release_models$models$mpi
  message <- paste("Scalable posterior predictive plots are not currently available",
    "for SCAM models. Use scalable = FALSE.")
  expect_error(posterior_predictive_plot(model, n = 3, scalable = TRUE, seed = 4),
    message, fixed = TRUE)
  expect_error(posterior_predictive_plot(model, n = 3, scalable_threshold = 1,
    seed = 4), message, fixed = TRUE)
  ordinary <- posterior_predictive_plot(model, n = 3, scalable = FALSE, seed = 4)
  expect_s3_class(ordinary, "ggplot")
  expect_true(all(0:3 %in% ordinary$data$.draw))
  expect_silent(ggplot2::ggplot_build(ordinary))
})
