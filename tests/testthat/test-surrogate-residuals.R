# Independent formula on ordinary probabilities for moderate fitted values.
surrogate_oracle <- function(model, seed) {
  cuts <- c(-Inf, model$family$getTheta(TRUE), Inf)
  eta <- model$linear.predictors
  withr::with_seed(seed, qlogis(runif(length(model$y),
    plogis(cuts[model$y] - eta), plogis(cuts[model$y + 1L] - eta))))
}

test_that("surrogate residuals follow the truncated logistic construction", {
  for (fam in list(mgcv::ocat(R = 4), mgcv::ocat(theta = c(1, 2)))) {
    m <- surrogate_test_fit(family = fam)
    r <- surrogate_residuals(m, seed = 12)
    expect_type(r, "double")
    expect_equal(r, surrogate_oracle(m, 12), tolerance = 1e-12)
    probabilities <- stats::predict(m, type = "response")
    cumulative <- cbind(0, t(apply(probabilities, 1L, cumsum)))
    rows <- seq_along(m$y)
    expected <- withr::with_seed(12, qlogis(runif(length(rows),
      cumulative[cbind(rows, m$y)], cumulative[cbind(rows, m$y + 1L)])))
    expect_equal(r, expected, tolerance = 1e-12)
    expect_equal(r, qlogis(quantile_residuals(m, "pit", seed = 12)),
      tolerance = 1e-12)
    expect_equal(quantile_residuals(m, "quantile", seed = 12),
      qnorm(plogis(r)), tolerance = 1e-12)
    cuts <- c(-Inf, m$family$getTheta(TRUE), Inf)
    s <- r + m$linear.predictors
    expect_true(all(s > cuts[m$y] & s < cuts[m$y + 1L]))
    # The prior weights are likelihood weights, not latent precision weights.
    changed <- m
    changed$prior.weights <- rep(50, length(m$y))
    expect_equal(surrogate_residuals(changed, seed = 12), r)
    expect_identical(diagnostic_type(m), "deviance")
  }
})

test_that("binary outcomes, absent categories and bam fits keep category indices", {
  dat <- surrogate_test_data()
  binary <- dat
  binary$y <- ifelse(dat$y == 1L, 1L, 2L)
  # mgcv 1.9-4 cannot estimate smoothing parameters for ocat(R = 2).
  m <- mgcv::gam(y ~ x + offset(off), data = binary, weights = w,
    family = mgcv::ocat(R = 2), method = "REML")
  expect_equal(surrogate_residuals(m, seed = 1), surrogate_oracle(m, 1))
  absent <- dat[dat$y != 2L, ]
  m <- surrogate_test_fit(absent, mgcv::ocat(theta = c(1, 2)))
  expect_equal(surrogate_residuals(m, seed = 1), surrogate_oracle(m, 1))
  b <- mgcv::bam(y ~ s(x, k = 6) + offset(off), data = dat,
    family = mgcv::ocat(R = 4), weights = w)
  expect_equal(surrogate_residuals(b, seed = 1), surrogate_oracle(b, 1))
})

test_that("extreme tails and close thresholds retain finite residuals", {
  m <- surrogate_test_fit(family = mgcv::ocat(theta = c(1e-9, 2)))
  m$y[1:3] <- c(1, 4, 2)
  m$linear.predictors[1:3] <- c(1000, -1000, 1000)
  r <- surrogate_residuals(m, seed = 12)
  v <- withr::with_seed(12, runif(length(m$y)))
  cuts <- m$family$getTheta(TRUE)
  expect_equal(r[1], qlogis(log(v[1]) +
    plogis(cuts[1] - 1000, log.p = TRUE), log.p = TRUE))
  expect_equal(r[2], qlogis(log1p(-v[2]) +
    plogis(tail(cuts, 1) + 1000, lower.tail = FALSE, log.p = TRUE),
    lower.tail = FALSE, log.p = TRUE))
  expect_true(all(is.finite(r)))
  expect_lt(r[1], -1000)
  expect_gt(r[2], 1000)
  expect_gte(r[3] + 1000, cuts[1])
  expect_lte(r[3] + 1000, cuts[2])
  # Public PITs have rounded endpoints; transforming those loses the tails.
  pit <- quantile_residuals(m, "pit", seed = 12)
  expect_identical(pit[1:2], c(0, 1))
})

test_that("surrogate seeds preserve or advance the caller state as requested", {
  m <- surrogate_test_fit()
  withr::local_preserve_seed()
  set.seed(44)
  before <- .Random.seed
  r <- surrogate_residuals(m, seed = 1)
  expect_identical(.Random.seed, before)
  expect_identical(surrogate_residuals(m, seed = 1), r)
  expect_false(identical(surrogate_residuals(m, seed = 2), r))
  surrogate_residuals(m)
  expect_false(identical(.Random.seed, before))
  rm(".Random.seed", envir = .GlobalEnv)
  surrogate_residuals(m, seed = 1)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("missing rows, subsetting and both offset forms stay aligned", {
  dat <- surrogate_test_data()
  dat$y[c(8, 20)] <- NA
  take <- seq_len(nrow(dat)) > 5L
  fits <- lapply(list(na.omit, na.exclude), function(action)
    mgcv::gam(y ~ s(x, k = 6), data = dat, offset = off,
      weights = w, subset = take, na.action = action,
      family = mgcv::ocat(R = 4), method = "REML"))
  r <- surrogate_residuals(fits[[1]], seed = 2)
  restored <- surrogate_residuals(fits[[2]], seed = 2)
  expect_equal(restored[!is.na(restored)], r)
  expect_identical(which(is.na(restored)), c(3L, 15L))
  expect_equal(r, surrogate_oracle(fits[[1]], 2))
  expect_equal(add_residuals(dat[take, ], fits[[2]], type = "surrogate",
    seed = 2)$.residual, restored)
  expect_equal(add_residuals(dat[take & !is.na(dat$y), ], fits[[2]],
    type = "surrogate", seed = 2)$.residual, r)
  formula_offset <- mgcv::gam(y ~ s(x, k = 6) + offset(off), data = dat,
    weights = w, subset = take, na.action = na.omit,
    family = mgcv::ocat(R = 4), method = "REML")
  expect_equal(surrogate_residuals(formula_offset, seed = 2), r,
    tolerance = 1e-8)
})

test_that("QQ, worm and ordinary panels share logistic residuals and references", {
  m <- surrogate_test_fit()
  r <- surrogate_residuals(m, seed = 21)
  q <- qq_plot(m, type = "surrogate", seed = 21)
  w <- worm_plot(m, type = "surrogate", seed = 21)
  expect_equal(q$data$residuals, sort(r))
  expect_equal(q$data$theoretical, qlogis(ppoints(length(r))))
  expect_equal(q$data$lower,
    qlogis(qbeta(.05, seq_along(r), rev(seq_along(r)))))
  expect_equal(q$data$upper,
    qlogis(qbeta(.95, seq_along(r), rev(seq_along(r)))))
  expect_equal(w$data$residuals, sort(r) - q$data$theoretical)
  expect_equal(w$data$lower, q$data$lower - q$data$theoretical)
  expect_equal(w$data$upper, q$data$upper - q$data$theoretical)
  expect_identical(q$labels$x, "Logistic quantiles")
  expect_identical(q$labels$y, "Surrogate residuals")
  expect_identical(q$labels$subtitle, "Method: logistic")
  expect_equal(q$layers[[1]]$data$slope, 1)
  expect_equal(q$layers[[1]]$data$intercept, 0)
  expect_equal(w$layers[[1]]$data$yintercept, 0)
  expect_equal(scalable_qq_plot(m, type = "surrogate", seed = 21)$data$residuals,
    q$data$residuals)
  expect_equal(scalable_worm_plot(m, type = "surrogate", seed = 21)$data$residuals,
    w$data$residuals)
  for (scalable in c(FALSE, TRUE)) {
    a <- appraise(m, type = "surrogate", seed = 21, scalable = scalable,
      density_method = "histogram")
    expect_equal(a[[1]]$data$residuals, sort(r))
    expect_identical(a[[1]]$labels$x, "Logistic quantiles")
    if (!scalable) expect_equal(a[[2]]$data$residuals, r)
    expect_plot_render(a)
  }
  p <- residuals_linpred_plot(m, type = "surrogate", seed = 21)
  expect_equal(p$data$residuals, r)
  expect_equal(p$data$eta, unname(stats::fitted(m)))
  expect_equal(p$layers[[1]]$data$yintercept, 0)
  expect_identical(p$labels$y, "Surrogate residuals")
  h <- residuals_hist_plot(m, type = "surrogate", seed = 21)
  expect_equal(h$data$residuals, r)
  expect_identical(h$labels$x, "Surrogate residuals")
  cov <- residuals_covariate_plot(m, covariates = "x", type = "surrogate",
    seed = 21, smooth = FALSE)
  expect_equal(cov$data$residuals, r)
  expect_plot_render(cov)
  expect_plot_render(scalable_residuals_covariate_plot(m, covariates = "x",
    type = "surrogate", seed = 21, smooth = FALSE))
  corr <- residual_correlation(m, type = "surrogate", seed = 21, lag_max = 3)
  expect_equal(corr$correlation[2], cor(r[-length(r)], r[-1]))
  expect_plot_render(residual_correlation_plot(m, type = "surrogate", seed = 21))
})

test_that("appraisal shares one unseeded draw across panels", {
  m <- surrogate_test_fit()
  withr::local_preserve_seed()
  for (scalable in c(FALSE, TRUE)) {
    set.seed(5)
    expected <- surrogate_residuals(m)
    expected_state <- .Random.seed
    set.seed(5)
    a <- appraise(m, type = "surrogate", scalable = scalable,
      density_method = "histogram")
    expect_identical(.Random.seed, expected_state)
    expect_equal(a[[1]]$data$residuals, sort(expected))
    if (!scalable) {
      expect_equal(a[[2]]$data$residuals, expected)
      expect_equal(a[[3]]$data$residuals, expected)
    }
  }
  d <- prepare_diagnostic_data(m, "surrogate", seed = 3)
  expect_equal(qq_plot(d)$data$residuals, sort(d$data$residuals))
})

test_that("surrogate plots preserve missing-row alignment", {
  dat <- surrogate_test_data()
  dat$y[c(8, 20)] <- NA
  m <- mgcv::gam(y ~ s(x, k = 6) + offset(off), data = dat,
    family = mgcv::ocat(R = 4), weights = w, na.action = na.exclude,
    method = "REML")
  r <- surrogate_residuals(m, seed = 21)
  retained <- which(!is.na(r))
  for (fun in list(residuals_linpred_plot, residuals_hist_plot)) {
    p <- fun(m, type = "surrogate", seed = 21)
    expect_equal(p$data$residuals, r)
  }
  for (fun in list(qq_plot, scalable_qq_plot)) {
    p <- fun(m, type = "surrogate", seed = 21)
    expect_equal(p$data$residuals, sort(r[retained]))
  }
  d <- prepare_diagnostic_data(m, "surrogate", seed = 21)
  expect_identical(d$data$.row, retained)
  expect_equal(d$data$residuals, r[retained])
  p <- residuals_covariate_plot(m, covariates = "x", type = "surrogate",
    seed = 21, smooth = FALSE)
  expect_equal(p$data$residuals, r[retained])
  expect_equal(p$data$x, dat$x[retained])
  for (scalable in c(FALSE, TRUE)) {
    a <- appraise(m, type = "surrogate", seed = 21, scalable = scalable,
      density_method = "histogram")
    expect_equal(a[[1]]$data$residuals, sort(r[retained]))
    if (!scalable) expect_equal(a[[2]]$data$residuals, r)
    expect_plot_render(a)
  }
})

test_that("surrogate references retain native diagnostic theme settings", {
  m <- surrogate_test_fit()
  settings <- ggplot2::theme(
    geom.gratia.diagnostic.point = ggplot2::element_geom(colour = "purple"),
    geom.gratia.diagnostic.reference = ggplot2::element_geom(colour = "green"),
    geom.gratia.diagnostic.interval = ggplot2::element_geom(fill = "orange"),
    gratia.alpha.diagnostic.interval = .35)
  for (fun in list(qq_plot, worm_plot, scalable_qq_plot, scalable_worm_plot)) {
    p <- fun(m, type = "surrogate", seed = 21) + settings
    layers <- ggplot2::ggplot_build(p)$data
    has_value <- function(field, value) any(vapply(layers, function(layer)
      field %in% names(layer) && all(layer[[field]] == value), logical(1)))
    expect_true(has_value("colour", "purple"))
    expect_true(has_value("colour", "green"))
    expect_true(has_value("fill", "orange"))
    expect_true(has_value("alpha", .35))
  }
})

test_that("unsupported models and reference methods fail explicitly", {
  m <- surrogate_test_fit()
  normal <- mgcv::gam(y ~ x, data = surrogate_test_data())
  expect_error(surrogate_residuals(normal), "mgcv::ocat")
  expect_error(surrogate_residuals(list()), "mgcv::ocat")
  expect_error(surrogate_residuals(m, conditioning = "marginal"), "conditional")
  for (fun in list(qq_plot, worm_plot, appraise, scalable_qq_plot,
    scalable_worm_plot, residuals_linpred_plot, residuals_hist_plot)) {
    expect_error(fun(normal, type = "surrogate"), "mgcv::ocat")
  }
  for (fun in list(qq_plot, worm_plot, appraise, scalable_qq_plot,
    scalable_worm_plot)) {
    for (method in c("normal", "uniform", "simulate", "marginal", "direct"))
      expect_error(fun(m, type = "surrogate", method = method), "method = NULL")
  }
  grouped <- mgcv::gam(cbind(y, rep(1L, nrow(surrogate_test_data()))) ~ x,
    data = surrogate_test_data(), family = mgcv::gfam(list(gaussian())))
  expect_error(surrogate_residuals(grouped), "mgcv::ocat")
})

test_that("dependent marginal checks do not acquire independent reference bands", {
  m <- surrogate_test_fit()
  # Non-Gaussian BAM's GEE correction does not define joint response draws.
  m$AR1.rho <- .4
  q <- qq_plot(m, type = "surrogate", seed = 2)
  expect_false(any(c("lower", "upper") %in% names(q$data)))
  expect_match(q$labels$caption, "bands omitted")
  expect_equal(q$data$theoretical, qlogis(ppoints(length(m$y))))
})

test_that("known-parameter surrogate residuals have the logistic reference", {
  withr::local_seed(359)
  n <- 30000L
  eta <- runif(n, -2, 2)
  cuts <- c(-1, 0, 2)
  y <- findInterval(eta + rlogis(n), cuts) + 1L
  distribution <- distribution_from_family(mgcv::ocat(theta = c(1, 2)),
    eta, rep(1, n), 1)
  r <- distribution_quantile_residuals(distribution, y, "quantile", seed = 12,
    quantile_function = qlogis)
  expect_equal(mean(r), 0, tolerance = .04)
  expect_equal(var(r), pi^2 / 3, tolerance = .09)
  p <- c(.05, .25, .5, .75, .95)
  expect_equal(unname(quantile(r, p)), qlogis(p), tolerance = .08)
})
