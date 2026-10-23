test_that("term intervals use joint coefficient draws and posterior quantiles", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$y <- 2 * d$x + 2 * d$z + sin(6 * d$w) + rnorm(nrow(d), sd = 0.6)
  for (fit in list(gam, bam)) {
    m <- fit(y ~ x + z + fac * poly(w, 2) + s(w, k = 6) + offset(off),
      data = d, method = "REML")
    got <- variance_comp(m, type = "terms", n_draws = 300, seed = 42,
      coverage = 0.8)
    expected <- reference_term_variance_draws(m, 300, 42)
    expect_equal(attr(got, "variance_draws"), expected, ignore_attr = TRUE)
    bounds <- apply(sqrt(expected), 2L, quantile, probs = c(0.1, 0.9))
    expect_equal(got$.lower_ci, unname(bounds[1L, ]))
    expect_equal(got$.upper_ci, unname(bounds[2L, ]))
    expect_lt(cor(attr(got, "variance_draws"))['x', 'z'], -0.5)
    point <- variance_comp(m, type = "terms", n_draws = 0)
    expect_equal(got$.variance, point$.variance)
    expect_equal(attr(got, "covariance"), attr(point, "covariance"))
    expect_equal(attr(got, "coverage"), 0.8)
    expect_true(all(got$.upper_ci >= got$.lower_ci))
  }
})

test_that("term interval controls are reproducible and validate inputs", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- gam(y ~ x + s(w, k = 6), data = d, method = "REML")
  state <- .Random.seed
  a <- variance_comp(m, type = "terms", n_draws = 80, seed = 14)
  expect_identical(.Random.seed, state)
  expect_identical(a, variance_comp(m, type = "terms", n_draws = 80, seed = 14))
  narrower <- variance_comp(m, type = "terms", n_draws = 80, seed = 14,
    coverage = 0.5)
  expect_equal(attr(narrower, "variance_draws"), attr(a, "variance_draws"))
  expect_true(all(narrower$.lower_ci >= a$.lower_ci))
  expect_true(all(narrower$.upper_ci <= a$.upper_ci))
  corrected <- variance_comp(m, type = "terms", n_draws = 80, seed = 14,
    unconditional = TRUE)
  expect_equal(attr(corrected, "variance_draws"),
    reference_term_variance_draws(m, 80, 14, unconditional = TRUE),
    ignore_attr = TRUE)
  expect_false(isTRUE(all.equal(attr(a, "variance_draws"),
    attr(corrected, "variance_draws"))))
  default <- variance_comp(m, type = "terms", seed = 14)
  expect_equal(nrow(attr(default, "variance_draws")), 1000L)
  for (bad in list(-1, 1, 1.5, NA_real_, Inf, c(2, 3), "10")) {
    expect_error(variance_comp(m, type = "terms", n_draws = bad), "n_draws")
  }
  expect_error(variance_comp(m, type = "terms", coverage = 1), "coverage")
  expect_error(variance_comp(m, type = "terms", unconditional = NA), "unconditional")
  m <- gam(y ~ 1, data = d)
  empty <- variance_comp(m, type = "terms", seed = 14)
  expect_equal(empty$.component, "total")
  expect_equal(empty$.variance, 0)
  expect_equal(empty$.lower_ci, 0)
  expect_equal(empty$.upper_ci, 0)
  expect_equal(unname(attr(empty, "variance_draws")), matrix(0, 1000, 1))
})

test_that("term draws retain fitting rows, whole smooths and multiple predictors", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$x[c(5, 40)] <- NA_real_
  models <- list(
    bam(y ~ x + s(w, k = 6), data = d, subset = z > 0.1,
      na.action = na.exclude, discrete = TRUE),
    gam(list(y ~ s(w, k = 6), ~x + z), data = d, family = gaulss()),
    gamm(y ~ te(x, w, k = c(4, 4)), data = d),
    gamm4::gamm4(y ~ t2(x, w, k = c(4, 4)), data = d)
  )
  for (m in models) {
    g <- if (inherits(m, "gam")) m else m$gam
    got <- variance_comp(m, type = "terms", n_draws = 40, seed = 13)
    expect_equal(attr(got, "variance_draws"),
      reference_term_variance_draws(g, 40, 13), ignore_attr = TRUE)
    expect_true(all(is.finite(got$.lower_ci)))
    expect_true(all(is.finite(got$.upper_ci)))
  }
})

# A full Gaussian posterior is an independent reference for covariance between
# GAM coefficients and additional fitted random effects. Include a smooth in
# the fit, then condition on its coefficients through the GAM covariance.
expect_joint_random_draws <- function(m, gam, native) {
  X <- predict_model(gam, type = "lpmatrix")
  Z <- as.matrix(native$design)
  G <- tcrossprod(as.matrix(native$root))
  R <- as.matrix(native$precision)
  # Conditioning a normal prior through the observation covariance remains
  # valid when a fitted random-effect covariance is singular.
  response_cov <- as.matrix(solve(R) + Z %*% G %*% t(Z))
  conditional <- G - G %*% t(Z) %*% solve(response_cov, Z %*% G)
  A <- -conditional %*% as.matrix(crossprod(Z, R %*% X))
  V <- gam$Vp
  expected_cross <- V %*% t(A)
  expected_random <- conditional + A %*% V %*% t(A)
  if (!inherits(m, "gamm")) {
    H <- X + Z %*% A
    predictor_cov <- H %*% V %*% t(H) + Z %*% conditional %*% t(Z)
    fixed <- lme4::getME(m$mer, "X")
    random <- lme4::getME(m$mer, "Z") %*% lme4::getME(m$mer, "Lambda") *
      sqrt(gam$sig2)
    full_design <- as.matrix(cbind(fixed, random))
    penalty <- diag(c(rep(0, ncol(fixed)), rep(1, ncol(random))))
    # Align the native fitting rows with those of the GAM component.
    rows <- match(rownames(gam$model), rownames(stats::model.frame(m$mer)))
    full_design <- full_design[rows, , drop = FALSE]
    precision <- crossprod(full_design, R %*% full_design) + penalty
    reference <- full_design %*% solve(precision, t(full_design))
    expect_equal(predictor_cov, reference, tolerance = 1e-7,
      ignore_attr = TRUE)
  }
  set.seed(15)
  delta <- t(mgcv::rmvn(12000, rep(0, ncol(X)), V))
  joint <- variance_terms_random_draws(m, gam, X, delta)
  # Put the per-term blocks back in their original coefficient order.
  indices <- unlist(native$columns, use.names = FALSE)
  random <- as.matrix(do.call(rbind, joint$perturbations))[order(indices), , drop = FALSE]
  actual <- cov(t(rbind(delta, random)))
  p <- ncol(X)
  q <- ncol(Z)
  cross <- actual[seq_len(p), p + seq_len(q), drop = FALSE]
  random_cov <- actual[p + seq_len(q), p + seq_len(q), drop = FALSE]
  scale <- sqrt(outer(diag(V), diag(expected_random)))
  expect_lt(max(abs(cross - expected_cross) / scale), 0.06)
  scale <- sqrt(outer(diag(expected_random), diag(expected_random)))
  expect_lt(max(abs(random_cov - expected_random) / scale), 0.06)
  # At least one covariance must be large enough to detect independent draws.
  expect_gt(max(abs(expected_cross) / sqrt(outer(diag(V), diag(expected_random)))), 0.1)
}

test_that("mixed term intervals preserve GAM and native random-effect covariance", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d <- d[sample(nrow(d)), ]
  d$y <- d$y - d$off
  d$x[c(4, 40)] <- NA_real_
  models <- list(
    gamm(y ~ x + z + fac + s(w, k = 6), random = list(group = ~1 + z),
      data = d, subset = z > 0.1, na.action = na.exclude, method = "REML"),
    gamm4::gamm4(y ~ x + z + fac + s(w, k = 6), random = ~(1 + z | group),
      data = d, subset = z > 0.1, na.action = na.exclude)
  )
  for (m in models) {
    g <- model_used_rows(m$gam)
    native <- if (inherits(m, "gamm")) variance_terms_lme_design(m, g) else
      variance_terms_mer_design(m, g)
    expect_joint_random_draws(m, g, native)
    got <- variance_comp(m, type = "terms", n_draws = 100, seed = 13)
    expect_true(all(is.finite(got$.lower_ci)))
    expect_true(all(is.finite(got$.upper_ci)))
    expect_equal(colnames(attr(got, "variance_draws")), got$.component)
    expect_identical(got, variance_comp(m, type = "terms", n_draws = 100, seed = 13))
  }
})

test_that("generalized and nested mixed models have posterior term intervals", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$count <- rpois(nrow(d), exp(0.3 * d$x + sin(6 * d$w) +
    rnorm(12, sd = 0.5)[d$group]))
  d$nested <- factor(rep(1:2, each = 10, length.out = nrow(d)))
  models <- list(
    gamm(count ~ x + s(w, k = 6), random = list(group = ~1, nested = ~1),
      family = poisson(), data = d, verbosePQL = FALSE),
    gamm4::gamm4(count ~ x + s(w, k = 6), random = ~(1 | group) + (0 + z | group),
      family = poisson(), data = d)
  )
  for (m in models) {
    got <- variance_comp(m, type = "terms", n_draws = 40, seed = 13)
    expect_true(all(is.finite(got$.lower_ci)))
    expect_true(all(is.finite(got$.upper_ci)))
    expect_true(all(got$.upper_ci >= got$.lower_ci))
  }
})

test_that("SCAM term intervals transform joint draws before computing variances", {
  skip_if_not_installed("scam")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  m <- scam::scam(y ~ z + s(x, bs = "mpi", k = 6), data = d)
  got <- variance_comp(m, type = "terms", n_draws = 50, seed = 13)
  betas <- gaussian_draws(m, n_draws = 50, seed = 13, mvn_method = "mgcv")
  X <- predict_model(m, type = "lpmatrix")
  indices <- variance_terms_columns(m)
  expected <- vapply(indices, function(i) {
    apply(X[, i, drop = FALSE] %*% t(betas[, i, drop = FALSE]), 2L, var)
  }, numeric(50))
  expected <- cbind(expected, apply(X %*% t(betas), 2L, var))
  expect_equal(attr(got, "variance_draws"), expected, ignore_attr = TRUE)
  expect_true(all(is.finite(got$.lower_ci)))
  expect_error(variance_comp(m, type = "terms", unconditional = TRUE),
    "Smoothing-parameter uncertainty")
})

test_that("term intervals include fixed, shared and parametric penalties", {
  withr::local_preserve_seed()
  d <- variance_terms_data()
  models <- list(
    gam(y ~ x + s(w, k = 6, sp = 0.7), data = d, method = "REML",
      paraPen = list(x = list(matrix(1)))),
    bam(y ~ x + s(w, by = fac, id = 1, k = 6), data = d, method = "REML")
  )
  for (m in models) {
    got <- variance_comp(m, type = "terms", n_draws = 30, seed = 13)
    expect_equal(attr(got, "variance_draws"),
      reference_term_variance_draws(m, 30, 13), ignore_attr = TRUE)
  }
})

test_that("lme term intervals retain residual weights and correlation", {
  skip_if_not_installed("nlme")
  withr::local_preserve_seed()
  d <- variance_terms_data()
  d$time <- rep(seq_len(20), 12)
  m <- gamm(y ~ x + z + fac + s(w, k = 6), random = list(group = ~1),
    weights = nlme::varIdent(form = ~1 | fac),
    correlation = nlme::corAR1(form = ~time | group), data = d, method = "REML")
  g <- model_used_rows(m$gam)
  expect_joint_random_draws(m, g, variance_terms_lme_design(m, g))
  got <- variance_comp(m, type = "terms", n_draws = 30, seed = 13)
  expect_true(all(is.finite(got$.lower_ci)))
  expect_true(all(is.finite(got$.upper_ci)))
})

test_that("zero random-effect variances give zero term intervals", {
  skip_if_not_installed("gamm4")
  withr::local_preserve_seed()
  d <- expand.grid(x = seq(0, 1, length.out = 20), group = factor(seq_len(10)))
  d$y <- 2 * d$x + sin(6 * d$x)
  # gamm4 can warn before falling back from a Cholesky factorisation when the
  # fitted variance is zero. The term-interval calculation must still succeed.
  m <- withCallingHandlers(
    gamm4::gamm4(y ~ s(x, k = 6), random = ~(1 | group), data = d),
    warning = function(w) {
      if (grepl("CHOLMOD warning 'not positive definite'", conditionMessage(w),
          fixed = TRUE)) invokeRestart("muffleWarning")
    })
  expect_true(lme4::isSingular(m$mer))
  got <- variance_comp(m, type = "terms", n_draws = 30, seed = 13)
  i <- match("group:(Intercept)", got$.component)
  expect_equal(got$.variance[i], 0)
  expect_equal(got$.lower_ci[i], 0)
  expect_equal(got$.upper_ci[i], 0)
})
